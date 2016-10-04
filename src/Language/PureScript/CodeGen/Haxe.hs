-- |
-- This module generates code in the simplified Javascript intermediate representation from Purescript code
--
module Language.PureScript.CodeGen.Haxe
  ( module AST
  , module Common
  , moduleToHaxe
  ) where

import Prelude.Compat

import Control.Arrow ((&&&))
import Control.Monad (replicateM, forM, void)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Supply.Class

import Data.List ((\\), delete, intersect, nub)
import Data.Maybe (isNothing, fromMaybe)
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Traversable as T

import Language.PureScript.AST.SourcePos
import Language.PureScript.CodeGen.Haxe.AST as AST
import Language.PureScript.CodeGen.Haxe.Common as Common
import Language.PureScript.CodeGen.Haxe.Optimizer
import Language.PureScript.CoreFn
import Language.PureScript.Crash
import Language.PureScript.Errors (ErrorMessageHint(..), SimpleErrorMessage(..),
                                   MultipleErrors(..), rethrow,
                                   errorMessage, rethrowWithPosition, addHint)
import Language.PureScript.Names
import Language.PureScript.Options
import Language.PureScript.Traversals (sndM)
import qualified Language.PureScript.Constants as C

import System.FilePath.Posix ((</>))

-- |
-- Generate code in the simplified Javascript intermediate representation for all declarations in a
-- module.
--
moduleToHaxe
  :: forall m
   . (Monad m, MonadReader Options m, MonadSupply m, MonadError MultipleErrors m)
  => Module Ann
  -> Maybe Haxe
  -> m [Haxe]
moduleToHaxe (Module coms mn imps exps foreigns decls) foreign_ =
  rethrow (addHint (ErrorInModule mn)) $ do
    let usedNames = concatMap getNames decls
    let mnLookup = renameImports usedNames imps
    jsImports <- T.traverse (importToJs mnLookup) . delete (ModuleName [ProperName C.prim]) . (\\ [mn]) $ nub $ map snd imps
    let decls' = renameModules mnLookup decls
    jsDecls <- mapM bindToJs decls'
    optimized <- T.traverse (T.traverse optimize) jsDecls
    F.traverse_ (F.traverse_ checkIntegers) optimized
    comments <- not <$> asks optionsNoComments
    let strict = HaxeStringLiteral Nothing "use strict"
    let header = if comments && not (null coms) then HaxeComment Nothing coms strict else strict
    let foreign' = [HaxeVariableIntroduction Nothing "$foreign" foreign_ | not $ null foreigns || isNothing foreign_]
    let moduleBody = header : foreign' ++ jsImports ++ concat optimized
    let foreignExps = exps `intersect` (fst `map` foreigns)
    let standardExps = exps \\ foreignExps
    let exps' = HaxeObjectLiteral Nothing $ map (runIdent &&& HaxeVar Nothing . identToJs) standardExps
                               ++ map (runIdent &&& foreignIdent) foreignExps
    return $ moduleBody ++ [HaxeAssignment Nothing (HaxeAccessor Nothing "exports" (HaxeVar Nothing "module")) exps']

  where

  -- |
  -- Extracts all declaration names from a binding group.
  --
  getNames :: Bind Ann -> [Ident]
  getNames (NonRec _ ident _) = [ident]
  getNames (Rec vals) = map (snd . fst) vals

  -- |
  -- Creates alternative names for each module to ensure they don't collide
  -- with declaration names.
  --
  renameImports :: [Ident] -> [(Ann, ModuleName)] -> M.Map ModuleName (Ann, ModuleName)
  renameImports = go M.empty
    where
    go :: M.Map ModuleName (Ann, ModuleName) -> [Ident] -> [(Ann, ModuleName)] -> M.Map ModuleName (Ann, ModuleName)
    go acc used ((ann, mn') : mns') =
      let mni = Ident $ runModuleName mn'
      in if mn' /= mn && mni `elem` used
         then let newName = freshModuleName 1 mn' used
              in go (M.insert mn' (ann, newName) acc) (Ident (runModuleName newName) : used) mns'
         else go (M.insert mn' (ann, mn') acc) used mns'
    go acc _ [] = acc

    freshModuleName :: Integer -> ModuleName -> [Ident] -> ModuleName
    freshModuleName i mn'@(ModuleName pns) used =
      let newName = ModuleName $ init pns ++ [ProperName $ runProperName (last pns) ++ "_" ++ show i]
      in if Ident (runModuleName newName) `elem` used
         then freshModuleName (i + 1) mn' used
         else newName

  -- |
  -- Generates Javascript code for a module import, binding the required module
  -- to the alternative
  --
  importToJs :: M.Map ModuleName (Ann, ModuleName) -> ModuleName -> m Haxe
  importToJs mnLookup mn' = do
    let ((ss, _, _, _), mnSafe) = fromMaybe (internalError "Missing value in mnLookup") $ M.lookup mn' mnLookup
    let moduleBody = HaxeApp Nothing (HaxeVar Nothing "require") [HaxeStringLiteral Nothing (".." </> runModuleName mn')]
    withPos ss $ HaxeVariableIntroduction Nothing (moduleNameToJs mnSafe) (Just moduleBody)

  -- |
  -- Replaces the `ModuleName`s in the AST so that the generated code refers to
  -- the collision-avoiding renamed module imports.
  --
  renameModules :: M.Map ModuleName (Ann, ModuleName) -> [Bind Ann] -> [Bind Ann]
  renameModules mnLookup binds =
    let (f, _, _) = everywhereOnValues id goExpr goBinder
    in map f binds
    where
    goExpr :: Expr a -> Expr a
    goExpr (Var ann q) = Var ann (renameQual q)
    goExpr e = e
    goBinder :: Binder a -> Binder a
    goBinder (ConstructorBinder ann q1 q2 bs) = ConstructorBinder ann (renameQual q1) (renameQual q2) bs
    goBinder b = b
    renameQual :: Qualified a -> Qualified a
    renameQual (Qualified (Just mn') a) =
      let (_,mnSafe) = fromMaybe (internalError "Missing value in mnLookup") $ M.lookup mn' mnLookup
      in Qualified (Just mnSafe) a
    renameQual q = q

  -- |
  -- Generate code in the simplified Javascript intermediate representation for a declaration
  --
  bindToJs :: Bind Ann -> m [Haxe]
  bindToJs (NonRec ann ident val) = return <$> nonRecToHaxe ann ident val
  bindToJs (Rec vals) = forM vals (uncurry . uncurry $ nonRecToHaxe)

  -- |
  -- Generate code in the simplified Javascript intermediate representation for a single non-recursive
  -- declaration.
  --
  -- The main purpose of this function is to handle code generation for comments.
  --
  nonRecToHaxe :: Ann -> Ident -> Expr Ann -> m Haxe
  nonRecToHaxe a i e@(extractAnn -> (_, com, _, _)) | not (null com) = do
    withoutComment <- asks optionsNoComments
    if withoutComment
       then nonRecToHaxe a i (modifyAnn removeComments e)
       else HaxeComment Nothing com <$> nonRecToHaxe a i (modifyAnn removeComments e)
  nonRecToHaxe (ss, _, _, _) ident val = do
    js <- valueToJs val
    withPos ss $ HaxeVariableIntroduction Nothing (identToJs ident) (Just js)

  withPos :: Maybe SourceSpan -> Haxe -> m Haxe
  withPos (Just ss) js = do
    withSM <- asks optionsSourceMaps
    return $ if withSM
      then withSourceSpan ss js
      else js
  withPos Nothing js = return js

  -- |
  -- Generate code in the simplified Javascript intermediate representation for a variable based on a
  -- PureScript identifier.
  --
  var :: Ident -> Haxe
  var = HaxeVar Nothing . identToJs

  -- |
  -- Generate code in the simplified Javascript intermediate representation for an accessor based on
  -- a PureScript identifier. If the name is not valid in Javascript (symbol based, reserved name) an
  -- indexer is returned.
  --
  accessor :: Ident -> Haxe -> Haxe
  accessor (Ident prop) = accessorString prop
  accessor (GenIdent _ _) = internalError "GenIdent in accessor"

  accessorString :: String -> Haxe -> Haxe
  accessorString prop | identNeedsEscaping prop = HaxeIndexer Nothing (HaxeStringLiteral Nothing prop)
                      | otherwise = HaxeAccessor Nothing prop

  -- |
  -- Generate code in the simplified Javascript intermediate representation for a value or expression.
  --
  valueToJs :: Expr Ann -> m Haxe
  valueToJs e =
    let (ss, _, _, _) = extractAnn e in
    withPos ss =<< valueToJs' e

  valueToJs' :: Expr Ann -> m Haxe
  valueToJs' (Literal (pos, _, _, _) l) =
    maybe id rethrowWithPosition pos $ literalToValueHaxe l
  valueToJs' (Var (_, _, _, Just (IsConstructor _ [])) name) =
    return $ HaxeAccessor Nothing "value" $ qualifiedToHaxe id name
  valueToJs' (Var (_, _, _, Just (IsConstructor _ _)) name) =
    return $ HaxeAccessor Nothing "create" $ qualifiedToHaxe id name
  valueToJs' (Accessor _ prop val) =
    accessorString prop <$> valueToJs val
  valueToJs' (ObjectUpdate _ o ps) = do
    obj <- valueToJs o
    sts <- mapM (sndM valueToJs) ps
    extendObj obj sts
  valueToJs' e@(Abs (_, _, _, Just IsTypeClassConstructor) _ _) =
    let args = unAbs e
    in return $ HaxeFunction Nothing Nothing (map identToJs args) (HaxeBlock Nothing $ map assign args)
    where
    unAbs :: Expr Ann -> [Ident]
    unAbs (Abs _ arg val) = arg : unAbs val
    unAbs _ = []
    assign :: Ident -> Haxe
    assign name = HaxeAssignment Nothing (accessorString (runIdent name) (HaxeVar Nothing "this"))
                               (var name)
  valueToJs' (Abs _ arg val) = do
    ret <- valueToJs val
    return $ HaxeFunction Nothing Nothing [identToJs arg] (HaxeBlock Nothing [HaxeReturn Nothing ret])
  valueToJs' e@App{} = do
    let (f, args) = unApp e []
    args' <- mapM valueToJs args
    case f of
      Var (_, _, _, Just IsNewtype) _ -> return (head args')
      Var (_, _, _, Just (IsConstructor _ fields)) name | length args == length fields ->
        return $ HaxeUnary Nothing HaxeNew $ HaxeApp Nothing (qualifiedToHaxe id name) args'
      Var (_, _, _, Just IsTypeClassConstructor) name ->
        return $ HaxeUnary Nothing HaxeNew $ HaxeApp Nothing (qualifiedToHaxe id name) args'
      _ -> flip (foldl (\fn a -> HaxeApp Nothing fn [a])) args' <$> valueToJs f
    where
    unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
    unApp (App _ val arg) args = unApp val (arg : args)
    unApp other args = (other, args)
  valueToJs' (Var (_, _, _, Just IsForeign) qi@(Qualified (Just mn') ident)) =
    return $ if mn' == mn
             then foreignIdent ident
             else varToJs qi
  valueToJs' (Var (_, _, _, Just IsForeign) ident) =
    error $ "Encountered an unqualified reference to a foreign ident " ++ showQualified showIdent ident
  valueToJs' (Var _ ident) = return $ varToJs ident
  valueToJs' (Case (maybeSpan, _, _, _) values binders) = do
    vals <- mapM valueToJs values
    bindersToJs maybeSpan binders vals
  valueToJs' (Let _ ds val) = do
    ds' <- concat <$> mapM bindToJs ds
    ret <- valueToJs val
    return $ HaxeApp Nothing (HaxeFunction Nothing Nothing [] (HaxeBlock Nothing (ds' ++ [HaxeReturn Nothing ret]))) []
  valueToJs' (Constructor (_, _, _, Just IsNewtype) _ (ProperName ctor) _) =
    return $ HaxeVariableIntroduction Nothing ctor (Just $
                HaxeObjectLiteral Nothing [("create",
                  HaxeFunction Nothing Nothing ["value"]
                    (HaxeBlock Nothing [HaxeReturn Nothing $ HaxeVar Nothing "value"]))])
  valueToJs' (Constructor _ _ (ProperName ctor) []) =
    return $ iife ctor [ HaxeFunction Nothing (Just ctor) [] (HaxeBlock Nothing [])
           , HaxeAssignment Nothing (HaxeAccessor Nothing "value" (HaxeVar Nothing ctor))
                (HaxeUnary Nothing HaxeNew $ HaxeApp Nothing (HaxeVar Nothing ctor) []) ]
  valueToJs' (Constructor _ _ (ProperName ctor) fields) =
    let constructor =
          let body = [ HaxeAssignment Nothing (HaxeAccessor Nothing (identToJs f) (HaxeVar Nothing "this")) (var f) | f <- fields ]
          in HaxeFunction Nothing (Just ctor) (identToJs `map` fields) (HaxeBlock Nothing body)
        createFn =
          let body = HaxeUnary Nothing HaxeNew $ HaxeApp Nothing (HaxeVar Nothing ctor) (var `map` fields)
          in foldr (\f inner -> HaxeFunction Nothing Nothing [identToJs f] (HaxeBlock Nothing [HaxeReturn Nothing inner])) body fields
    in return $ iife ctor [ constructor
                          , HaxeAssignment Nothing (HaxeAccessor Nothing "create" (HaxeVar Nothing ctor)) createFn
                          ]

  iife :: String -> [Haxe] -> Haxe
  iife v exprs = HaxeApp Nothing (HaxeFunction Nothing Nothing [] (HaxeBlock Nothing $ exprs ++ [HaxeReturn Nothing $ HaxeVar Nothing v])) []

  literalToValueHaxe :: Literal (Expr Ann) -> m Haxe
  literalToValueHaxe (NumericLiteral (Left i)) = return $ HaxeNumericLiteral Nothing (Left i)
  literalToValueHaxe (NumericLiteral (Right n)) = return $ HaxeNumericLiteral Nothing (Right n)
  literalToValueHaxe (StringLiteral s) = return $ HaxeStringLiteral Nothing s
  literalToValueHaxe (CharLiteral c) = return $ HaxeStringLiteral Nothing [c]
  literalToValueHaxe (BooleanLiteral b) = return $ HaxeBooleanLiteral Nothing b
  literalToValueHaxe (ArrayLiteral xs) = HaxeArrayLiteral Nothing <$> mapM valueToJs xs
  literalToValueHaxe (ObjectLiteral ps) = HaxeObjectLiteral Nothing <$> mapM (sndM valueToJs) ps

  -- |
  -- Shallow copy an object.
  --
  extendObj :: Haxe -> [(String, Haxe)] -> m Haxe
  extendObj obj sts = do
    newObj <- freshName
    key <- freshName
    evaluatedObj <- freshName
    let
      jsKey = HaxeVar Nothing key
      jsNewObj = HaxeVar Nothing newObj
      jsEvaluatedObj = HaxeVar Nothing evaluatedObj
      block = HaxeBlock Nothing (evaluate:objAssign:copy:extend ++ [HaxeReturn Nothing jsNewObj])
      evaluate = HaxeVariableIntroduction Nothing evaluatedObj (Just obj)
      objAssign = HaxeVariableIntroduction Nothing newObj (Just $ HaxeObjectLiteral Nothing [])
      copy = HaxeForIn Nothing key jsEvaluatedObj $ HaxeBlock Nothing [HaxeIfElse Nothing cond assign Nothing]
      cond = HaxeApp Nothing (HaxeAccessor Nothing "hasOwnProperty" jsEvaluatedObj) [jsKey]
      assign = HaxeBlock Nothing [HaxeAssignment Nothing (HaxeIndexer Nothing jsKey jsNewObj) (HaxeIndexer Nothing jsKey jsEvaluatedObj)]
      stToAssign (s, js) = HaxeAssignment Nothing (accessorString s jsNewObj) js
      extend = map stToAssign sts
    return $ HaxeApp Nothing (HaxeFunction Nothing Nothing [] block) []

  -- |
  -- Generate code in the simplified Javascript intermediate representation for a reference to a
  -- variable.
  --
  varToJs :: Qualified Ident -> Haxe
  varToJs (Qualified Nothing ident) = var ident
  varToJs qual = qualifiedToHaxe id qual

  -- |
  -- Generate code in the simplified Javascript intermediate representation for a reference to a
  -- variable that may have a qualified name.
  --
  qualifiedToHaxe :: (a -> Ident) -> Qualified a -> Haxe
  qualifiedToHaxe f (Qualified (Just (ModuleName [ProperName mn'])) a) | mn' == C.prim = HaxeVar Nothing . runIdent $ f a
  qualifiedToHaxe f (Qualified (Just mn') a) | mn /= mn' = accessor (f a) (HaxeVar Nothing (moduleNameToJs mn'))
  qualifiedToHaxe f (Qualified _ a) = HaxeVar Nothing $ identToJs (f a)

  foreignIdent :: Ident -> Haxe
  foreignIdent ident = accessorString (runIdent ident) (HaxeVar Nothing "$foreign")

  -- |
  -- Generate code in the simplified Javascript intermediate representation for pattern match binders
  -- and guards.
  --
  bindersToJs :: Maybe SourceSpan -> [CaseAlternative Ann] -> [Haxe] -> m Haxe
  bindersToJs maybeSpan binders vals = do
    valNames <- replicateM (length vals) freshName
    let assignments = zipWith (HaxeVariableIntroduction Nothing) valNames (map Just vals)
    jss <- forM binders $ \(CaseAlternative bs result) -> do
      ret <- guardsToJs result
      go valNames ret bs
    return $ HaxeApp Nothing (HaxeFunction Nothing Nothing [] (HaxeBlock Nothing (assignments ++ concat jss ++ [HaxeThrow Nothing $ failedPatternError valNames])))
                   []
    where
      go :: [String] -> [Haxe] -> [Binder Ann] -> m [Haxe]
      go _ done [] = return done
      go (v:vs) done' (b:bs) = do
        done'' <- go vs done' bs
        binderToJs v done'' b
      go _ _ _ = internalError "Invalid arguments to bindersToJs"

      failedPatternError :: [String] -> Haxe
      failedPatternError names = HaxeUnary Nothing HaxeNew $ HaxeApp Nothing (HaxeVar Nothing "Error") [HaxeBinary Nothing Add (HaxeStringLiteral Nothing failedPatternMessage) (HaxeArrayLiteral Nothing $ zipWith valueError names vals)]

      failedPatternMessage :: String
      failedPatternMessage = "Failed pattern match" ++ maybe "" (((" at " ++ runModuleName mn ++ " ") ++) . displayStartEndPos) maybeSpan ++ ": "

      valueError :: String -> Haxe -> Haxe
      valueError _ l@(HaxeNumericLiteral _ _) = l
      valueError _ l@(HaxeStringLiteral _ _)  = l
      valueError _ l@(HaxeBooleanLiteral _ _) = l
      valueError s _                        = HaxeAccessor Nothing "name" . HaxeAccessor Nothing "constructor" $ HaxeVar Nothing s

      guardsToJs :: Either [(Guard Ann, Expr Ann)] (Expr Ann) -> m [Haxe]
      guardsToJs (Left gs) = forM gs $ \(cond, val) -> do
        cond' <- valueToJs cond
        done  <- valueToJs val
        return $ HaxeIfElse Nothing cond' (HaxeBlock Nothing [HaxeReturn Nothing done]) Nothing
      guardsToJs (Right v) = return . HaxeReturn Nothing <$> valueToJs v

  binderToJs :: String -> [Haxe] -> Binder Ann -> m [Haxe]
  binderToJs s done binder =
    let (ss, _, _, _) = extractBinderAnn binder in
    traverse (withPos ss) =<< binderToJs' s done binder

  -- |
  -- Generate code in the simplified Javascript intermediate representation for a pattern match
  -- binder.
  --
  binderToJs' :: String -> [Haxe] -> Binder Ann -> m [Haxe]
  binderToJs' _ done NullBinder{} = return done
  binderToJs' varName done (LiteralBinder _ l) =
    literalToBinderHaxe varName done l
  binderToJs' varName done (VarBinder _ ident) =
    return (HaxeVariableIntroduction Nothing (identToJs ident) (Just (HaxeVar Nothing varName)) : done)
  binderToJs' varName done (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [b]) =
    binderToJs varName done b
  binderToJs' varName done (ConstructorBinder (_, _, _, Just (IsConstructor ctorType fields)) _ ctor bs) = do
    js <- go (zip fields bs) done
    return $ case ctorType of
      ProductType -> js
      SumType ->
        [HaxeIfElse Nothing (HaxeInstanceOf Nothing (HaxeVar Nothing varName) (qualifiedToHaxe (Ident . runProperName) ctor))
                  (HaxeBlock Nothing js)
                  Nothing]
    where
    go :: [(Ident, Binder Ann)] -> [Haxe] -> m [Haxe]
    go [] done' = return done'
    go ((field, binder) : remain) done' = do
      argVar <- freshName
      done'' <- go remain done'
      js <- binderToJs argVar done'' binder
      return (HaxeVariableIntroduction Nothing argVar (Just (HaxeAccessor Nothing (identToJs field) (HaxeVar Nothing varName))) : js)
  binderToJs' _ _ ConstructorBinder{} =
    internalError "binderToJs: Invalid ConstructorBinder in binderToJs"
  binderToJs' varName done (NamedBinder _ ident binder) = do
    js <- binderToJs varName done binder
    return (HaxeVariableIntroduction Nothing (identToJs ident) (Just (HaxeVar Nothing varName)) : js)

  literalToBinderHaxe :: String -> [Haxe] -> Literal (Binder Ann) -> m [Haxe]
  literalToBinderHaxe varName done (NumericLiteral num) =
    return [HaxeIfElse Nothing (HaxeBinary Nothing EqualTo (HaxeVar Nothing varName) (HaxeNumericLiteral Nothing num)) (HaxeBlock Nothing done) Nothing]
  literalToBinderHaxe varName done (CharLiteral c) =
    return [HaxeIfElse Nothing (HaxeBinary Nothing EqualTo (HaxeVar Nothing varName) (HaxeStringLiteral Nothing [c])) (HaxeBlock Nothing done) Nothing]
  literalToBinderHaxe varName done (StringLiteral str) =
    return [HaxeIfElse Nothing (HaxeBinary Nothing EqualTo (HaxeVar Nothing varName) (HaxeStringLiteral Nothing str)) (HaxeBlock Nothing done) Nothing]
  literalToBinderHaxe varName done (BooleanLiteral True) =
    return [HaxeIfElse Nothing (HaxeVar Nothing varName) (HaxeBlock Nothing done) Nothing]
  literalToBinderHaxe varName done (BooleanLiteral False) =
    return [HaxeIfElse Nothing (HaxeUnary Nothing Not (HaxeVar Nothing varName)) (HaxeBlock Nothing done) Nothing]
  literalToBinderHaxe varName done (ObjectLiteral bs) = go done bs
    where
    go :: [Haxe] -> [(String, Binder Ann)] -> m [Haxe]
    go done' [] = return done'
    go done' ((prop, binder):bs') = do
      propVar <- freshName
      done'' <- go done' bs'
      js <- binderToJs propVar done'' binder
      return (HaxeVariableIntroduction Nothing propVar (Just (accessorString prop (HaxeVar Nothing varName))) : js)
  literalToBinderHaxe varName done (ArrayLiteral bs) = do
    js <- go done 0 bs
    return [HaxeIfElse Nothing (HaxeBinary Nothing EqualTo (HaxeAccessor Nothing "length" (HaxeVar Nothing varName)) (HaxeNumericLiteral Nothing (Left (fromIntegral $ length bs)))) (HaxeBlock Nothing js) Nothing]
    where
    go :: [Haxe] -> Integer -> [Binder Ann] -> m [Haxe]
    go done' _ [] = return done'
    go done' index (binder:bs') = do
      elVar <- freshName
      done'' <- go done' (index + 1) bs'
      js <- binderToJs elVar done'' binder
      return (HaxeVariableIntroduction Nothing elVar (Just (HaxeIndexer Nothing (HaxeNumericLiteral Nothing (Left index)) (HaxeVar Nothing varName))) : js)

  -- Check that all integers fall within the valid int range for JavaScript.
  checkIntegers :: Haxe -> m ()
  checkIntegers = void . everywhereOnHaxeTopDownM go
    where
    go :: Haxe -> m Haxe
    go (HaxeUnary _ Negate (HaxeNumericLiteral ss (Left i))) =
      -- Move the negation inside the literal; since this is a top-down
      -- traversal doing this replacement will stop the next case from raising
      -- the error when attempting to use -2147483648, as if left unrewritten
      -- the value is `HaxeUnary Negate (HaxeNumericLiteral (Left 2147483648))`, and
      -- 2147483648 is larger than the maximum allowed int.
      return $ HaxeNumericLiteral ss (Left (-i))
    go js@(HaxeNumericLiteral _ (Left i)) =
      let minInt = -2147483648
          maxInt = 2147483647
      in if i < minInt || i > maxInt
         then throwError . errorMessage $ IntOutOfRange i "JavaScript" minInt maxInt
         else return js
    go other = return other

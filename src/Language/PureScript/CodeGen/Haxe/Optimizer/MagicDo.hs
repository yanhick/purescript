-- |
-- This module implements the "Magic Do" optimization, which inlines calls to return
-- and bind for the Eff monad, as well as some of its actions.
--
module Language.PureScript.CodeGen.Haxe.Optimizer.MagicDo (magicDo) where

import Prelude.Compat

import Data.List (nub)
import Data.Maybe (fromJust, isJust)

import Language.PureScript.CodeGen.Haxe.AST
import Language.PureScript.CodeGen.Haxe.Optimizer.Common
import Language.PureScript.Options
import qualified Language.PureScript.Constants as C

magicDo :: Options -> Haxe -> Haxe
magicDo opts | optionsNoMagicDo opts = id
             | otherwise = inlineST . magicDo'

-- |
-- Inline type class dictionaries for >>= and return for the Eff monad
--
-- E.g.
--
--  Prelude[">>="](dict)(m1)(function(x) {
--    return ...;
--  })
--
-- becomes
--
--  function __do {
--    var x = m1();
--    ...
--  }
--
magicDo' :: Haxe -> Haxe
magicDo' = everywhereOnHaxe undo . everywhereOnHaxeTopDown convert
  where
  -- The name of the function block which is added to denote a do block
  fnName = "__do"
  -- Desugar monomorphic calls to >>= and return for the Eff monad
  convert :: Haxe -> Haxe
  -- Desugar pure & return
  convert (HaxeApp _ (HaxeApp _ pure' [val]) []) | isPure pure' = val
  -- Desugar >>
  convert (HaxeApp _ (HaxeApp _ bind [m]) [HaxeFunction s1 Nothing [] (HaxeBlock s2 js)]) | isBind bind =
    HaxeFunction s1 (Just fnName) [] $ HaxeBlock s2 (HaxeApp s2 m [] : map applyReturns js )
  -- Desugar >>=
  convert (HaxeApp _ (HaxeApp _ bind [m]) [HaxeFunction s1 Nothing [arg] (HaxeBlock s2 js)]) | isBind bind =
    HaxeFunction s1 (Just fnName) [] $ HaxeBlock s2 (HaxeVariableIntroduction s2 arg (Just (HaxeApp s2 m [])) : map applyReturns js)
  -- Desugar untilE
  convert (HaxeApp s1 (HaxeApp _ f [arg]) []) | isEffFunc C.untilE f =
    HaxeApp s1 (HaxeFunction s1 Nothing [] (HaxeBlock s1 [ HaxeWhile s1 (HaxeUnary s1 Not (HaxeApp s1 arg [])) (HaxeBlock s1 []), HaxeReturn s1 $ HaxeObjectLiteral s1 []])) []
  -- Desugar whileE
  convert (HaxeApp _ (HaxeApp _ (HaxeApp s1 f [arg1]) [arg2]) []) | isEffFunc C.whileE f =
    HaxeApp s1 (HaxeFunction s1 Nothing [] (HaxeBlock s1 [ HaxeWhile s1 (HaxeApp s1 arg1 []) (HaxeBlock s1 [ HaxeApp s1 arg2 [] ]), HaxeReturn s1 $ HaxeObjectLiteral s1 []])) []
  convert other = other
  -- Check if an expression represents a monomorphic call to >>= for the Eff monad
  isBind (HaxeApp _ fn [dict]) | isDict (C.eff, C.bindEffDictionary) dict && isBindPoly fn = True
  isBind _ = False
  -- Check if an expression represents a monomorphic call to pure or return for the Eff applicative
  isPure (HaxeApp _ fn [dict]) | isDict (C.eff, C.applicativeEffDictionary) dict && isPurePoly fn = True
  isPure _ = False
  -- Check if an expression represents the polymorphic >>= function
  isBindPoly = isFn (C.controlBind, C.bind)
  -- Check if an expression represents the polymorphic pure or return function
  isPurePoly = isFn (C.controlApplicative, C.pure')
  -- Check if an expression represents a function in the Eff module
  isEffFunc name (HaxeAccessor _ name' (HaxeVar _ eff)) = eff == C.eff && name == name'
  isEffFunc _ _ = False

  -- Remove __do function applications which remain after desugaring
  undo :: Haxe -> Haxe
  undo (HaxeReturn _ (HaxeApp _ (HaxeFunction _ (Just ident) [] body) [])) | ident == fnName = body
  undo other = other

  applyReturns :: Haxe -> Haxe
  applyReturns (HaxeReturn ss ret) = HaxeReturn ss (HaxeApp ss ret [])
  applyReturns (HaxeBlock ss jss) = HaxeBlock ss (map applyReturns jss)
  applyReturns (HaxeWhile ss cond js) = HaxeWhile ss cond (applyReturns js)
  applyReturns (HaxeFor ss v lo hi js) = HaxeFor ss v lo hi (applyReturns js)
  applyReturns (HaxeForIn ss v xs js) = HaxeForIn ss v xs (applyReturns js)
  applyReturns (HaxeIfElse ss cond t f) = HaxeIfElse ss cond (applyReturns t) (applyReturns `fmap` f)
  applyReturns other = other

-- |
-- Inline functions in the ST module
--
inlineST :: Haxe -> Haxe
inlineST = everywhereOnHaxe convertBlock
  where
  -- Look for runST blocks and inline the STRefs there.
  -- If all STRefs are used in the scope of the same runST, only using { read, write, modify }STRef then
  -- we can be more aggressive about inlining, and actually turn STRefs into local variables.
  convertBlock (HaxeApp _ f [arg]) | isSTFunc C.runST f =
    let refs = nub . findSTRefsIn $ arg
        usages = findAllSTUsagesIn arg
        allUsagesAreLocalVars = all (\u -> let v = toVar u in isJust v && fromJust v `elem` refs) usages
        localVarsDoNotEscape = all (\r -> length (r `appearingIn` arg) == length (filter (\u -> let v = toVar u in v == Just r) usages)) refs
    in everywhereOnHaxe (convert (allUsagesAreLocalVars && localVarsDoNotEscape)) arg
  convertBlock other = other
  -- Convert a block in a safe way, preserving object wrappers of references,
  -- or in a more aggressive way, turning wrappers into local variables depending on the
  -- agg(ressive) parameter.
  convert agg (HaxeApp s1 f [arg]) | isSTFunc C.newSTRef f =
   HaxeFunction s1 Nothing [] (HaxeBlock s1 [HaxeReturn s1 $ if agg then arg else HaxeObjectLiteral s1 [(C.stRefValue, arg)]])
  convert agg (HaxeApp _ (HaxeApp s1 f [ref]) []) | isSTFunc C.readSTRef f =
    if agg then ref else HaxeAccessor s1 C.stRefValue ref
  convert agg (HaxeApp _ (HaxeApp _ (HaxeApp s1 f [ref]) [arg]) []) | isSTFunc C.writeSTRef f =
    if agg then HaxeAssignment s1 ref arg else HaxeAssignment s1 (HaxeAccessor s1 C.stRefValue ref) arg
  convert agg (HaxeApp _ (HaxeApp _ (HaxeApp s1 f [ref]) [func]) []) | isSTFunc C.modifySTRef f =
    if agg then HaxeAssignment s1 ref (HaxeApp s1 func [ref]) else HaxeAssignment s1 (HaxeAccessor s1 C.stRefValue ref) (HaxeApp s1 func [HaxeAccessor s1 C.stRefValue ref])
  convert _ other = other
  -- Check if an expression represents a function in the ST module
  isSTFunc name (HaxeAccessor _ name' (HaxeVar _ st)) = st == C.st && name == name'
  isSTFunc _ _ = False
  -- Find all ST Refs initialized in this block
  findSTRefsIn = everythingOnHaxe (++) isSTRef
    where
    isSTRef (HaxeVariableIntroduction _ ident (Just (HaxeApp _ (HaxeApp _ f [_]) []))) | isSTFunc C.newSTRef f = [ident]
    isSTRef _ = []
  -- Find all STRefs used as arguments to readSTRef, writeSTRef, modifySTRef
  findAllSTUsagesIn = everythingOnHaxe (++) isSTUsage
    where
    isSTUsage (HaxeApp _ (HaxeApp _ f [ref]) []) | isSTFunc C.readSTRef f = [ref]
    isSTUsage (HaxeApp _ (HaxeApp _ (HaxeApp _ f [ref]) [_]) []) | isSTFunc C.writeSTRef f || isSTFunc C.modifySTRef f = [ref]
    isSTUsage _ = []
  -- Find all uses of a variable
  appearingIn ref = everythingOnHaxe (++) isVar
    where
    isVar e@(HaxeVar _ v) | v == ref = [e]
    isVar _ = []
  -- Convert a Haxe value to a String if it is a HaxeVar
  toVar (HaxeVar _ v) = Just v
  toVar _ = Nothing

-- |
-- This module provides basic inlining capabilities
--
module Language.PureScript.CodeGen.Haxe.Optimizer.Inliner
  ( inlineVariables
  , inlineCommonValues
  , inlineCommonOperators
  , inlineFnComposition
  , etaConvert
  , unThunk
  , evaluateIifes
  ) where

import Prelude.Compat

import Control.Monad.Supply.Class (MonadSupply, freshName)

import Data.Maybe (fromMaybe)

import Language.PureScript.CodeGen.Haxe.AST
import Language.PureScript.CodeGen.Haxe.Optimizer.Common
import qualified Language.PureScript.Constants as C

-- TODO: Potential bug:
-- Shouldn't just inline this case: { var x = 0; x.toFixed(10); }
-- Needs to be: { 0..toFixed(10); }
-- Probably needs to be fixed in pretty-printer instead.
shouldInline :: Haxe -> Bool
shouldInline (HaxeVar _ _) = True
shouldInline (HaxeNumericLiteral _ _) = True
shouldInline (HaxeStringLiteral _ _) = True
shouldInline (HaxeBooleanLiteral _ _) = True
shouldInline (HaxeAccessor _ _ val) = shouldInline val
shouldInline (HaxeIndexer _ index val) = shouldInline index && shouldInline val
shouldInline _ = False

etaConvert :: Haxe -> Haxe
etaConvert = everywhereOnHaxe convert
  where
  convert :: Haxe -> Haxe
  convert (HaxeBlock ss [HaxeReturn _ (HaxeApp _ (HaxeFunction _ Nothing idents block@(HaxeBlock _ body)) args)])
    | all shouldInline args &&
      not (any (`isRebound` block) (map (HaxeVar Nothing) idents)) &&
      not (any (`isRebound` block) args)
      = HaxeBlock ss (map (replaceIdents (zip idents args)) body)
  convert (HaxeFunction _ Nothing [] (HaxeBlock _ [HaxeReturn _ (HaxeApp _ fn [])])) = fn
  convert js = js

unThunk :: Haxe -> Haxe
unThunk = everywhereOnHaxe convert
  where
  convert :: Haxe -> Haxe
  convert (HaxeBlock ss []) = HaxeBlock ss []
  convert (HaxeBlock ss jss) =
    case last jss of
      HaxeReturn _ (HaxeApp _ (HaxeFunction _ Nothing [] (HaxeBlock _ body)) []) -> HaxeBlock ss $ init jss ++ body
      _ -> HaxeBlock ss jss
  convert js = js

evaluateIifes :: Haxe -> Haxe
evaluateIifes = everywhereOnHaxe convert
  where
  convert :: Haxe -> Haxe
  convert (HaxeApp _ (HaxeFunction _ Nothing [] (HaxeBlock _ [HaxeReturn _ ret])) []) = ret
  convert js = js

inlineVariables :: Haxe -> Haxe
inlineVariables = everywhereOnHaxe $ removeFromBlock go
  where
  go :: [Haxe] -> [Haxe]
  go [] = []
  go (HaxeVariableIntroduction _ var (Just js) : sts)
    | shouldInline js && not (any (isReassigned var) sts) && not (any (isRebound js) sts) && not (any (isUpdated var) sts) =
      go (map (replaceIdent var js) sts)
  go (s:sts) = s : go sts

inlineCommonValues :: Haxe -> Haxe
inlineCommonValues = everywhereOnHaxe convert
  where
  convert :: Haxe -> Haxe
  convert (HaxeApp ss fn [dict])
    | isDict' [semiringNumber, semiringInt] dict && isFn fnZero fn = HaxeNumericLiteral ss (Left 0)
    | isDict' [semiringNumber, semiringInt] dict && isFn fnOne fn = HaxeNumericLiteral ss (Left 1)
    | isDict boundedBoolean dict && isFn fnBottom fn = HaxeBooleanLiteral ss False
    | isDict boundedBoolean dict && isFn fnTop fn = HaxeBooleanLiteral ss True
  convert (HaxeApp ss (HaxeApp _ (HaxeApp _ fn [dict]) [x]) [y])
    | isDict semiringInt dict && isFn fnAdd fn = intOp ss Add x y
    | isDict semiringInt dict && isFn fnMultiply fn = intOp ss Multiply x y
    | isDict euclideanRingInt dict && isFn fnDivide fn = intOp ss Divide x y
    | isDict ringInt dict && isFn fnSubtract fn = intOp ss Subtract x y
  convert other = other
  fnZero = (C.dataSemiring, C.zero)
  fnOne = (C.dataSemiring, C.one)
  fnBottom = (C.dataBounded, C.bottom)
  fnTop = (C.dataBounded, C.top)
  fnAdd = (C.dataSemiring, C.add)
  fnDivide = (C.dataEuclideanRing, C.div)
  fnMultiply = (C.dataSemiring, C.mul)
  fnSubtract = (C.dataRing, C.sub)
  intOp ss op x y = HaxeBinary ss BitwiseOr (HaxeBinary ss op x y) (HaxeNumericLiteral ss (Left 0))

inlineNonClassFunction :: (String, String) -> (Haxe -> Haxe -> Haxe) -> Haxe -> Haxe
inlineNonClassFunction (m, op) f = everywhereOnHaxe convert
  where
  convert :: Haxe -> Haxe
  convert (HaxeApp _ (HaxeApp _ op' [x]) [y]) | isOp op' = f x y
  convert other = other
  isOp (HaxeAccessor _ op' (HaxeVar _ m')) = m == m' && op == op'
  isOp _ = False

inlineCommonOperators :: Haxe -> Haxe
inlineCommonOperators = applyAll $
  [ binary semiringNumber opAdd Add
  , binary semiringNumber opMul Multiply

  , binary ringNumber opSub Subtract
  , unary  ringNumber opNegate Negate
  , binary ringInt opSub Subtract
  , unary  ringInt opNegate Negate

  , binary euclideanRingNumber opDiv Divide
  , binary euclideanRingInt opMod Modulus

  , binary eqNumber opEq EqualTo
  , binary eqNumber opNotEq NotEqualTo
  , binary eqInt opEq EqualTo
  , binary eqInt opNotEq NotEqualTo
  , binary eqString opEq EqualTo
  , binary eqString opNotEq NotEqualTo
  , binary eqChar opEq EqualTo
  , binary eqChar opNotEq NotEqualTo
  , binary eqBoolean opEq EqualTo
  , binary eqBoolean opNotEq NotEqualTo

  , binary ordBoolean opLessThan LessThan
  , binary ordBoolean opLessThanOrEq LessThanOrEqualTo
  , binary ordBoolean opGreaterThan GreaterThan
  , binary ordBoolean opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordChar opLessThan LessThan
  , binary ordChar opLessThanOrEq LessThanOrEqualTo
  , binary ordChar opGreaterThan GreaterThan
  , binary ordChar opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordInt opLessThan LessThan
  , binary ordInt opLessThanOrEq LessThanOrEqualTo
  , binary ordInt opGreaterThan GreaterThan
  , binary ordInt opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordNumber opLessThan LessThan
  , binary ordNumber opLessThanOrEq LessThanOrEqualTo
  , binary ordNumber opGreaterThan GreaterThan
  , binary ordNumber opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordString opLessThan LessThan
  , binary ordString opLessThanOrEq LessThanOrEqualTo
  , binary ordString opGreaterThan GreaterThan
  , binary ordString opGreaterThanOrEq GreaterThanOrEqualTo

  , binary semigroupString opAppend Add

  , binary heytingAlgebraBoolean opConj And
  , binary heytingAlgebraBoolean opDisj Or
  , unary  heytingAlgebraBoolean opNot Not

  , binary' C.dataIntBits (C..|.) BitwiseOr
  , binary' C.dataIntBits (C..&.) BitwiseAnd
  , binary' C.dataIntBits (C..^.) BitwiseXor
  , binary' C.dataIntBits C.shl ShiftLeft
  , binary' C.dataIntBits C.shr ShiftRight
  , binary' C.dataIntBits C.zshr ZeroFillShiftRight
  , unary'  C.dataIntBits C.complement BitwiseNot

  , inlineNonClassFunction (C.dataFunction, C.apply) $ \f x -> HaxeApp Nothing f [x]
  , inlineNonClassFunction (C.dataFunction, C.applyFlipped) $ \x f -> HaxeApp Nothing f [x]
  , inlineNonClassFunction (C.dataArrayUnsafe, C.unsafeIndex) $ flip (HaxeIndexer Nothing)
  ] ++
  [ fn | i <- [0..10], fn <- [ mkFn i, runFn i ] ]
  where
  binary :: (String, String) -> (String, String) -> BinaryOperator -> Haxe -> Haxe
  binary dict fns op = everywhereOnHaxe convert
    where
    convert :: Haxe -> Haxe
    convert (HaxeApp ss (HaxeApp _ (HaxeApp _ fn [dict']) [x]) [y]) | isDict dict dict' && isFn fns fn = HaxeBinary ss op x y
    convert other = other
  binary' :: String -> String -> BinaryOperator -> Haxe -> Haxe
  binary' moduleName opString op = everywhereOnHaxe convert
    where
    convert :: Haxe -> Haxe
    convert (HaxeApp ss (HaxeApp _ fn [x]) [y]) | isFn (moduleName, opString) fn = HaxeBinary ss op x y
    convert other = other
  unary :: (String, String) -> (String, String) -> UnaryOperator -> Haxe -> Haxe
  unary dicts fns op = everywhereOnHaxe convert
    where
    convert :: Haxe -> Haxe
    convert (HaxeApp ss (HaxeApp _ fn [dict']) [x]) | isDict dicts dict' && isFn fns fn = HaxeUnary ss op x
    convert other = other
  unary' :: String -> String -> UnaryOperator -> Haxe -> Haxe
  unary' moduleName fnName op = everywhereOnHaxe convert
    where
    convert :: Haxe -> Haxe
    convert (HaxeApp ss fn [x]) | isFn (moduleName, fnName) fn = HaxeUnary ss op x
    convert other = other
  mkFn :: Int -> Haxe -> Haxe
  mkFn 0 = everywhereOnHaxe convert
    where
    convert :: Haxe -> Haxe
    convert (HaxeApp _ mkFnN [HaxeFunction s1 Nothing [_] (HaxeBlock s2 js)]) | isNFn C.mkFn 0 mkFnN =
      HaxeFunction s1 Nothing [] (HaxeBlock s2 js)
    convert other = other
  mkFn n = everywhereOnHaxe convert
    where
    convert :: Haxe -> Haxe
    convert orig@(HaxeApp ss mkFnN [fn]) | isNFn C.mkFn n mkFnN =
      case collectArgs n [] fn of
        Just (args, js) -> HaxeFunction ss Nothing args (HaxeBlock ss js)
        Nothing -> orig
    convert other = other
    collectArgs :: Int -> [String] -> Haxe -> Maybe ([String], [Haxe])
    collectArgs 1 acc (HaxeFunction _ Nothing [oneArg] (HaxeBlock _ js)) | length acc == n - 1 = Just (reverse (oneArg : acc), js)
    collectArgs m acc (HaxeFunction _ Nothing [oneArg] (HaxeBlock _ [HaxeReturn _ ret])) = collectArgs (m - 1) (oneArg : acc) ret
    collectArgs _ _   _ = Nothing

  isNFn :: String -> Int -> Haxe -> Bool
  isNFn prefix n (HaxeVar _ name) = name == (prefix ++ show n)
  isNFn prefix n (HaxeAccessor _ name (HaxeVar _ dataFunctionUncurried)) | dataFunctionUncurried == C.dataFunctionUncurried = name == (prefix ++ show n)
  isNFn _ _ _ = False

  runFn :: Int -> Haxe -> Haxe
  runFn n = everywhereOnHaxe convert
    where
    convert :: Haxe -> Haxe
    convert js = fromMaybe js $ go n [] js

    go :: Int -> [Haxe] -> Haxe -> Maybe Haxe
    go 0 acc (HaxeApp ss runFnN [fn]) | isNFn C.runFn n runFnN && length acc == n = Just (HaxeApp ss fn acc)
    go m acc (HaxeApp _ lhs [arg]) = go (m - 1) (arg : acc) lhs
    go _ _   _ = Nothing

-- (f <<< g $ x) = f (g x)
-- (f <<< g)     = \x -> f (g x)
inlineFnComposition :: (MonadSupply m) => Haxe -> m Haxe
inlineFnComposition = everywhereOnHaxeTopDownM convert
  where
  convert :: (MonadSupply m) => Haxe -> m Haxe
  convert (HaxeApp s1 (HaxeApp s2 (HaxeApp _ (HaxeApp _ fn [dict']) [x]) [y]) [z])
    | isFnCompose dict' fn = return $ HaxeApp s1 x [HaxeApp s2 y [z]]
    | isFnComposeFlipped dict' fn = return $ HaxeApp s2 y [HaxeApp s1 x [z]]
  convert (HaxeApp ss (HaxeApp _ (HaxeApp _ fn [dict']) [x]) [y])
    | isFnCompose dict' fn = do
        arg <- freshName
        return $ HaxeFunction ss Nothing [arg] (HaxeBlock ss [HaxeReturn Nothing $ HaxeApp Nothing x [HaxeApp Nothing y [HaxeVar Nothing arg]]])
    | isFnComposeFlipped dict' fn = do
        arg <- freshName
        return $ HaxeFunction ss Nothing [arg] (HaxeBlock ss [HaxeReturn Nothing $ HaxeApp Nothing y [HaxeApp Nothing x [HaxeVar Nothing arg]]])
  convert other = return other
  isFnCompose :: Haxe -> Haxe -> Bool
  isFnCompose dict' fn = isDict semigroupoidFn dict' && isFn fnCompose fn
  isFnComposeFlipped :: Haxe -> Haxe -> Bool
  isFnComposeFlipped dict' fn = isDict semigroupoidFn dict' && isFn fnComposeFlipped fn
  fnCompose :: (String, String)
  fnCompose = (C.controlSemigroupoid, C.compose)
  fnComposeFlipped :: (String, String)
  fnComposeFlipped = (C.controlSemigroupoid, C.composeFlipped)

semiringNumber :: (String, String)
semiringNumber = (C.dataSemiring, C.semiringNumber)

semiringInt :: (String, String)
semiringInt = (C.dataSemiring, C.semiringInt)

ringNumber :: (String, String)
ringNumber = (C.dataRing, C.ringNumber)

ringInt :: (String, String)
ringInt = (C.dataRing, C.ringInt)

euclideanRingNumber :: (String, String)
euclideanRingNumber = (C.dataEuclideanRing, C.euclideanRingNumber)

euclideanRingInt :: (String, String)
euclideanRingInt = (C.dataEuclideanRing, C.euclideanRingInt)

eqNumber :: (String, String)
eqNumber = (C.dataEq, C.eqNumber)

eqInt :: (String, String)
eqInt = (C.dataEq, C.eqInt)

eqString :: (String, String)
eqString = (C.dataEq, C.eqString)

eqChar :: (String, String)
eqChar = (C.dataEq, C.eqChar)

eqBoolean :: (String, String)
eqBoolean = (C.dataEq, C.eqBoolean)

ordBoolean :: (String, String)
ordBoolean = (C.dataOrd, C.ordBoolean)

ordNumber :: (String, String)
ordNumber = (C.dataOrd, C.ordNumber)

ordInt :: (String, String)
ordInt = (C.dataOrd, C.ordInt)

ordString :: (String, String)
ordString = (C.dataOrd, C.ordString)

ordChar :: (String, String)
ordChar = (C.dataOrd, C.ordChar)

semigroupString :: (String, String)
semigroupString = (C.dataSemigroup, C.semigroupString)

boundedBoolean :: (String, String)
boundedBoolean = (C.dataBounded, C.boundedBoolean)

heytingAlgebraBoolean :: (String, String)
heytingAlgebraBoolean = (C.dataHeytingAlgebra, C.heytingAlgebraBoolean)

semigroupoidFn :: (String, String)
semigroupoidFn = (C.controlSemigroupoid, C.semigroupoidFn)

opAdd :: (String, String)
opAdd = (C.dataSemiring, C.add)

opMul :: (String, String)
opMul = (C.dataSemiring, C.mul)

opEq :: (String, String)
opEq = (C.dataEq, C.eq)

opNotEq :: (String, String)
opNotEq = (C.dataEq, C.notEq)

opLessThan :: (String, String)
opLessThan = (C.dataOrd, C.lessThan)

opLessThanOrEq :: (String, String)
opLessThanOrEq = (C.dataOrd, C.lessThanOrEq)

opGreaterThan :: (String, String)
opGreaterThan = (C.dataOrd, C.greaterThan)

opGreaterThanOrEq :: (String, String)
opGreaterThanOrEq = (C.dataOrd, C.greaterThanOrEq)

opAppend :: (String, String)
opAppend = (C.dataSemigroup, C.append)

opSub :: (String, String)
opSub = (C.dataRing, C.sub)

opNegate :: (String, String)
opNegate = (C.dataRing, C.negate)

opDiv :: (String, String)
opDiv = (C.dataEuclideanRing, C.div)

opMod :: (String, String)
opMod = (C.dataEuclideanRing, C.mod)

opConj :: (String, String)
opConj = (C.dataHeytingAlgebra, C.conj)

opDisj :: (String, String)
opDisj = (C.dataHeytingAlgebra, C.disj)

opNot :: (String, String)
opNot = (C.dataHeytingAlgebra, C.not)

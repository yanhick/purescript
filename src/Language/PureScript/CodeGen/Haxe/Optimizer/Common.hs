-- |
-- Common functions used by the various optimizer phases
--
module Language.PureScript.CodeGen.Haxe.Optimizer.Common where

import Prelude.Compat

import Data.Maybe (fromMaybe)

import Language.PureScript.Crash
import Language.PureScript.CodeGen.Haxe.AST

applyAll :: [a -> a] -> a -> a
applyAll = foldl1 (.)

replaceIdent :: String -> Haxe -> Haxe -> Haxe
replaceIdent var1 js = everywhereOnHaxe replace
  where
  replace (HaxeVar _ var2) | var1 == var2 = js
  replace other = other

replaceIdents :: [(String, Haxe)] -> Haxe -> Haxe
replaceIdents vars = everywhereOnHaxe replace
  where
  replace v@(HaxeVar _ var) = fromMaybe v $ lookup var vars
  replace other = other

isReassigned :: String -> Haxe -> Bool
isReassigned var1 = everythingOnHaxe (||) check
  where
  check :: Haxe -> Bool
  check (HaxeFunction _ _ args _) | var1 `elem` args = True
  check (HaxeVariableIntroduction _ arg _) | var1 == arg = True
  check (HaxeAssignment _ (HaxeVar _ arg) _) | var1 == arg = True
  check (HaxeFor _ arg _ _ _) | var1 == arg = True
  check (HaxeForIn _ arg _ _) | var1 == arg = True
  check _ = False

isRebound :: Haxe -> Haxe -> Bool
isRebound js d = any (\v -> isReassigned v d || isUpdated v d) (everythingOnHaxe (++) variablesOf js)
  where
  variablesOf (HaxeVar _ var) = [var]
  variablesOf _ = []

isUsed :: String -> Haxe -> Bool
isUsed var1 = everythingOnHaxe (||) check
  where
  check :: Haxe -> Bool
  check (HaxeVar _ var2) | var1 == var2 = True
  check (HaxeAssignment _ target _) | var1 == targetVariable target = True
  check _ = False

targetVariable :: Haxe -> String
targetVariable (HaxeVar _ var) = var
targetVariable (HaxeAccessor _ _ tgt) = targetVariable tgt
targetVariable (HaxeIndexer _ _ tgt) = targetVariable tgt
targetVariable _ = internalError "Invalid argument to targetVariable"

isUpdated :: String -> Haxe -> Bool
isUpdated var1 = everythingOnHaxe (||) check
  where
  check :: Haxe -> Bool
  check (HaxeAssignment _ target _) | var1 == targetVariable target = True
  check _ = False

removeFromBlock :: ([Haxe] -> [Haxe]) -> Haxe -> Haxe
removeFromBlock go (HaxeBlock ss sts) = HaxeBlock ss (go sts)
removeFromBlock _  js = js

isFn :: (String, String) -> Haxe -> Bool
isFn (moduleName, fnName) (HaxeAccessor _ x (HaxeVar _ y)) =
  x == fnName && y == moduleName
isFn (moduleName, fnName) (HaxeIndexer _ (HaxeStringLiteral _ x) (HaxeVar _ y)) =
  x == fnName && y == moduleName
isFn _ _ = False

isDict :: (String, String) -> Haxe -> Bool
isDict (moduleName, dictName) (HaxeAccessor _ x (HaxeVar _ y)) = x == dictName && y == moduleName
isDict _ _ = False

isDict' :: [(String, String)] -> Haxe -> Bool
isDict' xs js = any (`isDict` js) xs

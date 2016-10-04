-- |
-- This module implements tail call elimination.
--
module Language.PureScript.CodeGen.Haxe.Optimizer.TCO (tco) where

import Prelude.Compat

import Data.Monoid

import Language.PureScript.Options
import Language.PureScript.CodeGen.Haxe.AST

-- |
-- Eliminate tail calls
--
tco :: Options -> Haxe -> Haxe
tco opts | optionsNoTco opts = id
         | otherwise = tco'

tco' :: Haxe -> Haxe
tco' = everywhereOnHaxe convert
  where
  tcoLabel :: String
  tcoLabel = "tco"

  tcoVar :: String -> String
  tcoVar arg = "__tco_" ++ arg

  copyVar :: String -> String
  copyVar arg = "__copy_" ++ arg

  convert :: Haxe -> Haxe
  convert js@(HaxeVariableIntroduction ss name (Just fn@HaxeFunction {})) =
    let
      (argss, body', replace) = collectAllFunctionArgs [] id fn
    in case () of
      _ | isTailCall name body' ->
            let
              allArgs = concat $ reverse argss
            in
              HaxeVariableIntroduction ss name (Just (replace (toLoop name allArgs body')))
        | otherwise -> js
  convert js = js

  collectAllFunctionArgs :: [[String]] -> (Haxe -> Haxe) -> Haxe -> ([[String]], Haxe, Haxe -> Haxe)
  collectAllFunctionArgs allArgs f (HaxeFunction s1 ident args (HaxeBlock s2 (body@(HaxeReturn _ _):_))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (HaxeFunction s1 ident (map copyVar args) (HaxeBlock s2 [b]))) body
  collectAllFunctionArgs allArgs f (HaxeFunction ss ident args body@(HaxeBlock _ _)) =
    (args : allArgs, body, f . HaxeFunction ss ident (map copyVar args))
  collectAllFunctionArgs allArgs f (HaxeReturn s1 (HaxeFunction s2 ident args (HaxeBlock s3 [body]))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (HaxeReturn s1 (HaxeFunction s2 ident (map copyVar args) (HaxeBlock s3 [b])))) body
  collectAllFunctionArgs allArgs f (HaxeReturn s1 (HaxeFunction s2 ident args body@(HaxeBlock _ _))) =
    (args : allArgs, body, f . HaxeReturn s1 . HaxeFunction s2 ident (map copyVar args))
  collectAllFunctionArgs allArgs f body = (allArgs, body, f)

  isTailCall :: String -> Haxe -> Bool
  isTailCall ident js =
    let
      numSelfCalls = everythingOnHaxe (+) countSelfCalls js
      numSelfCallsInTailPosition = everythingOnHaxe (+) countSelfCallsInTailPosition js
      numSelfCallsUnderFunctions = everythingOnHaxe (+) countSelfCallsUnderFunctions js
      numSelfCallWithFnArgs = everythingOnHaxe (+) countSelfCallsWithFnArgs js
    in
      numSelfCalls > 0
      && numSelfCalls == numSelfCallsInTailPosition
      && numSelfCallsUnderFunctions == 0
      && numSelfCallWithFnArgs == 0
    where
    countSelfCalls :: Haxe -> Int
    countSelfCalls (HaxeApp _ (HaxeVar _ ident') _) | ident == ident' = 1
    countSelfCalls _ = 0

    countSelfCallsInTailPosition :: Haxe -> Int
    countSelfCallsInTailPosition (HaxeReturn _ ret) | isSelfCall ident ret = 1
    countSelfCallsInTailPosition _ = 0

    countSelfCallsUnderFunctions :: Haxe -> Int
    countSelfCallsUnderFunctions (HaxeFunction _ _ _ js') = everythingOnHaxe (+) countSelfCalls js'
    countSelfCallsUnderFunctions _ = 0

    countSelfCallsWithFnArgs :: Haxe -> Int
    countSelfCallsWithFnArgs ret = if isSelfCallWithFnArgs ident ret [] then 1 else 0

  toLoop :: String -> [String] -> Haxe -> Haxe
  toLoop ident allArgs js = HaxeBlock rootSS $
        map (\arg -> HaxeVariableIntroduction rootSS arg (Just (HaxeVar rootSS (copyVar arg)))) allArgs ++
        [ HaxeLabel rootSS tcoLabel $ HaxeWhile rootSS (HaxeBooleanLiteral rootSS True) (HaxeBlock rootSS [ everywhereOnHaxe loopify js ]) ]
    where
    rootSS = Nothing

    loopify :: Haxe -> Haxe
    loopify (HaxeReturn ss ret) | isSelfCall ident ret =
      let
        allArgumentValues = concat $ collectSelfCallArgs [] ret
      in
        HaxeBlock ss $ zipWith (\val arg ->
                    HaxeVariableIntroduction ss (tcoVar arg) (Just val)) allArgumentValues allArgs
                  ++ map (\arg ->
                    HaxeAssignment ss (HaxeVar ss arg) (HaxeVar ss (tcoVar arg))) allArgs
                  ++ [ HaxeContinue ss tcoLabel ]
    loopify other = other
    collectSelfCallArgs :: [[Haxe]] -> Haxe -> [[Haxe]]
    collectSelfCallArgs allArgumentValues (HaxeApp _ fn args') = collectSelfCallArgs (args' : allArgumentValues) fn
    collectSelfCallArgs allArgumentValues _ = allArgumentValues

  isSelfCall :: String -> Haxe -> Bool
  isSelfCall ident (HaxeApp _ (HaxeVar _ ident') _) = ident == ident'
  isSelfCall ident (HaxeApp _ fn _) = isSelfCall ident fn
  isSelfCall _ _ = False

  isSelfCallWithFnArgs :: String -> Haxe -> [Haxe] -> Bool
  isSelfCallWithFnArgs ident (HaxeVar _ ident') args | ident == ident' && any hasFunction args = True
  isSelfCallWithFnArgs ident (HaxeApp _ fn args) acc = isSelfCallWithFnArgs ident fn (args ++ acc)
  isSelfCallWithFnArgs _ _ _ = False

  hasFunction :: Haxe -> Bool
  hasFunction = getAny . everythingOnHaxe mappend (Any . isFunction)
    where
    isFunction HaxeFunction{} = True
    isFunction _ = False

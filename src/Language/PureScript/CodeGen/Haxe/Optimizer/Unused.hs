-- |
-- Removes unused variables
--
module Language.PureScript.CodeGen.Haxe.Optimizer.Unused
  ( removeCodeAfterReturnStatements
  , removeUnusedArg
  , removeUndefinedApp
  ) where

import Prelude.Compat

import Language.PureScript.CodeGen.Haxe.AST
import Language.PureScript.CodeGen.Haxe.Optimizer.Common
import qualified Language.PureScript.Constants as C

removeCodeAfterReturnStatements :: Haxe -> Haxe
removeCodeAfterReturnStatements = everywhereOnHaxe (removeFromBlock go)
  where
  go :: [Haxe] -> [Haxe]
  go jss | not (any isHaxeReturn jss) = jss
         | otherwise = let (body, ret : _) = break isHaxeReturn jss in body ++ [ret]
  isHaxeReturn (HaxeReturn _ _) = True
  isHaxeReturn _ = False

removeUnusedArg :: Haxe -> Haxe
removeUnusedArg = everywhereOnHaxe convert
  where
  convert (HaxeFunction ss name [arg] body) | arg == C.__unused = HaxeFunction ss name [] body
  convert js = js

removeUndefinedApp :: Haxe -> Haxe
removeUndefinedApp = everywhereOnHaxe convert
  where
  convert (HaxeApp ss fn [HaxeVar _ arg]) | arg == C.undefined = HaxeApp ss fn []
  convert js = js

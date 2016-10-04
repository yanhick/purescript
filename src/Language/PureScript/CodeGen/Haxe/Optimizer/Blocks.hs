-- |
-- Optimizer steps for simplifying Javascript blocks
--
module Language.PureScript.CodeGen.Haxe.Optimizer.Blocks
  ( collapseNestedBlocks
  , collapseNestedIfs
  ) where

import Prelude.Compat

import Language.PureScript.CodeGen.Haxe.AST

-- |
-- Collapse blocks which appear nested directly below another block
--
collapseNestedBlocks :: Haxe -> Haxe
collapseNestedBlocks = everywhereOnHaxe collapse
  where
  collapse :: Haxe -> Haxe
  collapse (HaxeBlock ss sts) = HaxeBlock ss (concatMap go sts)
  collapse js = js
  go :: Haxe -> [Haxe]
  go (HaxeBlock _ sts) = sts
  go s = [s]

collapseNestedIfs :: Haxe -> Haxe
collapseNestedIfs = everywhereOnHaxe collapse
  where
  collapse :: Haxe -> Haxe
  collapse (HaxeIfElse s1 cond1 (HaxeBlock _ [HaxeIfElse s2 cond2 body Nothing]) Nothing) =
      HaxeIfElse s1 (HaxeBinary s2 And cond1 cond2) body Nothing
  collapse js = js

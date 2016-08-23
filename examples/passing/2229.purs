module Main where

import Prelude
import Control.Monad.Eff.Console (log)

foo =
  if true
    then { test: id }
    else { test: id }

bar = _ { test = id }

baz =
  if false
    then { test: show }
    else { test: id }

main = log (baz.test "Done")

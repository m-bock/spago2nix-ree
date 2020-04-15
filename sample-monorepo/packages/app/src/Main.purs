module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Sub.Module as SubModule
import Lib1 as Lib1

foreign import depTest :: Effect Unit

main :: Effect Unit
main = do
  depTest
  log SubModule.x
  log Lib1.x
  log "🍝"

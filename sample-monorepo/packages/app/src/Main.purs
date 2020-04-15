module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Sub.Module as SubModule

foreign import depTest :: Effect Unit

main :: Effect Unit
main = do
  depTest
  log SubModule.x
  log "🍝"

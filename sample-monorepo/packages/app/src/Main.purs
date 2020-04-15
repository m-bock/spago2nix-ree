module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

foreign import depTest :: Effect Unit

main :: Effect Unit
main = do
  depTest
  log "🍝"

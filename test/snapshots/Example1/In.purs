module Example1.In where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)

main :: Effect Unit
main = launchAff_ do
  log "This is some text I'm logging to the console"
  pure unit
  log $ show "foo"

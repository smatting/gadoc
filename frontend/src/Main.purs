module Main where

import Prelude

import Effect (Effect)
import Browser as Browser
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Browser.component unit body

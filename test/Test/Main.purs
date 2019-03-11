module Test.Main where

import Prelude

import Effect as Effect
import Test.Button as Test.Button
import Test.Unit.Main as Test.Unit.Main

main :: Effect.Effect Unit
main = Test.Unit.Main.runTest do
  Test.Button.suite

module Test.Container (suite) where

import Prelude

import Data.Lens ((+=))
import Data.Lens as Data.Lens
import Data.Lens.Record as Data.Lens.Record
import Data.Maybe as Data.Maybe
import Data.Symbol as Data.Symbol
import Effect.Aff as Effect.Aff
import Halogen as Halogen
import Halogen.Driver as Halogen.Driver
import Test.Button as Test.Button
import Test.Unit as Test.Unit
import Test.Unit.Assert as Test.Unit.Assert

suite :: Test.Unit.TestSuite
suite = Test.Unit.suite "Test.Container" do
  Test.Unit.suite "eval" do
    checkButtonState
    handleButton

checkButtonState :: Test.Unit.TestSuite
checkButtonState = Test.Unit.suite "CheckButtonState" do
  Test.Unit.suite "without a child" do
    Test.Unit.test "won't give a useful result" $ run do
      eval (CheckButtonState unit)
      { buttonState } <- Halogen.get
      Halogen.liftAff (Test.Unit.Assert.equal Data.Maybe.Nothing buttonState)

handleButton :: Test.Unit.TestSuite
handleButton = Test.Unit.suite "HandleButton" do
  Test.Unit.test "increments `toggleCount`" $ run do
    old <- Halogen.get
    eval (HandleButton (Test.Button.Toggled true) unit)
    new <- Halogen.get
    Halogen.liftAff (Test.Unit.Assert.equal (old.toggleCount + 1) new.toggleCount)

run :: Halogen.HalogenM State Query Test.Button.Query Unit Void Effect.Aff.Aff ~> Effect.Aff.Aff
run =
  Halogen.Driver.evalAff
    parent
    eval
    { buttonState: Data.Maybe.Nothing
    , toggleCount: 0
    }

data Query a
  = HandleButton Test.Button.Message a
  | CheckButtonState a

type State
  = { buttonState :: Data.Maybe.Maybe Boolean
    , toggleCount :: Int
    }

eval ::
  forall f.
  Query ~> Halogen.ParentDSL State Query Test.Button.Query Unit Void f
eval = case _ of
  HandleButton message next -> case message of
    Test.Button.Toggled _ -> do
      toggleCount += 1
      pure next
  CheckButtonState next -> do
    x <- Halogen.query unit (Test.Button.IsOn identity)
    pure next

parent :: Void -> Effect.Aff.Aff Unit
parent = absurd

toggleCount :: Data.Lens.Lens' State Int
toggleCount = Data.Lens.Record.prop (Data.Symbol.SProxy :: _ "toggleCount")

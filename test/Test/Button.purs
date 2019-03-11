module Test.Button (suite) where

import Prelude

import Data.Const as Data.Const
import Data.Newtype as Data.Newtype
import Effect.Aff as Effect.Aff
import Halogen as Halogen
import Halogen.Driver as Halogen.Driver
import Test.Unit as Test.Unit
import Test.Unit.Assert as Test.Unit.Assert

suite :: Test.Unit.TestSuite
suite = Test.Unit.suite "button" do
  Test.Unit.suite "eval" do
    isOn
    toggle

isOn :: Test.Unit.TestSuite
isOn = Test.Unit.suite "IsOn" do
  Test.Unit.test "answers with `false` when toggled off" $ run do
    on <- eval (IsOn identity)
    Halogen.liftAff (Test.Unit.Assert.assertFalse "state should be `false`" on)

  Test.Unit.test "answers with `true` when toggled on" $ run do
    eval (Toggle unit)
    on <- eval (IsOn identity)
    Halogen.liftAff (Test.Unit.Assert.assert "state should be `true`" on)

toggle :: Test.Unit.TestSuite
toggle = Test.Unit.suite "Toggle" do
  Test.Unit.test "changes state from `false` to `true`" $ run do
    eval (Toggle unit)
    state <- Halogen.get
    Halogen.liftAff (Test.Unit.Assert.assert "State should be `true`" state)

  Test.Unit.test "changes state from `true` to `false`" $ run do
    eval (Toggle unit)
    eval (Toggle unit)
    state <- Halogen.get
    Halogen.liftAff (Test.Unit.Assert.assertFalse "state should be `false`" state)

run :: Halogen.HalogenM Boolean Query (Data.Const.Const Void) Void Message Effect.Aff.Aff ~> Effect.Aff.Aff
run = Halogen.Driver.evalAff parent child eval false

newtype Message = Toggled Boolean

data Query a
  = Toggle a
  | IsOn (Boolean -> a)

type State = Boolean

child :: Data.Const.Const Void ~> Query
child = absurd <<< Data.Newtype.un Data.Const.Const

eval ::
  forall m.
  Query ~> Halogen.ComponentDSL State Query Message m
eval = case _ of
  Toggle next -> do
    state <- Halogen.get
    let nextState = not state
    Halogen.put nextState
    Halogen.raise (Toggled nextState)
    pure next
  IsOn reply -> do
    state <- Halogen.get
    pure (reply state)

parent :: Message -> Effect.Aff.Aff Unit
parent = case _ of
  Toggled _ -> mempty

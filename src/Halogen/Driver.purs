module Halogen.Driver (aff, evalAff) where

import Prelude

import Control.Applicative.Free as Control.Applicative.Free
import Control.Coroutine as Control.Coroutine
import Control.Monad.Free as Control.Monad.Free
import Control.Monad.State as Control.Monad.State
import Control.Plus as Control.Plus
import Data.Foldable as Data.Foldable
import Data.Maybe as Data.Maybe
import Data.Set as Data.Set
import Data.Tuple as Data.Tuple
import Effect.Aff as Effect.Aff
import Effect.Exception as Effect.Exception
import Halogen as Halogen
import Halogen.Query.EventSource as Halogen.Query.EventSource
import Halogen.Query.ForkF as Halogen.Query.ForkF
import Halogen.Query.HalogenM as Halogen.Query.HalogenM
import Unsafe.Coerce as Unsafe.Coerce

type AffState s p
  = { children :: Data.Set.Set p
    , state :: s
    }

aff ::
  forall f g h i o p.
  Ord p =>
  (o -> Effect.Aff.Aff Unit) ->
  (g ~> f) ->
  i ->
  Halogen.Component h f i o Effect.Aff.Aff ->
  Effect.Aff.Aff Unit
aff parent child input component =
  Data.Foldable.for_ (spec.receiver input) \x ->
    evalAff parent spec.eval initialState (spec.eval x)
  where
  initialState :: forall s. s
  initialState = spec.initialState input
  spec ::
    forall s.
    Halogen.ParentLifecycleComponentSpec h s f g p i o Effect.Aff.Aff
  spec = Halogen.unComponent Unsafe.Coerce.unsafeCoerce component

evalAff ::
  forall s f g p o.
  Ord p =>
  (o -> Effect.Aff.Aff Unit) ->
  (f ~> Halogen.HalogenM s f g p o Effect.Aff.Aff) ->
  s ->
  Halogen.HalogenM s f g p o Effect.Aff.Aff ~> Effect.Aff.Aff
evalAff parent eval initialState (Halogen.HalogenM x'') =
  Control.Monad.State.evalStateT (Control.Monad.Free.foldFree go x'') initialState
  where
  go ::
    Halogen.HalogenF s f g p o Effect.Aff.Aff ~>
    Control.Monad.State.StateT s Effect.Aff.Aff
  go = case _ of
    Halogen.ChildQuery _ _ -> Control.Plus.empty
    Halogen.CheckSlot _ f -> pure (f false)
    Halogen.Fork x' -> Halogen.liftAff do
      flap Halogen.Query.ForkF.unFork x' \(Halogen.Query.ForkF.ForkF x f) -> do
        let unforked = evalAff parent eval initialState x
        fiber <- Effect.Aff.forkAff unforked
        pure (f $ flip Effect.Aff.killFiber fiber)
    Halogen.GetRef _ f -> pure (f Data.Maybe.Nothing)
    Halogen.GetSlots f -> pure (f mempty)
    Halogen.Halt error -> Effect.Aff.throwError (Effect.Exception.error error)
    Halogen.Lift x -> Halogen.liftAff x
    Halogen.Par (Halogen.Query.HalogenM.HalogenAp x) -> do
      let loop = evalAff parent eval initialState
      Halogen.liftAff (Control.Applicative.Free.foldFreeAp loop x)
    Halogen.Raise message y -> do
      Halogen.liftAff (parent message)
      pure y
    Halogen.State f -> do
      state <- Control.Monad.State.get
      let Data.Tuple.Tuple x s = f state
      Control.Monad.State.put s
      pure x
    Halogen.Subscribe eventSource x' -> Halogen.liftAff do
      let consumer ::
            Control.Coroutine.Consumer
              (f Halogen.Query.EventSource.SubscribeStatus)
              Effect.Aff.Aff Unit
          consumer = do
            x <- Control.Coroutine.await
            status <-
              Control.Monad.State.lift
                (evalAff parent eval initialState $ eval x)
            case status of
              Halogen.Query.EventSource.Done -> mempty
              Halogen.Query.EventSource.Listening -> consumer
      { producer, done } <- Halogen.Query.EventSource.unEventSource eventSource
      Control.Coroutine.runProcess (Control.Coroutine.connect producer consumer)
      done
      pure x'

module KeyCombo.Events where

import KeyCombo.Types

import Control.Coroutine (Consumer, Producer, await)
import Control.Coroutine.Aff (produce')
import Control.Extend (extend, map, void)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (Ref, modifyRef, modifyRef', readRef)
import Control.Monad.Except (lift, runExcept)
import Control.Monad.Rec.Class (forever)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.KeyboardEvent (eventToKeyboardEvent, key)
import DOM.HTML.Event.EventTypes (keydown, keyup)
import DOM.Node.Types (Element, elementToEventTarget)
import Data.Array (catMaybes, concat, head, intersect, nub, sort, sortBy, tail, union, zip)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.StrMap (StrMap, delete, empty, fromFoldable, insert, isEmpty, keys, member, size, values)
import Data.Tuple (Tuple(..), fst, snd)
import Debug.Trace (traceAnyM)
import KeyCombo.Performance as Performance
import Math (abs)
import Prelude (Unit, bind, compare, discard, pure, unit, when, (#), ($), (&&), (-), (<), (==), (>), (>>=))

-- | Remove a key from the state's currently
-- | pressed keys and add it to the release queue recording
-- | when it was pressed
removeKey :: forall e. String -> Performance.TimeStamp -> KeyState -> KeyState
removeKey k (Performance.TimeStamp time) (KeyState { pressed, releaseQueue }) =
  KeyState {
    pressed : delete k pressed,
    releaseQueue : insert k time releaseQueue
  }

-- | Add a key to the state's currently
-- | pressed keys
addKey :: String -> KeyState -> KeyState
addKey k (KeyState { pressed, releaseQueue }) =
  KeyState {
    pressed : insert k true pressed,
    releaseQueue : empty
  }

-- | Produce key press events on a particular element
keyDownProducer :: forall e. Element -> Producer String (KeyComboM e) Unit
keyDownProducer root = produce' \emit -> do
  addEventListener
    keydown
     (eventListener \e ->
      -- do void $ traceAnyM e
      case runExcept $ eventToKeyboardEvent e of
        Right event -> emit $ Left (key event)
        Left err -> pure unit
    )
    false
    (elementToEventTarget root)

-- | Produce key release events on a particular element
keyUpProducer :: forall e. Element -> Producer String (KeyComboM e) Unit
keyUpProducer root = produce' \emit -> do
  addEventListener
    keyup
    (eventListener \e ->      
      case runExcept $ eventToKeyboardEvent e of
        Right event -> emit $ Left (key event)
        Left err -> pure unit
    )
    false
    (elementToEventTarget root)

-- | Consume key presses, modify the state accordingly, and
-- | trigger certain event listeners
keyDownListener :: forall e
   . Ref KeyState
  -> OnKeyDown e
  -> Consumer String (KeyComboM e) Unit
keyDownListener state (OnKeyDown fn) = forever $ do
  k <- await
  (KeyState {pressed}) <- liftEff $ readRef state
  if (member k pressed)
    then pure unit
    else do
      r <- liftEff $ modifyRef' state (\s ->
        let v = addKey k s
        in { state : v, value : v }
      )
      lift $ fn k r

-- | Consume key releases, modify the state accordingly, and
-- | trigger certain event listeners
keyUpListener :: forall e
   . Ref KeyState
  -> OnKeyUp e
  -> OnComboRelease e
  -> OnExactRelease e
  -> Consumer String (KeyComboM e) Unit
keyUpListener
  state
  (OnKeyUp keyUp)
  (OnComboRelease comboRelease)
  (OnExactRelease exactReleases) = forever $ do
    k <- await
    now <- liftEff $ Performance.now'
    r@KeyState({ pressed, releaseQueue }) <- liftEff $ modifyRef' state (\s ->
      let v = removeKey k now s
      in { state : v, value : v }
    )

    when (isEmpty pressed) $
      lift $ exactReleases # traverse_ (\(Tuple userKeys action) -> do
        let zipped = zip (keys releaseQueue) (values releaseQueue) 
        let times = sortBy (\a b -> compare (snd a) (snd b)) zipped

        let s1 = sort $ union (map fst times) userKeys

        -- void $ traceAnyM [s1, userKeys]
        if s1 == (sort userKeys)
          then action unit
          else pure unit
      )

    lift $ keyUp k r

    let combo = stateHasCombo (MillisecondInterval 20.0) r
    case (isEmpty combo) of
      true -> pure unit
      false -> do
        lift $ comboRelease combo
        liftEff $ modifyRef state (\(KeyState s) -> KeyState {
          pressed : s.pressed,
          releaseQueue : empty
        })
     

-- | Check if the current keypress state has a key combo, that is
-- | no other keys are being pressed, and the release time
-- | between the keys is less than the specified limit
stateHasCombo :: MillisecondInterval -> KeyState -> StrMap Number
stateHasCombo (MillisecondInterval limit) (KeyState { pressed, releaseQueue}) =
  case isEmpty pressed && size releaseQueue > 1 of
    false -> empty
    true -> fromFoldable $ nub $ concat $ catMaybes do
      let zipped = zip (keys releaseQueue) (values releaseQueue) 
      let times = sortBy (\a b -> compare (snd a) (snd b)) zipped
      times # extend \s -> do
        h1 <- head s
        h2 <- tail s >>= head
        let d = (abs ((snd h1) - (snd h2)))
        pure $ if (d < limit)
          then [h1, h2]
          else []

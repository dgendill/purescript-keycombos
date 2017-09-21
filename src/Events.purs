module KeyCombo.Events where

import Control.Coroutine (Consumer, Producer, await, pullFrom, runProcess)
import Control.Coroutine.Aff (produce')
import Control.Extend (extend, map, (<$>))
import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, modifyRef', newRef)
import Control.Monad.Except (lift, runExcept)
import Control.Monad.Rec.Class (forever)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.KeyboardEvent (eventToKeyboardEvent, key)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (keydown, keyup)
import DOM.HTML.Types (htmlDocumentToEventTarget)
import DOM.HTML.Window (document)
import Data.Array (catMaybes, concat, fold, foldMap, foldl, head, nub, reverse, sort, sortBy, tail, zip, zipWith)
import Data.Either (Either(..))
import Data.Monoid (mempty, (<>))
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap (StrMap, delete, empty, fromFoldable, insert, isEmpty, keys, size, values)
import Data.StrMap (fold) as StrMap
import Data.Tuple (Tuple(..), fst, snd)
import Debug.Trace (spy, traceAnyM)
import KeyCombo.Performance as Performance
import Math (abs, min)
import Prelude (class Show, Unit, bind, compare, discard, max, pure, show, unit, void, (#), ($), (&&), (-), (<), (>), (>>=))

newtype KeyState = KeyState {
  pressed :: StrMap Boolean,
  releaseQueue :: StrMap Number
}
derive instance newtypeKeyState :: Newtype KeyState _

data KeyCombo
  = ReleaseRequired KeyCombo
  | KeyCombo String (Array Char)

instance showKeyState :: Show KeyState where
  show (KeyState { pressed : pressed }) = show pressed


-- | Event handler for one key being pressed
newtype OnKeyDown e = OnKeyDown (String -> KeyState -> Aff (Effects e) Unit)

-- | Event handler for one key being released
newtype OnKeyUp e = OnKeyUp (String -> KeyState -> Aff (Effects e) Unit)

-- | Event handler for multiple keys being
-- | released all at approximatly the same time
newtype OnComboRelease e = OnComboRelease (StrMap Number -> Aff (Effects e) Unit)

type Effects e = (ref :: REF, avar :: AVAR, console :: CONSOLE, dom :: DOM | e)
type KeyComboM e = Aff (Effects e)


removeKey :: forall e. String -> Number -> KeyState -> KeyState
removeKey k time (KeyState { pressed, releaseQueue }) =
  KeyState {
    pressed : delete k pressed,
    releaseQueue : insert k time releaseQueue
  }

addKey :: String -> KeyState -> KeyState
addKey k (KeyState { pressed, releaseQueue }) =
  KeyState {
    pressed : insert k true pressed,
    releaseQueue : empty
  }

keyDownProducer :: forall e. Producer String (KeyComboM e) Unit
keyDownProducer = produce' \emit -> do
  document <- window >>= document
  addEventListener
    keydown
     (eventListener \e ->
      case runExcept $ eventToKeyboardEvent e of
        Right event -> emit $ Left (key event)
        Left err -> pure unit -- emit $ Left ""
    )
    false
    (htmlDocumentToEventTarget document)

keyUpProducer :: forall e. Producer String (KeyComboM e) Unit
keyUpProducer = produce' \emit -> do
  document <- window >>= document
  addEventListener
    keyup
    (eventListener \e ->
      case runExcept $ eventToKeyboardEvent e of
        Right event -> emit $ Left (key event)
        Left err -> emit $ Left ""
    )
    false
    (htmlDocumentToEventTarget document)

keyDownListener :: forall e
   . Ref KeyState
  -> OnKeyDown e
  -> Consumer String (KeyComboM e) Unit
keyDownListener state (OnKeyDown fn) = forever $ do
  k <- await
  r <- liftEff $ modifyRef' state (\s ->
    let v = addKey k s
    in { state : v, value : v }
  )
  lift $ fn k r

keyUpListener :: forall e
   . Ref KeyState
  -> OnKeyUp e
  -> OnComboRelease e
  -> Consumer String (KeyComboM e) Unit
keyUpListener
  state
  (OnKeyUp keyUp)
  (OnComboRelease comboRelease) = forever $ do
    k <- await
    now <- liftEff $ Performance.now
    r <- liftEff $ modifyRef' state (\s ->
      let v = removeKey k now s
      in { state : v, value : v }
    )
    lift $ keyUp k r
    
    let latestKeyReleased = StrMap.fold (\m _ v ->
            max m v
          ) 0.0 ((unwrap r).releaseQueue)
    
    void $ traceAnyM $ (show latestKeyReleased) <> "  " <> (show now)


    let combo = stateHasCombo 5.0 r
    case (isEmpty combo) of
      true -> pure unit
      false -> do
        lift $ comboRelease combo
        liftEff $ modifyRef state (\(KeyState s) -> KeyState {
          pressed : s.pressed,
          releaseQueue : empty
        })
     

-- | Only trigger the combo if the time between
-- | first key release and last key release is less
-- | than the limit
stateHasCombo :: Number -> KeyState -> StrMap Number
stateHasCombo limit (KeyState { pressed, releaseQueue}) =
  case isEmpty pressed &&  size releaseQueue > 1 of
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
  

run :: forall e
   . OnKeyDown e
  -> OnKeyUp e
  -> OnComboRelease e
  -> Aff (Effects e) Unit
run keyDown keyUp keyComboRelease = do
  r <- liftEff $ newRef (KeyState {
    pressed : empty,
    releaseQueue : empty
  })
  void $ forkAff (runProcess do
    (keyUpListener r keyUp keyComboRelease) `pullFrom` keyUpProducer
  )
  void $ forkAff (runProcess do
    (keyDownListener r keyDown) `pullFrom` keyDownProducer  
  )
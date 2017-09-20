module Main.Events where

import Prelude

import Control.Coroutine (Consumer, Producer, await, pullFrom, runProcess)
import Control.Coroutine.Aff (produce')
import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef', newRef)
import Control.Monad.Except (lift, runExcept)
import Control.Monad.Rec.Class (forever)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.KeyboardEvent (eventToKeyboardEvent, key)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (keydown, keyup)
import DOM.HTML.Types (htmlDocumentToEventTarget)
import DOM.HTML.Window (document)
import Data.Either (Either(..))
import Data.Monoid (mempty)
import Data.StrMap (StrMap, delete, insert)

newtype KeyState = KeyState {
  pressed :: StrMap String
}
instance showKeyState :: Show KeyState where
  show (KeyState { pressed : pressed }) = show pressed

removeKey :: String -> KeyState -> KeyState
removeKey k (KeyState { pressed : keys}) =
  KeyState { pressed : delete k keys }

addKey :: String -> KeyState -> KeyState
addKey k (KeyState { pressed : keys}) =
  KeyState { pressed : insert k k keys }

type Effects e = (ref :: REF, avar :: AVAR, console :: CONSOLE, dom :: DOM | e)
type KeyComboM e = Aff (Effects e)

keyDownProducer :: forall e. Producer String (KeyComboM e) Unit
keyDownProducer = produce' \emit -> do
  document <- window >>= document
  addEventListener
    keydown
     (eventListener \e ->
      case runExcept $ eventToKeyboardEvent e of
        Right event -> emit $ Left (key event)
        Left err -> emit $ Left ""
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
  -> (KeyState -> Aff (Effects e) Unit)
  -> Consumer String (KeyComboM e) Unit
keyDownListener state fn = forever $ do
  c <- await
  r <- liftEff $ modifyRef' state (\s ->
    let v = addKey c s
    in { state : v, value : v }
  )
  lift $ fn r

keyUpListener :: forall e
   . Ref KeyState
  -> (KeyState -> Aff (Effects e) Unit)
  -> Consumer String (KeyComboM e) Unit
keyUpListener state fn = forever $ do
  c <- await
  r <- liftEff $ modifyRef' state (\s ->
    let v = removeKey c s
    in { state : v, value : v }
  )
  lift $ fn r

run :: forall e
   . (KeyState -> Aff (Effects e) Unit)
  -> Aff (Effects e) Unit-- KeyComboM e Unit -- Eff (Effects e) Unit
run cb = do
    r <- liftEff $ newRef (KeyState { pressed : mempty })
    void $ forkAff (runProcess do
      (keyUpListener r cb) `pullFrom` keyUpProducer
    )
    void $ forkAff (runProcess do
      (keyDownListener r cb) `pullFrom` keyDownProducer  
    )
module KeyCombo (
    addKeyComboListener,
    addKeyComboListenerOn,
    jsSetup,
    module Types
  ) where

import KeyCombo.Events
import KeyCombo.FFI
import KeyCombo.Types
import Prelude

import Control.Coroutine (connect, runProcess)
import Control.Monad.Aff (Aff, forkAff, runAff_, throwError)
import Control.Monad.Aff.Compat (EffFn1, EffFn2, fromEffFnAff, mkEffFn1, mkEffFn2)
import Control.Monad.Aff.Console as Affc
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (error, log, logShow)
import Control.Monad.Eff.Ref (newRef)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (QuerySelector(..), querySelector, querySelectorAll)
import DOM.Node.Types (Element, elementToParentNode)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Foreign (Foreign)
import Data.Functor (voidRight)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.StrMap (empty)
import Data.String (joinWith)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, isValid, unV)
import Debug.Trace (traceAnyM)
import KeyCombo.Types as Types
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

show1 :: forall e a. (Show a) => a -> Aff (Effects e) Unit
show1 a = Affc.log $ (show a)

show2 :: forall e a b. (Show a) => (Show b) => a -> b -> Aff (Effects e) Unit
show2 a b = Affc.log $ (show a) <> " " <> (show b)

doNothing1 :: forall e a. (Show a) => a -> Aff (Effects e) Unit
doNothing1 a = pure unit

doNothing2 :: forall e a b. (Show a) => (Show b) => a -> b -> Aff (Effects e) Unit
doNothing2 a b = pure unit

validateSetup :: Foreign -> V (Array String) Foreign
validateSetup f = unsafePartial $
  lmap (specifyNoun "The KeyCombo configuration") $
    hasAtLeastOneOf f [
      "onKeyDown", "onKeyUp",
      "onComboRelease", "onExactRelease"
    ] *>

  allObjectPairsUser f [
    "onKeyDown", "onKeyUp",
    "onComboRelease"
  ] "are functions" (\key f -> isFunction' f)  *>

  allObjectPairsUser f [
    "onKeyDown", "onKeyUp"
  ] "should have two function arguments (key, state)"
    (\key f -> getArgumentCount f == 2)  *>
  
  allObjectPairsUser f [
    "onComboRelease"
  ] "should have one function arguments (keyPressTimes)"
    (\key f -> getArgumentCount f == 1) *>

  mutateObject f [
    "onKeyDown", "onKeyUp"
  ] ffiEff2ToAff  *>

  mutateObject f [
    "onComboRelease"
  ] ffiEff1ToAff  *>

  setDefaultKeys f [
    Tuple "onKeyDown" (doNothing2 :: String -> KeyState -> _),
    Tuple "onKeyUp" (doNothing2 :: String -> KeyState -> _)
  ]  *>

  setDefaultKeys f [
    Tuple "onComboRelease" (doNothing1 :: String -> _)
  ] *>

  setDefaultKeys f [
    Tuple "onExactRelease" []
  ]

fromNestedMaybe :: forall a. a -> Maybe (Maybe a) -> a
fromNestedMaybe a m = fromMaybe a (fromMaybe Nothing m)

-- | Accept a foreign configuation object and
-- | attempt to run the key combo handler on the
-- | document
jsSetup :: forall e. Foreign -> Eff (Effects e) Unit
jsSetup config = do
  document' <- unsafeCoerce <$> (window >>= document)

  -- isString a *> 

  elementOk <- case getObjectKey config "element" of
    Just selectorOrElement -> do
      if (isString' selectorOrElement)
        then do
          r <- querySelector
                  (QuerySelector (unsafeCoerce selectorOrElement))
                  (elementToParentNode document')

          case r of  
            Just e ->
              pure true <*
                (pure
                $ unsafePartial
                $ setObjectKeyValue config "element" e)
            Nothing -> do
              error $
                "Could not find query selector '"
                <> (unsafeCoerce selectorOrElement)
                <> "' on the page."
              pure false
        else pure true
    Nothing -> pure true

  if elementOk
    then do
      let result = unV
            (\err -> Left $ joinWith " " err)
            (\s -> Right "Success")
            (validateSetup config) 

      -- void $ traceAnyM f

      case result of
        Left a -> error a
        Right a -> runAff_ (\r -> case r of
            Left e -> error $ show e
            Right r -> log "ok"
          ) `traverse_` (
            addKeyComboListenerOn
            <$> (getObjectKey config "element")
            <*> (OnKeyDown <$> (getObjectKey config "onKeyDown"))
            <*> (OnKeyUp <$> (getObjectKey config "onKeyUp"))
            <*> (OnComboRelease <$> (getObjectKey config "onComboRelease"))
            <*> (OnExactRelease <$> (getObjectKey config "onExactRelease"))
          )
    else pure unit

-- | Listen for keyboard input on the document element
addKeyComboListener :: forall e
   . OnKeyDown e
  -> OnKeyUp e
  -> OnComboRelease e
  -> OnExactRelease e
  -> Aff (Effects e) Unit
addKeyComboListener keyDown keyUp keyComboRelease exactReleases = do
  root <- liftEff $ unsafeCoerce <$> (window >>= document)
  addKeyComboListenerOn root keyDown keyUp keyComboRelease exactReleases

addKeyComboListenerOn :: forall e
  .  Element
  -> OnKeyDown e
  -> OnKeyUp e
  -> OnComboRelease e
  -> OnExactRelease e
  -> Aff (Effects e) Unit
addKeyComboListenerOn root keyDown keyUp keyComboRelease exactReleases = do
  r <- liftEff $ newRef (KeyState {
    pressed : empty,
    releaseQueue : empty
  })
  void $ forkAff (runProcess $
    connect
      (keyUpProducer root)
      (keyUpListener r keyUp keyComboRelease exactReleases)
  )
  void $ forkAff (runProcess $
    connect
      (keyDownProducer root)
      (keyDownListener r keyDown) 
  )
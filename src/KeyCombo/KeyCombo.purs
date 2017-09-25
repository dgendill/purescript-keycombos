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
import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Aff.Compat (fromEffFnAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (error, log, logShow)
import Control.Monad.Eff.Ref (newRef)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.Node.Types (Element)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.StrMap (empty)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, unV)
import KeyCombo.Types as Types
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

doNothing :: forall e. Aff (Effects e) Unit
doNothing = pure unit

validateSetup :: Foreign -> V (Array String) Foreign
validateSetup f = unsafePartial $
  lmap (specifyNoun "The KeyCombo configuration") $
    hasAtLeastOneOf f [
      "onKeyDown", "onKeyUp",
      "onKeyComboRelease", "onExactRelease"
    ] *>

  mutateObject f [
    "onKeyDown", "onKeyUp",
    "onKeyComboRelease", "onExactRelease"
  ] fromEffFnAff *>

  setDefaultKeys f [
    Tuple "onKeyDown" doNothing,
    Tuple "onKeyComboRelease" doNothing,
    Tuple "onKeyUp" doNothing,
    Tuple "onExactRelease" doNothing
  ]

  

jsSetup :: forall e. Foreign -> Eff (Effects e) Unit
jsSetup f = do
  let result = unV
        (\err -> Left $ joinWith " " err)
        (\s -> Right "Success")
        (validateSetup f)
  
  case result of
    Left a -> error a
    Right a -> log a

  

--   validateSetup f $>
--   ffiApply addKeyComboListenerOn
--     [ (ffi ffiObjKey "element" f)
--     , (ffiObjKey "onKeyDown" f)
--     , (unsafeCoerceObjKeyFn "onKeyUp" f)
--     , (unsafeCoerceObjKeyFn "onKeyComboRelease" f)
--     , (ffiObjKey "onExactRelease" f # ffiMap (
--         ffiFst >>> ffiTuple <<< ffiSnd
--                (ffiSnd ffiEffToAff)
--         <<< ffiTuple
--         )
--     ]

-- {
--   element : Element | Selector,
--   onKeyDown : function(key, state) {}
--   onKeyUp : function(key, state) {}
--   onKeyComboRelease : function(key, state) {}
--   onExactRelease : [
--     [["Control","Shift","+"], function() {}],
--     [["Control","Shift","-"], function() {}],
-- }

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
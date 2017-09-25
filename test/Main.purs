module Test.Main where

import Control.Monad.Aff (launchAff_)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (log) as Effc
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Uncurried (EffFn1, mkEffFn1)
import DOM (DOM)
import Data.Foreign (Foreign)
import Data.Tuple (Tuple(..))
import KeyCombo (addKeyComboListener, OnKeyDown(..), OnKeyUp(..), OnComboRelease(..), OnExactRelease(..))
import KeyCombo as KeyCombo
import Prelude (Unit, discard, show, ($), (<>))

main :: forall e. Eff (ref :: REF , avar :: AVAR , dom :: DOM, console :: CONSOLE | e) Unit
main = do
  Effc.log "Running..."

  -- Rigid
  launchAff_ $ addKeyComboListener
    (OnKeyDown \k s -> do
      log $ "KeyDown " <> k
    )
    (OnKeyUp \k s -> do
      log $ "KeyUp " <>  k
    )
    (OnComboRelease \k -> do
      log $ "Combo " <> (show k)
    )
    (OnExactRelease [
      Tuple ["Control","Shift","+"] \k -> do
        log $ "Ctrl Shift +"
    ])

jsSetup :: forall e. EffFn1 (ref :: REF , avar :: AVAR , dom :: DOM, console :: CONSOLE | e) Foreign Unit
jsSetup = mkEffFn1 \f -> KeyCombo.jsSetup f

  -- More flexible
  -- errors if you call any fn twice
  -- allows optional setup of certain listeners
  -- launchAff_ $ addKeyComboListener' do
  --   -- Adds row 'onKeyDown'
  --   (onKeyDown \k s -> do
  --     log $ "KeyDown2 " <> k
  --   ) 
  --   -- Adds row 'onKeyUp
  --   (onKeyUp \k s -> do
  --     log $ "KeyUp2 " <>  k
  --   )
  --   -- Adds row 'onComboRelease'
  --   (onComboRelease \k -> do
  --     log $ "Combo2 " <> (show k)
  --   )
  --   -- Adds row 'onExactRelease'
  --   (onExactRelease [
  --     Tuple ["a","s","d"] \k -> do
  --       log $ "Exact Keypress a+s+d2 "
  --   ])
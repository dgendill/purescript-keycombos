module Main where

import Control.Monad.Aff (launchAff_)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (log) as Effc
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import KeyCombo.Events (run, OnKeyDown(..), OnKeyUp(..), OnComboRelease(..))
import Prelude (Unit, discard, show, ($), (<>))

main :: forall e. Eff (ref :: REF , avar :: AVAR , dom :: DOM, console :: CONSOLE | e) Unit
main = do
  Effc.log "Running..."
  launchAff_ $ run
    (OnKeyDown \k s -> do
      log $ "KeyDown " <> k
    )
    (OnKeyUp \k s -> do
      log $ "KeyUp " <>  k
    )
    (OnComboRelease \k -> do
      log $ "Combo " <> (show k)
    )

data KeyCombo
  = ReleaseRequired KeyCombo
  | KeyCombo String (Array Char)
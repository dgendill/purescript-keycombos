module Main where

import Control.Monad.Aff (launchAff_)
import Control.Monad.Aff.Console (log)
import Main.Events (run)
import Prelude (show, ($))

-- main :: forall e. Eff (ref :: REF , avar :: AVAR , dom :: DOM, console :: CONSOLE | e) Unit
main = do
  launchAff_ $ run (\c -> do
    log $ show c
  )

data KeyCombo
  = ReleaseRequired KeyCombo
  | KeyCombo String (Array Char)
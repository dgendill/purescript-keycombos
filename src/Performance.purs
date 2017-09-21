module KeyCombo.Performance (
  now
) where

import Control.Monad.Eff (Eff)

foreign import now :: forall e. Eff e Number

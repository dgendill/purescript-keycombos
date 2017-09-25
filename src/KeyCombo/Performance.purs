module KeyCombo.Performance (
  now,
  now',
  TimeStamp(..)
) where

import Control.Monad.Eff (Eff)
import Data.Newtype (class Newtype)
import Prelude ((<$>))

newtype TimeStamp = TimeStamp Number
derive instance newtypeKCTimeStamp :: Newtype TimeStamp _

foreign import nowImpl :: forall e. Eff e Number

now :: forall e. Eff e Number
now = nowImpl

now' :: forall e. Eff e TimeStamp
now' = TimeStamp <$> now

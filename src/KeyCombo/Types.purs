module KeyCombo.Types where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import Data.Newtype (class Newtype)
import Data.StrMap (StrMap)
import Data.Tuple (Tuple)
import Prelude (class Show, Unit, show)

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

-- | General purpose type for time intervals
newtype Interval = Interval Number
derive instance newtypeKCInterval :: Newtype Interval _

-- | Type for millisecond intervals
newtype MillisecondInterval = MillisecondInterval Number
derive instance newtypeKCMSInterval :: Newtype MillisecondInterval _


-- | Event handler for one key being pressed
newtype OnKeyDown e = OnKeyDown (String -> KeyState -> Aff (Effects e) Unit)

-- | Event handler for one key being released
newtype OnKeyUp e = OnKeyUp (String -> KeyState -> Aff (Effects e) Unit)

-- | Event handler for multiple keys being
-- | released all at approximatly the same time
newtype OnComboRelease e = OnComboRelease (StrMap Number -> Aff (Effects e) Unit)

-- | A set of event handlers that will trigger
-- | when the specified characters are released
newtype OnExactRelease e = OnExactRelease (Array (Tuple (Array String) (Unit -> Aff (Effects e) Unit)))

type Effects e = (ref :: REF, avar :: AVAR, console :: CONSOLE, dom :: DOM | e)
type KeyComboM e = Aff (Effects e)
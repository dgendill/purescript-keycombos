module KeyCombo.FFI (
    getFunctionArgs,
    getArgumentCount,
    hasReqKeys,
    hasReqKeysUser,
    arrayToTuple,
    lengthGt,
    isArray,
    hasAtLeastOneOf,
    mutateObject,
    objectKeyEquals,
    setDefaultKeys,
    specifyNoun,
    allObjectPairs
  ) where

import Prelude

import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (throwError)
import Data.Array (foldMap, foldl, init, last, singleton, tail, unsafeIndex)
import Data.Bifunctor (lmap)
import Data.Const (Const(..))
import Data.Foldable (length, surround)
import Data.Foreign (Foreign)
import Data.Foreign as Foreign
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (unwrap)
import Data.String (joinWith, trim)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, invalid, isValid)
import Type.Equality (class TypeEquals)
import Type.Row (Cons, Nil, RProxy(..), kind RowList)
import Unsafe.Coerce (unsafeCoerce)

foreign import getFunctionArgsImpl :: Foreign -> Array String
foreign import getArgumentCountImpl :: Foreign -> Int
foreign import getMissingRequiredKeysImpl :: Foreign -> Array String -> Array String
foreign import mutateObjectImpl :: forall a b. Foreign -> Array String -> (a -> b) -> Foreign
foreign import objectKeyEqualsImpl :: forall a. Foreign -> String -> a -> Boolean
foreign import setObjectKeyValueImpl :: forall a. Foreign -> String -> a -> Foreign
foreign import allObjectPairsImpl :: Foreign -> Array String -> (Fn2 String Foreign Boolean) -> Boolean
foreign import isFunctionImpl :: Foreign -> Boolean

allObjectPairs :: Foreign -> Array String -> (String -> Foreign -> Boolean) -> V (Array String) Boolean
allObjectPairs f keys fn = unwrap <$> (foldMap Conj) <$> traverse (\key ->
    if allObjectPairsImpl f [key] (mkFn2 fn)
      then pure true
      else invalid $ [key]
  ) keys

allObjectPairsUser :: Foreign -> Array String -> String -> (String -> Foreign -> Boolean) -> V (Array Error) Boolean
allObjectPairsUser f keys reason fn = allObjectPairs f keys fn # (lmap \keys -> [
  error $ senta $ "'s " <> (quoteAndSeperate keys) <> " " <> reason
])

isFunction :: Foreign -> V (Array String) Foreign
isFunction f = case isFunctionImpl f of
  true -> pure f
  false -> invalid ["is not a function."]

getFunctionArgs :: Foreign -> Array String
getFunctionArgs = getFunctionArgsImpl

getArgumentCount :: Foreign -> Int
getArgumentCount = getArgumentCountImpl

hasReqKeys :: forall a. Foreign -> Array String -> V (Array String) Unit
hasReqKeys f keys =
  case getMissingRequiredKeysImpl f keys of
    [] -> pure unit
    a -> invalid a
    
hasReqKeysUser :: forall a. Foreign -> Array String -> V (Array Error) Unit
hasReqKeysUser f a = hasReqKeys f a # (lmap \keys -> [
  error $ "is missing keys " <> quoteAndSeperate keys
])

hasAtLeastOneOf :: forall a. Foreign -> Array String -> V (Array String) Unit
hasAtLeastOneOf f keys =
  case getMissingRequiredKeysImpl f keys of
    [] -> pure unit
    a -> if ((length a :: Int) == (length keys :: Int))
           then invalid $ [senta $
              "must have at least one of the following keys: "
              <> quoteAndSeperate keys
            ]
           else pure unit

mutateObject :: forall a b. Partial => Foreign -> Array String -> (a -> b) -> V (Array String) Foreign
mutateObject f keys fn = pure $ mutateObjectImpl f keys fn

setObjectKeyValue :: forall a b. Partial => Foreign -> String -> a -> Foreign
setObjectKeyValue f key value = setObjectKeyValueImpl f key value

setDefaultKeys :: forall a. Partial => Foreign -> Array (Tuple String a) -> V (Array String) Foreign
setDefaultKeys f keys = pure $ foldl (\ff (Tuple key val) ->
    if not isValid $ hasAtLeastOneOf ff [key]
      then setObjectKeyValue ff key val
      else ff
  ) f keys

mapLast :: forall a b. (a -> a) -> Array a -> Array a
mapLast  fn a = case length a > 1 of
  true -> fromMaybe [] do
    i <- init a
    l <- last a
    pure $ i <> [fn l]
  false -> a

quoteAndSeperate :: Array String -> String
quoteAndSeperate =
  joinWith ", "
  <<< mapLast (append "and ")
  <<< map (\v -> "'" <> v <> "'")

senta :: String -> String
senta s = s <> "."

isArray :: Foreign -> V (Array String) Foreign
isArray f = case Foreign.isArray f of
  true -> pure f
  false -> invalid ["is not an array."]

specifyNoun :: String -> Array String -> Array String
specifyNoun noun messages = map (\message -> (trim noun) <> " " <> message) messages

lengthGt :: Partial => Int -> Foreign -> V (Array String) Foreign
lengthGt l arr = case (length (unsafeCoerce arr :: forall a. Array a)) > l of
  true -> pure arr
  false -> invalid $ [senta $ "Array length is not greater than " <> (show l)]

arrayToTuple :: forall a. Partial => Foreign -> Tuple a a
arrayToTuple arr =
  let array = unsafeCoerce arr
  in Tuple (unsafeIndex array 0) (unsafeIndex array 1)

objectKeyEquals :: forall a. Partial => Foreign -> String -> a -> Boolean
objectKeyEquals f key value = objectKeyEqualsImpl f key value

-- foreign import data Audit :: # Auditor -> Type -> Type

-- foreign import kind Auditor

-- instance applicativeAudited :: Applicative (Audited e) where
--   pure = id

-- foreign import unwrapAudited :: forall e. Audited e a -> a

-- data IsArray
-- data Length (s :: Symbol)

-- | Type alias for a validated string and its rules
-- type ForeignAuditor (insights :: # Type) a = Const a (RProxy insights)
-- type ForeignAuditor (insights :: # Type) a = Const a (RProxy insights)

-- class Audit rule a where
--   auditImpl :: Proxy insight -> a -> a

-- foreign import lengthGTImpl :: Int -> Array 

-- lengthGT :: forall a sym r r1 r2.
--   (IsSymbol sym)
--     -- ( RowCons sym Int r1 r2
--     -- , IsSymbol sym)
--   => Int
--   -> ForeignAuditor { isArray :: IsArray | r1 } (Array a)
--   -> ForeignAuditor { isArray :: IsArray, hasLength :: (Length sym) | r1 } (Array a)
-- lengthGT i arr = case length (unwrap arr) > i of
--   false -> throwError $ senta $ "Array length is not greater than " <> (show i)
--   true -> unsafeCoerce $ pure arr

-- -- arrayToTuple :: forall a. FFI { validated :: Validated } Foreign -> Tuple a a
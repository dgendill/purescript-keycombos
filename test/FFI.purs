module Test.FFI where

import KeyCombo.FFI
import Prelude

import Control.Monad.Aff.Console (log)
import Data.Foreign (toForeign)
import Data.Ord (lessThan)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, invalid, isValid, unV)
import Partial.Unsafe (unsafePartial)
import Test.Unit (suite, test, timeout)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Unsafe.Coerce (unsafeCoerce)

runV v fn = unV (const false) id (v <#> fn)

main = runTest do
  suite "FFI" do
    test "hasReqKeys" do
      let r1 = hasReqKeys (toForeign {
            "a":1,
            "b":{"c":{"d":1},"e":1},
            "c":1
          }) ["b.c.d","a","b.e"]
      Assert.assert "should detect all required object keys" $ (isValid r1)

      let r2 = hasReqKeys (toForeign {
            "a":1, "c": {}
          }) ["c", "c.f","a","c.e"]
      Assert.assert "should detect missing object keys" $ (r2 == invalid ["c.f","c.e"])

      let r3 = hasReqKeys (toForeign 1) []
      Assert.assert "should not fail with foreign 1" $ (isValid r3)

      let r4 = hasReqKeys (toForeign "abcd") ["3"]
      Assert.assert "should not fail with key 3, foreign 'abcd'" $ (isValid r4)

      let r5 = hasReqKeys (toForeign "abcd") ["4"]
      Assert.assert "should fail with key 4, foreign 'abcd'" $ (r5 == invalid ["4"])

    test "hasAtLeastOneOf" do
      let r1 = hasAtLeastOneOf (toForeign {
            "a":1,
            "b":{"c":1},
            "c":1
          }) ["b.c","a","c"]
      Assert.assert "should detect at least one key" $ (isValid r1)

      let r2 = hasAtLeastOneOf (toForeign {
            "a":1,
            "b":{"c":1},
            "c":1
          }) ["d","e","f"]
      Assert.assert "should fail when at least one key not found" $ (not isValid r2)

    test "isArray" do
      let t = isArray (toForeign [1,2])
      Assert.assert "isArray should validate an array" $ (isValid t)
    
    test "lengthGt" do
      let t1 = unsafePartial $ lengthGt 1 (toForeign [1,2])
      let t2 = unsafePartial $ lengthGt 1 (toForeign [1])
      Assert.assert "lengthGt should validate the lenght of an array" $ (isValid t1)
      Assert.assert "lengthGt should validate the lenght of an array" $ (not isValid t2)


    test "arrayToTuple" do
      let t = unsafePartial $ arrayToTuple (toForeign [1,2])
      Assert.assert "should convert an array who's length is > 2 to a tuple" $ (t == Tuple 1 2)

    test "mapObject" do
      let tt = unsafePartial $ mutateObject (toForeign {
            "a":1,
            "b":2,
            "c":3,
            "d":4
          }) ["a","b","c"] (add 1)

      Assert.assert "should mutate certain object keys" $
        runV tt \t ->
          (unsafePartial $ objectKeyEquals t "a" 2) &&
          (unsafePartial $ objectKeyEquals t "b" 3) &&
          (unsafePartial $ objectKeyEquals t "c" 4) &&
          (unsafePartial $ objectKeyEquals t "d" 4)

    test "setDefaultKeys" do
      let tt = unsafePartial $ setDefaultKeys (toForeign {
            "a":1,
            "d":4
          }) [
            Tuple "a" 2,
            Tuple "b" 2,
            Tuple "c" 3
          ]

      Assert.assert "should set object key if they're not already set" $
        runV tt \t ->
          (unsafePartial $ objectKeyEquals t "a" 1) &&
          (unsafePartial $ objectKeyEquals t "b" 2) &&
          (unsafePartial $ objectKeyEquals t "c" 3) &&
          (unsafePartial $ objectKeyEquals t "d" 4)
      
    test "allObjectPairs" do
    
      let tt = unsafePartial $ allObjectPairs (toForeign {
            "a":1,
            "b":4,
            "c":10
          }) ["a","b"] (\key f -> lessThan (unsafeCoerce f) 10)

      Assert.assert "should should test if certain pairs in an object pass a test" $
        runV tt (\t -> true)
        
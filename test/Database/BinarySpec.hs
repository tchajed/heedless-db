module Database.BinarySpec (spec) where

import Data.Serialize.Get
import Data.Serialize.Put
import Database.Binary
import Test.Hspec
import qualified Data.ByteString as BS

roundtrip :: (Show a, Eq a) => Putter a -> Get a -> a -> Expectation
roundtrip p g x = do
  case runGet g (runPut (p x)) of
    Left e -> error e
    Right y -> y `shouldBe` x

spec :: Spec
spec = do
  describe "binary encoding" $ do
    it "should encode/decode array16" $
      let test = roundtrip putArray16 getArray16 in
        do
          test "foo"
          test ""
          test "xxxxxxxx"
          test (BS.replicate 65535 42)
    it "should encode/decode repeated elements" $
      let test = roundtrip (mapM_ putWord32le) (getRepeated getWord32le) in
        do
          test []
          test [32, 45, 12]

{-# LANGUAGE Rank2Types #-}
module Database.Filesys.MemFsSpec (spec) where

import Test.Hspec
import Database.Filesys
import qualified Database.Filesys.MemFs as MemFs

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

shouldProduce :: (Show a, Eq a) => (forall s. MemFs.M s a) -> a -> Expectation
shouldProduce p x =
  MemFs.run p `shouldBe` x

makeFoo :: MemFs.M s (File (MemFs.M s))
makeFoo = do
  f <- create "foo"
  append f "hello"
  append f " world"
  return f

spec :: Spec
spec = do
  describe "memfs" $ do
    it "should read created files" $ do
      (do
          makeFoo >>= close
          f <- open "foo"
          readAll f) `shouldProduce` "hello world"
    it "should truncate files" $ do
      (do
          f <- makeFoo
          ftruncate f
          close f
          f <- open "foo"
          readAll f) `shouldProduce` ""
    it "should delete files" $ do
      (do
          makeFoo >>= close
          firstListing <- list
          delete "foo"
          secondListing <- list
          return (firstListing, secondListing)) `shouldProduce` (["foo"], [])

module Database.Filesys.MemFsSpec (spec) where

import           Control.Monad.Reader (MonadReader, MonadIO)
import           Database.Filesys
import qualified Database.Filesys.MemFs as MemFs
import           Database.Filesys.Utils
import           Test.Hspec

makeFoo :: (MonadReader env m, MemFs.HasFilesysCache env, MonadIO m) => m FilePath
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

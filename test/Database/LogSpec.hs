module Database.LogSpec (spec) where

import           Database.Filesys
import           Database.Filesys.Utils
import qualified Database.Log as Log
import           Test.Hspec

spec :: Spec
spec = do
  describe "logging impl" $ do
    it "should support an empty file" $ do
      (do
          log <- create "log"
          close log
          Log.recoverTxns log) `shouldProduce` []
    it "should recover entries" $ do
      (do
          log <- create "log"
          Log.add log "txn 1"
          Log.add log "txn 2"
          Log.add log "txn 3"
          close log
          Log.recoverTxns log) `shouldProduce` ["txn 1", "txn 2", "txn 3"]

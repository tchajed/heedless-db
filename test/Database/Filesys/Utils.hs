{-# LANGUAGE Rank2Types #-}

module Database.Filesys.Utils where

import qualified Database.Filesys.MemFs as MemFs
import           Test.Hspec

shouldProduce :: (Show a, Eq a) => MemFs.M a -> a -> Expectation
shouldProduce p x =
  MemFs.run p `shouldReturn` x

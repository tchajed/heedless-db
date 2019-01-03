{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}

module Database.Filesys.Utils where

import           Control.Monad.Reader
import Database.Filesys.MemFs
import           Test.Hspec

newtype Env = Env {filesysCache_ :: FilesysCache }

instance HasFilesysCache Env where
  filesysCache = filesysCache_

newtype M a = M (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO)

shouldProduce :: (Show a, Eq a) => M a -> a -> Expectation
shouldProduce (M p) x = do
  c <- newCache
  runReaderT p (Env c) `shouldReturn` x

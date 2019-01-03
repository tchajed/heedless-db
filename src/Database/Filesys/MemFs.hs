{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Database.Filesys.MemFs
  ( M
  , run
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader ( ReaderT
                                       , MonadReader
                                       , runReaderT
                                       , ask)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.HashTable.IO as H
import           Database.Filesys

{-# ANN module ("HLint: ignore Use &&&" :: String) #-}

type HashTable k v = H.BasicHashTable k v
newtype FilesysState = FilesysState (HashTable FName ByteString)

newtype M a = M (ReaderT FilesysState IO a)
  deriving (Functor, Applicative, Monad, MonadReader FilesysState, MonadIO)

run :: M a -> IO a
run (M p) = do
  ht <- H.new
  runReaderT p (FilesysState ht)

withFs :: (HashTable FName ByteString -> IO a) -> M a
withFs act = do
  FilesysState ht <- ask
  liftIO $ act ht

withExistingFile :: FName -> (ByteString -> (Maybe ByteString, a)) -> M a
withExistingFile name f = withFs $ \ht -> H.mutate ht name op
  where op (Just contents) = f contents
        op Nothing = error $ "use of file " ++ name ++ " that does not exist"

instance FilesysLayer FName M where
  open = return
  list = withFs $ \ht -> H.foldM (\l (k, _) -> return $ k:l) [] ht
  size f = withExistingFile f $ \bs ->
      (Just bs, BS.length bs)
  readAt f off length = withExistingFile f $ \bs ->
      (Just bs, BS.take length . BS.drop off $ bs)
  atomicCreate f bs = withFs $ \ht -> H.insert ht f bs
  create f = withFs $ \ht -> H.insert ht f BS.empty >> return f
  append f bs = withExistingFile f $ \contents ->
      (Just (BS.append contents bs), ())
  delete f = withFs $ \ht ->
    H.delete ht f
  ftruncate f = withExistingFile f $ const (Just BS.empty, ())
  close _f = return ()

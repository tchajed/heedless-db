{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Database.Filesys.MemFs
  ( M
  , run
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader ( ReaderT
                                       , MonadReader
                                       , runReaderT
                                       , ask)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.HashTable.IO as H
import           Database.Filesys
import qualified Database.FilesysObject as FS

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}
{-# ANN module ("HLint: ignore Use &&&" :: String) #-}

type HashTable k v = H.BasicHashTable k v
newtype FilesysState = FilesysState (HashTable FName ByteString)

newtype M a = M (ReaderT FilesysState IO a)
  deriving (Functor, Applicative, Monad, MonadReader FilesysState, MonadIO)

run :: M a -> IO a
run p = do
  ht <- H.new
  let M r = p in
    runReaderT r (FilesysState ht)

withExistingFile :: HashTable FName ByteString -> FName -> (ByteString -> (Maybe ByteString, a)) -> IO a
withExistingFile ht name f = H.mutate ht name op
  where op (Just contents) = f contents
        op Nothing = error $ "use of file " ++ name ++ " that does not exist"

instance MonadFilesys M FName where
  getFs = ask >>= \(FilesysState ht) -> return FS.Filesys
    { FS.open = return
    , FS.list = H.foldM (\l (k, _) -> return $ k:l) [] ht
    , FS.size = \f -> withExistingFile ht f $ \bs ->
        (Just bs, BS.length bs)
    , FS.readAt = \f off length -> withExistingFile ht f $ \bs ->
        (Just bs, BS.take length . BS.drop off $ bs)
    , FS.atomicCreate = \f bs -> H.insert ht f bs
    , FS.create = \f -> H.insert ht f BS.empty >> return f
    , FS.append = \f bs -> withExistingFile ht f $ \contents ->
        (Just (BS.append contents bs), ())
    , FS.delete = \f -> H.delete ht f
    , FS.ftruncate = \f -> withExistingFile ht f $
      const (Just BS.empty, ())
    , FS.close = \_ -> return () }

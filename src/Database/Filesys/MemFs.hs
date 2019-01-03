{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Filesys.MemFs
  ( HasFilesysCache(..)
  , FilesysCache(..)
  , newCache
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader ( MonadReader
                                      , reader)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.HashTable.IO as H
import           Database.Filesys

{-# ANN module ("HLint: ignore Use &&&" :: String) #-}

type HashTable k v = H.BasicHashTable k v
newtype FilesysCache = FilesysCache (HashTable FName ByteString)

class HasFilesysCache env where
  filesysCache :: env -> FilesysCache

newCache :: IO FilesysCache
newCache = FilesysCache <$> H.new

withFs :: (MonadReader env m, HasFilesysCache env, MonadIO m) =>
          (HashTable FName ByteString -> IO a) -> m a
withFs act = do
  FilesysCache ht <- reader filesysCache
  liftIO $ act ht

withExistingFile :: (MonadReader env m, HasFilesysCache env, MonadIO m) =>
                    FName -> (ByteString -> (Maybe ByteString, a)) -> m a
withExistingFile name f = withFs $ \ht -> H.mutate ht name op
  where op (Just contents) = f contents
        op Nothing = error $ "use of file " ++ name ++ " that does not exist"

instance (MonadReader env m, HasFilesysCache env, MonadIO m) => FilesysLayer FName m where
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

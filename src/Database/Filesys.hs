{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Database.Filesys where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.ByteString (ByteString)
import qualified Database.FilesysObject as FS

type FName = FS.FName

class MonadIO m => MonadFilesys m fh | m -> fh where
  getFs :: m (FS.Filesys fh)

lift1 :: MonadFilesys m fh =>
  (FS.Filesys fh -> a -> IO r) ->
  a -> m r
lift1 f x = getFs >>= \fs -> liftIO $ f fs x

lift2 :: MonadFilesys m fh =>
  (FS.Filesys fh -> a -> b -> IO r) ->
  a -> b -> m r
lift2 f x y = getFs >>= \fs -> liftIO $ f fs x y

lift3 :: MonadFilesys m fh =>
  (FS.Filesys fh -> a -> b -> c -> IO r) ->
  a -> b -> c -> m r
lift3 f x y z = getFs >>= \fs -> liftIO $ f fs x y z

open :: MonadFilesys m fh => FName -> m fh
open = lift1 FS.open

list :: MonadFilesys m fh => m [FName]
list = getFs >>= liftIO . FS.list

size :: MonadFilesys m fh => fh -> m Int
size = lift1 FS.size

readAt :: MonadFilesys m fh => fh -> Int -> Int -> m ByteString
readAt = lift3 FS.readAt

create :: MonadFilesys m fh => FName -> m fh
create = lift1 FS.create

append :: MonadFilesys m fh => fh -> ByteString -> m ()
append = lift2 FS.append

delete :: MonadFilesys m fh => FName -> m ()
delete = lift1 FS.delete

ftruncate :: MonadFilesys m fh => FName -> m ()
ftruncate = lift1 FS.ftruncate

atomicCreate :: MonadFilesys m fh => FName -> ByteString -> m ()
atomicCreate = lift2 FS.atomicCreate

close :: MonadFilesys m fh => fh -> m ()
close = lift1 FS.close

readAll :: MonadFilesys m fh => fh -> m ByteString
readAll f = do
  sz <- size f
  -- TODO: do this in max-sized chunks
  readAt f 0 sz

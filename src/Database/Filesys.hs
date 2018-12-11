{-# LANGUAGE TypeFamilies #-}
module Database.Filesys where

import Data.ByteString (ByteString)

-- A filename (should not contain any path separators).
type FName = String

-- A MonadFilesys is an abstraction of the filesystem exposing a single
-- directory and only the APIs used by the database.
class Monad m => MonadFilesys m where
  type File m :: *
  -- reading
  open :: FName -> m (File m)
  list :: m [FName]
  size :: File m -> m Int
  -- readAt offset length
  readAt :: File m -> Int -> Int -> m ByteString

  -- modifying
  create :: FName -> m (File m)
  append :: File m -> ByteString -> m ()
  delete :: FName -> m ()
  truncate :: FName -> m ()
  atomicCreate :: FName -> ByteString -> m ()

  close :: File m -> m ()

readAll :: MonadFilesys m => File m -> m ByteString
readAll f = do
  sz <- size f
  -- TODO: do this in max-sized chunks
  readAt f 0 sz

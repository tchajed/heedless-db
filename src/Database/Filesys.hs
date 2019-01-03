{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Database.Filesys where

import Data.ByteString (ByteString)

-- A filename (should not contain any path separators).
type FName = String

-- A FilesysLayer exposes filesystem APIs used by the database.
--
-- The fh type parameter is the internal type of filesystem handles.
class Monad m => FilesysLayer fh m | m -> fh where
  open :: FName -> m fh
  close :: fh -> m ()

  -- reading
  list :: m [FName]
  size :: fh -> m Int
  -- readAt fh offset length
  readAt :: fh -> Int -> Int -> m ByteString

  -- modifying
  create :: FName -> m fh
  append :: fh -> ByteString -> m ()
  delete :: FName -> m ()
  ftruncate :: FName -> m ()
  atomicCreate :: FName -> ByteString -> m ()

readAll ::  FilesysLayer fh m => fh -> m ByteString
readAll f = do
  sz <- size f
  -- TODO: do this in max-sized chunks
  readAt f 0 sz

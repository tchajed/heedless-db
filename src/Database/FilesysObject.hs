module Database.FilesysObject where

import Data.ByteString (ByteString)

-- A filename (should not contain any path separators).
type FName = String

-- A Filesys is an abstraction of the filesystem exposing a single directory and
-- only the APIs used by the database.
data Filesys fh = Filesys {
  -- reading
  open :: FName -> IO fh
  , list :: IO [FName]
  , size :: fh -> IO Int
  -- readAt offset length
  , readAt :: fh -> Int -> Int -> IO ByteString

  -- modifying
  , create :: FName -> IO fh
  , append :: fh -> ByteString -> IO ()
  , delete :: FName -> IO ()
  , ftruncate :: FName -> IO ()
  , atomicCreate :: FName -> ByteString -> IO ()

  , close :: fh -> IO ()
  }

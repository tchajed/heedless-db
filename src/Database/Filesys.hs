module Database.Filesys where

import qualified Data.ByteString as BS
import qualified System.Posix.Types

-- A filename (should not contain any path separators).
type FName = String
-- A File is a handle to an open file.
type File = System.Posix.Types.Fd

-- Filesys is an abstraction of the filesystem exposing a single directory and
-- only the APIs used by the database.
data Filesys = Filesys {
    -- reading
    open :: FName -> IO File
  , list :: IO [FName]
  , size :: File -> IO Int
  , readAt :: Int -> Int -> IO BS.ByteString

    -- modifying
  , create :: FName -> IO File
  , append :: File -> BS.ByteString -> IO ()
  , delete :: FName -> IO ()
  , truncate :: FName -> IO ()
  , atomicCreate :: FName -> BS.ByteString -> IO ()
  }

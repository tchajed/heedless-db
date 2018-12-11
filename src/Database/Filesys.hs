module Database.Filesys where

import qualified Data.ByteString as BS

type FName = String

-- fh is the type of filesystem handles (System.Posix.Types.Fd or GHC.IO.Handle,
-- depending on whether we use package unix or built-in Haskell file operations)
data Filesys fh = Filesys {
    -- reading
    open :: FName -> IO fh
  , list :: IO [FName]
  , size :: fh -> IO Int
  , readAt :: Int -> Int -> IO BS.ByteString

    -- modifying
  , create :: FName -> IO fh
  , append :: fh -> BS.ByteString -> IO ()
  , delete :: FName -> IO ()
  , truncate :: FName -> IO ()
  , atomicCreate :: FName -> BS.ByteString -> IO ()
  }

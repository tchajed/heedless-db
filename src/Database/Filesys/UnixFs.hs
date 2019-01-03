{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Database.Filesys.UnixFs
  ( M
  , run ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader ( ReaderT
                            , MonadReader
                            , runReaderT
                            , ask)
import Control.Monad (when)
import Database.Filesys
import System.Directory (listDirectory, removeFile)
import System.FilePath.Posix (joinPath)
import System.Posix.Files (getFdStatus, fileSize, setFileSize, rename)
import System.Posix.IO ( openFd,  OpenMode(..)
                       , closeFd)
import "unix-bytestring" System.Posix.IO.ByteString (fdPread, fdWrite)
import qualified System.Posix.IO as PosixIO
import System.Posix.Types (Fd)
import qualified Data.ByteString as BS

newtype M a = M (ReaderT FilePath IO a)
  deriving (Functor, Applicative, Monad, MonadReader FilePath, MonadIO)

withRoot :: (FilePath -> IO a) -> M a
withRoot act = do
  root <- ask
  liftIO $ act root

run :: FilePath -> M a -> IO a
run root (M act) = runReaderT act root

join :: FilePath -> FilePath -> FilePath
join p1 p2 = joinPath [p1, p2]

instance FilesysLayer Fd M where
  open f = let perms = Nothing
               mode = PosixIO.defaultFileFlags in
           withRoot $ \root -> openFd (root `join` f) ReadOnly perms mode
  create f = let perms = Just 0644
                 mode = PosixIO.defaultFileFlags {PosixIO.append=True} in
             withRoot $ \root -> openFd (root `join` f) ReadWrite perms mode
  list = withRoot $ \root -> listDirectory root
  size f = liftIO $ do
    s <- getFdStatus f
    return $ fromIntegral . fileSize $ s
  close f = liftIO $ closeFd f
  delete f = withRoot $ \root -> removeFile (root `join` f)
  ftruncate f = liftIO $ setFileSize f 0
  readAt f off len = liftIO $ fdPread f (fromIntegral len) (fromIntegral off)
  append f bs = liftIO $ do
    count <- fdWrite f bs
    when (fromIntegral count < BS.length bs) $ error "short read"
  atomicCreate f bs = withRoot $ \root -> do
    let dstFile = root `join` f
    let tmpFile = dstFile ++ ".tmp"
    BS.writeFile tmpFile bs
    rename tmpFile dstFile

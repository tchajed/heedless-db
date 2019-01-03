{-# LANGUAGE PackageImports #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- we're apparently doing something odd with typeclasses
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Filesys.UnixFs
  ( HasFilesysRoot(..)
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader ( MonadReader
                            , reader )
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

class HasFilesysRoot env where
  filesysRoot :: env -> FilePath

withRoot :: (MonadReader env m, HasFilesysRoot env, MonadIO m) =>
            (FilePath -> IO a) -> m a
withRoot act = do
  root <- reader filesysRoot
  liftIO $ act root

resolvePath :: (MonadReader env m, HasFilesysRoot env, MonadIO m) =>
               (FilePath -> IO a) -> FilePath -> m a
resolvePath act f = withRoot $ \root -> act (joinPath [root, f])

instance (MonadIO m, MonadReader env m, HasFilesysRoot env) => FilesysLayer Fd m where
  open = let perms = Nothing
             mode = PosixIO.defaultFileFlags in
           resolvePath $ \f -> openFd f ReadOnly perms mode
  create = let perms = Just 0644
               mode = PosixIO.defaultFileFlags {PosixIO.append=True} in
             resolvePath $ \f -> openFd f ReadWrite perms mode
  list = withRoot $ \root -> listDirectory root
  size f = liftIO $ do
    s <- getFdStatus f
    return $ fromIntegral . fileSize $ s
  close f = liftIO $ closeFd f
  delete = resolvePath $ \f -> removeFile f
  ftruncate f = liftIO $ setFileSize f 0
  readAt f off len = liftIO $ fdPread f (fromIntegral len) (fromIntegral off)
  append f bs = liftIO $ do
    count <- fdWrite f bs
    when (fromIntegral count < BS.length bs) $ error "short read"
  atomicCreate f bs = withRoot $ \root -> do
    let dstFile = joinPath [root, f]
    let tmpFile = dstFile ++ ".tmp"
    BS.writeFile tmpFile bs
    rename tmpFile dstFile

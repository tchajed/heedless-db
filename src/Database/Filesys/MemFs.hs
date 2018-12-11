{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Database.Filesys.MemFs
  ( M
  , run
  ) where

import           Control.Monad.Reader (ReaderT,
                                      runReaderT,
                                      ask,
                                      lift)
import           Control.Monad.ST
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.HashTable.ST.Basic as H
import           Database.Filesys

type HashTable s k v = H.HashTable s k v
type FilesysState s = HashTable s FName ByteString

type FsMonad s a = ReaderT (FilesysState s) (ST s) a
newtype M s a = M (FsMonad s a) deriving (Functor, Applicative, Monad)

getReader :: M s a -> FsMonad s a
getReader (M p) = p

run :: (forall s. M s a) -> a
run p = runST $ do
  ht <- H.new
  runReaderT (getReader p) ht

withTable :: (FilesysState s -> ST s a) -> M s a
withTable f = M $ ask >>= (lift . f)

withExistingFile :: FName -> (ByteString -> (Maybe ByteString, a)) -> M s a
withExistingFile name f = withTable $ \ht ->
  H.mutate ht name op
  where op (Just contents) = f contents
        op Nothing = error $ "use of file " ++ name ++ " that does not exist"

instance MonadFilesys (M s) where
  type File (M s) = FName
  open = return
  list = withTable $
    H.foldM (\l (k, _) -> return $ k:l) []
  size f = withExistingFile f $ \bs ->
    (Just bs, BS.length bs)
  readAt f off length = withExistingFile f $ \bs ->
    (Just bs, BS.take length . BS.drop off $ bs)

  atomicCreate f bs = withTable $ \ht -> H.insert ht f bs
  create f = atomicCreate f BS.empty >> return f
  append f bs = withExistingFile f $ \contents ->
    (Just (BS.append contents bs), ())
  delete f = withTable $ \ht ->
    H.delete ht f
  truncate f = withExistingFile f $
    const (Just BS.empty, ())
  close _ = return ()

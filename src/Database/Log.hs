module Database.Log
  ( add
  , recoverTxns
  ) where

import           Data.ByteString (ByteString)
import           Data.Serialize.Get
import           Data.Serialize.Put
import           Database.Binary
import           Database.Filesys

data Record =
  DataRecord ByteString
  | CommitRecord
  deriving (Eq, Show)

putRecord :: Putter Record
putRecord (DataRecord bs) = putWord8 1 <> putArray16 bs
putRecord CommitRecord = putWord8 2

putEntry :: Putter ByteString
putEntry bs = putRecord (DataRecord bs) <> putRecord CommitRecord

getRecord :: Get Record
getRecord = do
  tag <- getWord8
  case tag of
    1 -> DataRecord <$> getArray16
    2 -> return CommitRecord
    _ -> fail "unknown tag"

getEntry :: Get ByteString
getEntry = do
  r <- getRecord
  case r of
    DataRecord bs -> do
      r <- getRecord
      if r == CommitRecord then return bs
        else fail "expected commit record"
    CommitRecord -> fail "expected data record"

add :: FilesysLayer fh m => fh -> ByteString -> m ()
add f bs =
  let d = runPut (putEntry bs) in
  append f d

recoverTxns :: FilesysLayer fh m => fh -> m [ByteString]
recoverTxns f = do
  d <- readAll f
  -- TODO: this should tolerate partial writes
  return $ runGetSuccess (getRepeated getEntry) d

module Database.Binary where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Serialize.Get
import           Data.Serialize.Put

putArray16 :: Putter ByteString
putArray16 bs =
  putWord16le (fromIntegral . BS.length $ bs)
  <> putByteString bs

getArray16 :: Get ByteString
getArray16 = do
  len <- getWord16le
  getBytes (fromIntegral len)

getRepeated :: Get a -> Get [a]
getRepeated g = do
  b <- isEmpty
  if b
    then return []
    else pure (:) <*> g <*> getRepeated g

runGetSuccess :: Get a -> ByteString -> a
runGetSuccess g bs =
  case runGet g bs of
    Left err -> error ("parse failed: " ++ err)
    Right x -> x

module Database.Interface where

import qualified Data.ByteString as BS
import Data.Word

newtype Key = Key Word64
newtype Value = Value BS.ByteString

data Entry = Entry Key Value

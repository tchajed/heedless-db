{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Interface where

import qualified Data.ByteString as BS
import           Data.Hashable
import           Data.Word

newtype Key = Key Word64
  deriving (Eq, Show, Hashable)
newtype Value = Value BS.ByteString
  deriving (Eq, Show)

data Entry = Entry Key Value

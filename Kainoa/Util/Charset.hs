module Kainoa.Util.Charset
( utf8ToLatin1
, latin1ToUtf8
) where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Codec.Text.IConv (convert)

{-
  USE:
    query = utf8ToLatin1 query
    BL.putStrLn query
-}

utf8ToLatin1 :: String -> BL.ByteString
utf8ToLatin1 = utf8ToLatin1' . pack

latin1ToUtf8 :: String -> BL.ByteString
latin1ToUtf8 = latin1ToUtf8' . pack

{-
  Completely Lazy versions.
-}

type Converter = BL.ByteString -> BL.ByteString

utf8ToLatin1' :: Converter
utf8ToLatin1' = convert "UTF8" "LATIN1"
    
latin1ToUtf8' :: Converter
latin1ToUtf8' = convert "LATIN1" "UTF8"
    

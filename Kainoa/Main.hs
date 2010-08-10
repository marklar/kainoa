module Main where

import System.Environment

import Kainoa.Types
import Kainoa.Util.Charset (utf8ToLatin1)
import Kainoa.Domain (openDomain)
import Kainoa.Search (search)

main = do
  lexemes <- getArgs
  let queryStr = unwords lexemes
  domains <- mapM (openDomain . fullDirName) domainNames
  putStrLn . show $ search domains queryStr

domainNames :: [String]
domainNames = ["browse", "games", "music"] -- ["videos", "books"]

rootIdxDir :: FilePath
rootIdxDir = "idx"

fullDirName :: String -> FilePath
fullDirName domain = rootIdxDir ++ "/" ++ domain

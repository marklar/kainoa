{- |

Kainoa is a cmd-line search engine.

A search engine typically has two different types of processes:
  + an indexer
  + a searcher

Kainoa is a reimplementation of just the latter -- the search part of
a search engine.  (The indexing code hasn't been ported over.)

Unlike the OCaml implementation on which it's based, Kainoa does not
run as an HTTP service.  It's merely a command-line tool.  You pass it
a query string on the command line, and it outputs its search results
to stdout.

-}
module Main where

import System.Environment

import Kainoa.Types
import Kainoa.Util.Charset (utf8ToLatin1)
import Kainoa.Domain (openDomain)
import Kainoa.Search (search)

main = do
  -- Get the queryStr from the cmd line.
  lexemes <- getArgs
  let queryStr = unwords lexemes

  -- Mmap the search index files.  (Each set of files is a 'domain'.)
  domains <- mapM (openDomain . fullDirName) domainNames

  -- Perform the search over each domain.
  putStrLn . show $ search domains queryStr

domainNames :: [String]
domainNames = ["browse", "games", "music"] -- ["videos", "books"]

rootIdxDir :: FilePath
rootIdxDir = "idx"

fullDirName :: String -> FilePath
fullDirName domain = rootIdxDir ++ "/" ++ domain

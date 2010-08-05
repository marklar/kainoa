module Main where

import qualified Data.ByteString.Lazy as BL
import System.Environment

import Kainoa.Domain (makeDomain, getText, getTargets, getResultsForTarget)
import Kainoa.Types

main = do
  [idStr] <- getArgs
  let id = read idStr :: Int

  domain <- makeDomain "idx/games"

  putStrLn "text:"
  case getText domain id of
    Just s  -> BL.putStrLn s
    Nothing -> putStrLn "not found"

  putStrLn "\ntargets:"
  let targetIds = getTargets domain id
  putStrLn $ show targetIds

  putStrLn "\nall results with these targets:"
  mapM_ (putStrLn . show . getResultsForTarget domain) targetIds

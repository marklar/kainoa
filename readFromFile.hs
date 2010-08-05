module Main where

import qualified Data.ByteString.Lazy as BL
import System.Environment

import Kainoa.ResultTbl (openResultTbl, getText, getTargets, getResultsForTarget)
import Kainoa.Types

main = do
  [idStr] <- getArgs
  let id = read idStr :: Int

  resultTbl <- openResultTbl "idx/games"

  putStrLn "text:"
  case getText resultTbl id of
    Just s  -> BL.putStrLn s
    Nothing -> putStrLn "not found"

  putStrLn "\ntargets:"
  let targetIds = getTargets resultTbl id
  putStrLn $ show targetIds

  putStrLn "\nall results with these targets:"
  mapM_ (putStrLn . show . getResultsForTarget resultTbl) targetIds

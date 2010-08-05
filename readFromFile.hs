module Main where

import qualified Data.ByteString.Lazy as BL
import System.Environment

import Kainoa.ResultTbl (openResultTbl, getPop, getText, getTargets, getResultsForTarget)
import Kainoa.Types

main = do
  [idStr] <- getArgs
  let id = read idStr :: Int

  resultTbl <- openResultTbl "idx/games"

  putStrLn "pop:"
  case getPop resultTbl id of
    Just p  -> putStrLn $ show p
    Nothing -> putStrLn "none"

  putStrLn "\ntext:"
  case getText resultTbl id of
    Just s  -> BL.putStrLn s
    Nothing -> putStrLn "none"

  putStrLn "\ntargets:"
  let targetIds = getTargets resultTbl id
  putStrLn $ show targetIds

  putStrLn "\nall results with these targets:"
  mapM_ (putStrLn . show . getResultsForTarget resultTbl) targetIds

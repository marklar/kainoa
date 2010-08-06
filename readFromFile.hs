module Main where

import qualified Data.ByteString.Lazy as BL
import System.Environment

import Kainoa.ResultTbl
    ( openResultTbl, getPop, getResultIdForPop
    , getText, getTargets, getResultsForTarget
    )
import Kainoa.Types

main = do
  [idStr] <- getArgs
  let id = read idStr :: Int

  resultTbl <- openResultTbl "idx/games"

  putStr "pop: "
  case getPop resultTbl id of
    Just p  -> do
        putStrLn $ show p
        putStr "resolves to ID: "
        putStrLn $ case getResultIdForPop resultTbl p of
                     Just resId -> show resId
                     Nothing -> "none"
    Nothing ->
        putStrLn "none"

  putStr "\ntext: "
  case getText resultTbl id of
    Just s  -> BL.putStrLn s
    Nothing -> putStrLn "none"

  putStr "\ntargets: "
  let targetIds = getTargets resultTbl id
  putStrLn $ show targetIds

  putStr "\nall results with these targets: "
  mapM_ (putStrLn . show . getResultsForTarget resultTbl) targetIds

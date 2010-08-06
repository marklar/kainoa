module Main where

import qualified Data.ByteString.Lazy as BL
import System.Environment
import Control.Monad (liftM)
import Data.List (concat)

import Kainoa.Types
import Kainoa.ResultTbl
    ( openResultTbl, getPop, getResultIdForPop
    , getText, getTargets, getResultsForTarget
    )
import Kainoa.Matrix (openMatrix, getIds, getPops)
import Kainoa.Lexicon (openLexicon, getLexeme', findId, ids)

import Kainoa.Util.Charset (utf8ToLatin1)

dir :: FilePath
dir = "idx/games"

main = do
  [lexeme] <- getArgs
  putStrLn lexeme
  lexemeIds <- getLexemeIds lexeme
  -- mapM_ showLexeme lexemeIds
  popIds <- liftM concat $ mapM getPopPostings lexemeIds
  putStrLn ""
  mapM_ showPopResult popIds

getLexemeIds :: String -> IO [Int]
getLexemeIds lexeme = do
  lexicon <- openLexicon dir
  let res = ids lexicon lexeme
  putStrLn $ show [(id, getLexeme' lexicon id) | id <- res ]
  return $ res

showLexeme :: Int -> IO ()
showLexeme lxmId = do
  lexicon <- openLexicon dir
  putStr "lexeme: "
  case getLexeme' lexicon lxmId of
    Nothing -> putStrLn "none"
    Just lxm -> do
      putStrLn lxm
      let id = findId lexicon lxm
      putStrLn $ "lexeme ID: " ++ show id

getPopPostings :: Int -> IO [Int]
getPopPostings lxmId = do
  mtx <- openMatrix dir
  let ids  = getIds  mtx lxmId
      pops = getPops mtx lxmId
  -- putStrLn "postings:"
  -- putStrLn $ "  ids:  " ++ (show ids)
  -- putStrLn $ "  pops: " ++ (show pops)
  return pops

showPopResult :: Int -> IO ()
showPopResult popId = do
  resultTbl <- openResultTbl dir
  case getResultIdForPop resultTbl popId of
    Just resId -> showResult resId
    Nothing -> putStrLn "none"

showResult :: Int -> IO ()
showResult id = do
  resultTbl <- openResultTbl dir

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

  putStr "text: "
  case getText resultTbl id of
    Just s  -> BL.putStrLn s
    Nothing -> putStrLn "none"

  putStr "targets: "
  let targetIds = getTargets resultTbl id
  putStrLn $ show targetIds

  putStr "all results with these targets: "
  mapM_ (putStrLn . show . getResultsForTarget resultTbl) targetIds

  putStrLn ""
module Main where

import qualified Data.ByteString.Lazy as BL
import System.Environment

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
  [idStr, lexeme] <- getArgs
  let id = read idStr :: Int
  showLexemeIds lexeme
  showLexeme id
  showPostings id
  showResult id

showLexemeIds :: String -> IO ()
showLexemeIds lexeme = do
  lexicon <- openLexicon dir
  putStrLn $ "lexeme IDs: " ++ show (ids lexicon lexeme)

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

showPostings :: Int -> IO ()
showPostings lxmId = do
  mtx <- openMatrix dir
  let ids  = getIds  mtx lxmId
      pops = getPops mtx lxmId
  putStrLn "postings:"
  putStrLn $ "  ids:  " ++ (show ids)
  putStrLn $ "  pops: " ++ (show pops)
  putStrLn ""

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

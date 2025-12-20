module Main (main) where

import Codec.Compression.BWT

import Data.ByteString qualified as BS

import System.Directory

import Control.Monad

main :: IO ()
main = do
  putStrLn "(E)ncoding or (D)ecoding? or (M)ultiple?"
  str <- getLine
  case str of
    [] -> main
    (c:rst)
      | (c == 'E' || c == 'e') -> mainEnc
      | (c == 'D' || c == 'd') -> mainDec
      | (c == 'Q' || c == 'q') -> return ()
      | (c == 'M' || c == 'm') -> mainMulti rst
      | otherwise              -> main

mainEnc :: IO ()
mainEnc = do
  putStrLn "Enter file name."
  inFile <- getLine
  putStrLn "Enter output name"
  outFile <- getLine
  bl1 <- doesFileExist inFile
  bl2 <- doesFileExist outFile
  if (not bl1)
    then putStrLn ("File does not exist: \"" ++ inFile ++ "\".")
    else do
      if bl2
        then putStrLn ("File already exists: \"" ++ outFile ++ "\".")
        else do
          bstr  <- BS.readFile inFile
          bstr2 <- encodeBwtIO bstr
          BS.writeFile outFile bstr2
          
mainDec :: IO ()
mainDec = do
  putStrLn "Enter file name."
  inFile <- getLine
  putStrLn "Enter output name"
  outFile <- getLine
  bl1 <- doesFileExist inFile
  bl2 <- doesFileExist outFile
  if (not bl1)
    then putStrLn ("File does not exist: \"" ++ inFile ++ "\".")
    else do
      if bl2
        then putStrLn ("File already exists: \"" ++ outFile ++ "\".")
        else do
          bstr  <- BS.readFile inFile
          bstr2 <- decodeBwtIO bstr
          BS.writeFile outFile bstr2
          
mainMulti :: String -> IO ()
mainMulti [] = askGet >> getLine >>= mainMulti
mainMulti (c:_) = do
  if | (c == 'E' || c == 'e') -> mainMultiEnc
     | (c == 'D' || c == 'd') -> mainMultiDec
     | (c == 'Q' || c == 'q') -> return ()
     | otherwise              -> askGet >> getLine >>= mainMulti

mainMultiEnc :: IO ()
mainMultiEnc = do
  (inFiles, outFiles) <- unzip <$> getFileNames
  inBstrs  <- mapM BS.readFile inFiles
  outBstrs <- encodeMultiBwtIO inBstrs
  zipWithM_ BS.writeFile outFiles outBstrs

mainMultiDec :: IO ()
mainMultiDec = do
  (inFiles, outFiles) <- unzip <$> getFileNames
  inBstrs  <- mapM BS.readFile inFiles
  outBstrs <- decodeMultiBwtIO inBstrs
  zipWithM_ BS.writeFile outFiles outBstrs

askGet :: IO ()
askGet = putStrLn "(E)ncoding or (D)ecoding?"

getFileNames :: IO [(String, String)]
getFileNames = do
  putStrLn "Enter file name."
  inFile <- getLine
  if (inFile == "" || inFile == "\n")
    then return []
    else do
      putStrLn "Enter output name"
      outFile <- getLine
      bl1 <- doesFileExist inFile
      bl2 <- doesFileExist outFile
      if (not bl1)
        then (putStrLn ("File does not exist: \"" ++ inFile ++ "\".") >> getFileNames)
        else do
          if bl2
            then (putStrLn ("File already exists: \"" ++ outFile ++ "\".") >> getFileNames)
            else ((inFile, outFile):) <$> getFileNames
  



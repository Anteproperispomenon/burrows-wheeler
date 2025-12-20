module Main (main) where

import Codec.Compression.BWT

import Data.ByteString qualified as BS

import System.Directory

main :: IO ()
main = do
  putStrLn "(E)ncoding or (D)ecoding?"
  str <- getLine
  case str of
    [] -> main
    (c:rst)
      | (c == 'E' || c == 'e') -> mainEnc
      | (c == 'D' || c == 'd') -> mainDec
      | (c == 'Q' || c == 'q') -> return ()
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
          

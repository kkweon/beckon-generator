{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified CommandLineParser as L
import qualified Data.Text.IO as TIO
import Options.Applicative as OA
import qualified System.Directory as Directory

main :: IO ()
main = do
  doesPackageJsonExist <- Directory.doesFileExist "./package.json"
  if doesPackageJsonExist
    then L.handleArgOptions =<< OA.execParser L.finalParserInfo
    else TIO.putStrLn "package.json not found in the current directory"

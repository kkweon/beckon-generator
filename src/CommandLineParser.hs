{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : CommandLineParser
Description : Command line arguments
Copyright   : (c) Mo Kweon
License     : MIT
Maintainer  : kkweon@gmail.com
Stability   : experimental
Portability : POSIX

This file handles command line arguments
-}
module CommandLineParser
  ( handleArgOptions
  , finalParserInfo
  ) where

import qualified AngularJS.Main as AMain
import Control.Monad (unless)
import Data.Semigroup ((<>))
import Data.Version (showVersion)
import Options.Applicative ((<**>))
import qualified Options.Applicative as A
import Paths_beckon_generator (version)
import qualified React.Main as RMain
import qualified React.Model as RModel
import qualified System.Directory as D
import System.Exit (exitFailure)

handleArgOptions :: ArgOptions -> IO ()
handleArgOptions (React reactOption) = do
  checkPackageJson
  RMain.handleReactOption reactOption
handleArgOptions (AngularJS angularJSOption) = do
  checkPackageJson
  AMain.handleNgGenerateComponent angularJSOption

checkPackageJson :: IO ()
checkPackageJson = do
  doesPackageJsonExist <- D.doesFileExist "./package.json"
  unless
    doesPackageJsonExist
    (putStrLn "package.json not found." >> exitFailure)

finalParserInfo :: A.ParserInfo ArgOptions
finalParserInfo =
  A.info
    (A.helper <*> versionOption <*> finalParser)
    (A.fullDesc <> A.progDesc "Beckon Generator CLI")
  where
    versionOption =
      A.infoOption
        (showVersion version)
        (A.long "version" <> A.short 'V' <> A.help "show version")

finalParser :: A.Parser ArgOptions
finalParser =
  A.subparser
    (A.command "react" reactParser <>
     A.command
       "ng"
       (A.info
          (argAngularJSOptionParser <**> A.helper)
          (A.progDesc "Generate AngularJS file")))
  where
    reactParser =
      A.info
        (argReactOptionParser <**> A.helper)
        (A.progDesc "Generate React file")

data ArgOptions
  = React RModel.ReactOption
  | AngularJS AMain.AngularJSOption
  deriving (Show)

argAngularJSOptionParser :: A.Parser ArgOptions
argAngularJSOptionParser = AngularJS <$> AMain.beckonNgGeneratorOptionParser

argReactOptionParser :: A.Parser ArgOptions
argReactOptionParser = React <$> RMain.reactOptionParser

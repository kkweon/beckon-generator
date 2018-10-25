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
import Data.Semigroup ((<>))
import Options.Applicative ((<**>))
import qualified Options.Applicative as A
import qualified React.Main as RMain
import qualified React.Model as RModel

handleArgOptions :: ArgOptions -> IO ()
handleArgOptions (React reactOption) = RMain.handleReactOption reactOption
handleArgOptions (AngularJS angularJSOption) =
  AMain.handleNgGenerateComponent angularJSOption

finalParserInfo :: A.ParserInfo ArgOptions
finalParserInfo =
  A.info (finalParser <**> A.helper) (A.progDesc "Beckon Generator CLI")

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

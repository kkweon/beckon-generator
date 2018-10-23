{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : Lib
Description : Command line arguments
Copyright   : (c) Mo Kweon
License     : MIT
Maintainer  : kkweon@gmail.com
Stability   : experimental
Portability : POSIX

This file handles command line arguments
-}
module Lib
  ( handleGenerateComponent
  , BeckonGeneratorOption(..)
  , beckonGeneratorOptionParser
  )
where

import           Data.Semigroup                 ( (<>) )
import           Control.Applicative            ( (<|>) )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified FileIO                        as F
import qualified Options.Applicative           as A
import qualified System.Directory              as D
import qualified Template                      as TE
import qualified AngularJSType
import qualified FileType

-- | Entry point
handleGenerateComponent :: BeckonGeneratorOption -> IO ()
handleGenerateComponent BeckonGeneratorOption { moduleName, generatedFileTypeFlag, angularJSType, force, fileType }
  = do
    doesPackageJsonExist <- D.doesFileExist "package.json"
    if doesPackageJsonExist
      then case TE.getBeckonGeneratedFile fileType angularJSType moduleName of
        Left errorMsg -> TIO.putStrLn errorMsg
        Right beckonGenerated ->
          F.generateBeckonFiles generatedFileTypeFlag force beckonGenerated
      else
        TIO.putStrLn
          "package.json not found. Please run again from the project root (where package.json exists)"

-- | Available command line options
data BeckonGeneratorOption = BeckonGeneratorOption
  { moduleName :: T.Text
  , generatedFileTypeFlag :: F.GenerateFileTypeFlag
  , angularJSType :: AngularJSType.AngularJSType
  , force :: Bool
  , fileType :: FileType.FileType
  }

-- | Helper to build a command line options
beckonGeneratorOptionParser :: A.Parser BeckonGeneratorOption
beckonGeneratorOptionParser =
  BeckonGeneratorOption
    <$> A.strArgument
          (  A.metavar "MODULE NAME"
          <> A.help "Beckon Module Name (e.g., beckon.steel.answerPage)"
          )
    <*> (   A.flag
            F.SrcOnly
            F.All
            (A.long "spec" <> A.short 'S' <> A.help
              "Generate a spec file along with JS/TS file"
            )
        <|> A.flag'
              F.SpecOnly
              (A.long "spec-only" <> A.help "Generate a spec file only")
        )
    <*> A.flag AngularJSType.Component
               AngularJSType.Service
               (A.long "service" <> A.help "Generate a service file")
    <*> A.switch (A.long "force" <> A.help "Force (overwrite if file exists)")
    <*> A.flag
          FileType.JavaScript
          FileType.OldTypeScript
          (  A.long "old-typescript"
          <> A.help "Generates an old (namespace) TypeScript"
          )

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
handleGenerateComponent BeckonGeneratorOption { moduleName, specFile, specOnly, isService, force, isOldTS }
  = do
    doesPackageJsonExist <- D.doesFileExist "package.json"
    if doesPackageJsonExist
      then
        case
          TE.getBeckonGeneratedFile (getFileType isOldTS)
                                    (getAngularJSType isService)
                                    moduleName
        of
          Left errorMsg -> TIO.putStrLn errorMsg
          Right beckonGenerated ->
            F.generateBeckonFiles flags force beckonGenerated
      else
        TIO.putStrLn
          "package.json not found. Please run again from the project root (where package.json exists)"
 where
  flags :: F.GenerateFileTypeFlag
  flags | specOnly  = F.SpecOnly
        | specFile  = F.All
        | otherwise = F.SrcOnly

  getFileType
    :: Bool  -- ^ isOldTypeScript
    -> FileType.FileType
  getFileType True = FileType.OldTypeScript
  getFileType _    = FileType.JavaScript

  getAngularJSType
    :: Bool -- ^ isService
    -> AngularJSType.AngularJSType
  getAngularJSType True = AngularJSType.Service
  getAngularJSType _    = AngularJSType.Component

-- | Available command line options
data BeckonGeneratorOption = BeckonGeneratorOption
  { moduleName :: T.Text
  , specFile :: Bool
  , specOnly :: Bool
  , isService :: Bool
  , force :: Bool
  , isOldTS :: Bool
  }

-- | Helper to build a command line options
beckonGeneratorOptionParser :: A.Parser BeckonGeneratorOption
beckonGeneratorOptionParser =
  BeckonGeneratorOption
    <$> A.strArgument
          (  A.metavar "MODULE NAME"
          <> A.help "Beckon Module Name (e.g., beckon.steel.answerPage)"
          )
    <*> A.switch (A.long "spec" <> A.short 'S' <> A.help "Generate a spec file")
    <*> A.switch (A.long "spec-only" <> A.help "Generate a spec file only")
    <*> A.switch (A.long "service" <> A.help "Generate a service file")
    <*> A.switch (A.long "force" <> A.help "Force (overwrite if file exists)")
    <*> A.switch
          (  A.long "old-typescript"
          <> A.help "Generates an old (namespace) TypeScript"
          )

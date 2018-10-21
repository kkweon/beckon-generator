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
import qualified NameBuilder                   as N
import qualified Options.Applicative           as A
import qualified System.Directory              as D
import qualified Template                      as TE

import           Control.Monad                  ( when )

-- | Entry point
handleGenerateComponent :: BeckonGeneratorOption -> IO ()
handleGenerateComponent BeckonGeneratorOption { moduleName, specFile, specOnly, isService, force }
  = do
    doesPackageJsonExist <- D.doesFileExist "package.json"
    if doesPackageJsonExist
      then
        (let generatedType = if isService then TE.Service else TE.Component
         in  case TE.getBeckonGeneratedFile generatedType moduleName of
               Left errorMsg -> TIO.putStrLn errorMsg
               Right beckonGenerated ->
                 F.generateBeckonFiles flags force beckonGenerated
        )
      else
        TIO.putStrLn
          "package.json not found. Please run again from the project root (where package.json exists)"
 where
  flags :: F.GenerateFileTypeFlag
  flags | specOnly  = F.SpecOnly
        | specFile  = F.All
        | otherwise = F.SrcOnly


-- | Available command line options
data BeckonGeneratorOption = BeckonGeneratorOption
  { moduleName :: T.Text
  , specFile :: Bool
  , specOnly :: Bool
  , isService :: Bool
  , force :: Bool
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

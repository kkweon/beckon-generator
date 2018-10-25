{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module AngularJS.Main
  ( handleNgGenerateComponent
  , AngularJSOption
  , beckonNgGeneratorOptionParser
  ) where

import qualified AngularJS.AngularJSType as AngularJSType
import qualified AngularJS.FileBuilder as AFileBuilder
import qualified AngularJS.Model as AModel
import qualified AngularJS.Template as ATemplate
import qualified Common.FileType
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Options.Applicative as A
import Options.Applicative ((<|>))
import qualified System.Directory as D

-- | Entry point
handleNgGenerateComponent :: AngularJSOption -> IO ()
handleNgGenerateComponent AngularJSOption { moduleName
                                          , generatedFileTypeFlag
                                          , angularJSType
                                          , force
                                          , fileType
                                          } = do
  doesPackageJsonExist <- D.doesFileExist "./package.json"
  if doesPackageJsonExist
    then case ATemplate.getBeckonGeneratedFile fileType angularJSType moduleName of
           Left errorMsg -> TIO.putStrLn errorMsg
           Right beckonGenerated ->
             AFileBuilder.generateNgBeckonFiles
               generatedFileTypeFlag
               force
               beckonGenerated
    else TIO.putStrLn
           "package.json not found. Please run again from the project root (where package.json exists)"

-- | Available command line options
data AngularJSOption = AngularJSOption
  { moduleName :: T.Text
  , generatedFileTypeFlag :: AModel.AngularJSGenType
  , angularJSType :: AngularJSType.AngularJSType
  , force :: Bool
  , fileType :: Common.FileType.FileType
  } deriving (Show)

-- | Helper to build a command line options
beckonNgGeneratorOptionParser :: A.Parser AngularJSOption
beckonNgGeneratorOptionParser =
  AngularJSOption <$>
  A.strArgument
    (A.metavar "MODULE NAME" <>
     A.help "Beckon Module Name (e.g., beckon.steel.answerPage)") <*>
  (A.flag
     AModel.SrcOnly
     AModel.All
     (A.long "spec" <> A.short 'S' <>
      A.help "Generate a spec file along with JS/TS file") <|>
   A.flag'
     AModel.SpecOnly
     (A.long "spec-only" <> A.help "Generate a spec file only")) <*>
  A.flag
    AngularJSType.Component
    AngularJSType.Service
    (A.long "service" <> A.help "Generate a service file") <*>
  A.switch (A.long "force" <> A.help "Force (overwrite if file exists)") <*>
  A.flag
    Common.FileType.JavaScript
    Common.FileType.OldTypeScript
    (A.long "old-typescript" <> A.help "Generates an old (namespace) TypeScript")

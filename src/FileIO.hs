{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : FileIO
Description : Generate files based on the filepath
Copyright   : (c) Mo Kweon
License     : MIT
Maintainer  : kkweon@gmail.com
Stability   : experimental
Portability : POSIX

Generates a file
-}
module FileIO
  ( generateBeckonFiles
  , GenerateFileTypeFlag(..)
  )
where

import           Text.Printf                    ( printf )
import           Control.Monad                  ( unless
                                                , when
                                                )
import qualified Data.Text.IO                  as IO
import qualified System.Directory              as D
import qualified System.FilePath.Posix         as F
import           Template                       ( BeckonFile(..)
                                                , BeckonGeneratedFile(..)
                                                )


data GenerateFileTypeFlag = All | SpecOnly | SrcOnly deriving (Eq)

-- | Generate Beckon Files
generateBeckonFiles
  :: GenerateFileTypeFlag
  -> Bool -- ^ FORCE
  -> BeckonGeneratedFile
  -> IO ()
generateBeckonFiles SpecOnly force BeckonGeneratedService { specFile } =
  generateBeckonFile specFile force
generateBeckonFiles SpecOnly force BeckonGeneratedComponent { specFile } =
  generateBeckonFile specFile force
generateBeckonFiles generateType force BeckonGeneratedService { srcFile, specFile }
  = do
    generateBeckonFile srcFile force
    when (generateType == All) (generateBeckonFile specFile force)
generateBeckonFiles generateType force BeckonGeneratedComponent { srcFile, tmplFile, specFile }
  = do
    generateBeckonFile srcFile  force
    generateBeckonFile tmplFile force
    when (generateType == All) (generateBeckonFile specFile force)

generateBeckonFile
  :: BeckonFile
  -> Bool -- ^ Force
  -> IO ()
generateBeckonFile BeckonFile { target, content } force = do
  let dirName = F.takeDirectory target
  doesDirExist <- D.doesDirectoryExist dirName
  unless doesDirExist (D.createDirectoryIfMissing True dirName)

  fileExists <- D.doesFileExist target
  writeFileHandler fileExists force
 where
  writeFileHandler True True =
    printf "Overwriting %s\n" target >> IO.writeFile target content
  writeFileHandler True  False = showErrorMsg
  writeFileHandler False _     = IO.writeFile target content

  showErrorMsg :: IO ()
  showErrorMsg =
    printf "Skipping a file (%s) because it already exists\n" target

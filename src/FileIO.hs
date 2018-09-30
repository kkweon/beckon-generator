{-# LANGUAGE NamedFieldPuns #-}

module FileIO where

import Control.Monad (when, unless)
import qualified Data.Text.IO as IO
import qualified System.Directory as D
import qualified System.FilePath.Posix as F
import Template (BeckonFile(..), BeckonGeneratedFile(..))

generateBeckonFiles :: Bool -> BeckonGeneratedFile -> IO ()
generateBeckonFiles shouldGenerateSpecFile BeckonGeneratedService {srcFile, specFile} = do
  generateBeckonFile srcFile
  unless (not shouldGenerateSpecFile) (generateBeckonFile specFile)
generateBeckonFiles shouldGenerateSpecFile BeckonGeneratedComponent {srcFile, tmplFile, specFile} = do
  generateBeckonFile srcFile
  generateBeckonFile tmplFile

  unless (not shouldGenerateSpecFile) (generateBeckonFile specFile)

generateBeckonFile :: BeckonFile -> IO ()
generateBeckonFile BeckonFile {target, content} = do
  let dirName = F.takeDirectory target
  doesDirExist <- D.doesDirectoryExist dirName
  unless doesDirExist (D.createDirectoryIfMissing True dirName)
  IO.writeFile target content

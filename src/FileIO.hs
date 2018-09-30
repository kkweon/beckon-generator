{-# LANGUAGE NamedFieldPuns #-}

module FileIO where

import Control.Monad (when, unless)
import qualified Data.Text.IO as IO
import qualified System.Directory as D
import qualified System.FilePath.Posix as F
import Template (BeckonFile(..), BeckonGenerated(..))

generateBeckonFiles :: Bool -> BeckonGenerated -> IO ()
generateBeckonFiles shouldGenerateSpecFile (BeckonGenerated {srcFile, tmplFile, specFile}) = do
  generateBeckonFile srcFile
  generateBeckonFile tmplFile

  unless (not shouldGenerateSpecFile) (generateBeckonFile specFile)

generateBeckonFile :: BeckonFile -> IO ()
generateBeckonFile BeckonFile {target, content} = do
  let dirName = F.takeDirectory target
  doesDirExist <- D.doesDirectoryExist dirName
  when (not doesDirExist) (D.createDirectoryIfMissing True dirName)
  IO.writeFile target content

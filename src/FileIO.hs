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
  ) where

import           Control.Monad         (unless, when)
import qualified Data.Text.IO          as IO
import qualified System.Directory      as D
import qualified System.FilePath.Posix as F
import           Template              (BeckonFile (..),
                                        BeckonGeneratedFile (..))

-- | Generate Beckon Files
generateBeckonFiles :: Bool -- ^ If True, generates a spec file
                    -> BeckonGeneratedFile
                    -> IO ()
generateBeckonFiles shouldGenerateSpecFile BeckonGeneratedService { srcFile
                                                                  , specFile
                                                                  } = do
  generateBeckonFile srcFile
  unless (not shouldGenerateSpecFile) (generateBeckonFile specFile)
generateBeckonFiles shouldGenerateSpecFile BeckonGeneratedComponent { srcFile
                                                                    , tmplFile
                                                                    , specFile
                                                                    } = do
  generateBeckonFile srcFile
  generateBeckonFile tmplFile
  unless (not shouldGenerateSpecFile) (generateBeckonFile specFile)

generateBeckonFile :: BeckonFile -> IO ()
generateBeckonFile BeckonFile {target, content} = do
  let dirName = F.takeDirectory target
  doesDirExist <- D.doesDirectoryExist dirName
  unless doesDirExist (D.createDirectoryIfMissing True dirName)
  IO.writeFile target content

{-# LANGUAGE NamedFieldPuns #-}

module AngularJS.Model
  ( AngularJSGenType(..)
  , BeckonNgGeneratedFile(..)
  , grabFiles
  ) where

import Common.BeckonFile (BeckonFile(..))
import qualified Common.FileIO as FIO

data AngularJSGenType
  = All
  | SpecOnly
  | SrcOnly
  deriving (Eq, Show)

-- | Either Service / Component is supported
data BeckonNgGeneratedFile
  = BeckonGeneratedComponent { srcFile :: Common.BeckonFile.BeckonFile
                             , tmplFile :: Common.BeckonFile.BeckonFile
                             , specFile :: Common.BeckonFile.BeckonFile }
  | BeckonGeneratedService { srcFile :: Common.BeckonFile.BeckonFile
                           , specFile :: Common.BeckonFile.BeckonFile }

class FileGeneratable a where
  grabFiles :: a -> AngularJSGenType -> [BeckonFile]

instance FileGeneratable BeckonNgGeneratedFile where
  grabFiles BeckonGeneratedService {srcFile, specFile} flag =
    case flag of
      All -> [srcFile, specFile]
      SpecOnly -> [specFile]
      SrcOnly -> [srcFile]
  grabFiles BeckonGeneratedComponent {srcFile, tmplFile, specFile} flag =
    case flag of
      All -> [srcFile, tmplFile, specFile]
      SpecOnly -> [specFile]
      SrcOnly -> [srcFile, tmplFile]

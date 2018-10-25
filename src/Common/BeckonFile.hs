module Common.BeckonFile
  ( BeckonFile(..)
  ) where

import qualified Common.FileType
import qualified Data.Text
import qualified System.FilePath.Posix

-- | Indicate a each Beckon File
data BeckonFile = BeckonFile
  { target :: System.FilePath.Posix.FilePath -- ^ FilePath to be created
  , content :: Data.Text.Text -- ^ The content of file to be created
  , fileType :: Common.FileType.FileType -- ^ JS or TS
  } deriving (Show)

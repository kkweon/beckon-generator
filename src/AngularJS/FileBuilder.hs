module AngularJS.FileBuilder
  ( generateNgBeckonFiles
  ) where

import qualified AngularJS.Model as Model
import qualified Common.FileIO as FIO

-- | Generate Beckon Files
generateNgBeckonFiles ::
     Model.AngularJSGenType
  -> Bool -- ^ FORCE
  -> Model.BeckonNgGeneratedFile
  -> IO ()
generateNgBeckonFiles flag force beckonTarget =
  FIO.parallelGenerateBeckonFiles force files
  where
    files = Model.grabFiles beckonTarget flag

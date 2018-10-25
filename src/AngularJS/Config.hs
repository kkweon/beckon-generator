{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : AngularJS.Config
Description : Prefix and Template files
Copyright   : (c) Mo Kweon
License     : MIT
Maintainer  : kkweon@gmail.com
Stability   : experimental
Portability : POSIX

Template and prefix are defined in this file
-}
module AngularJS.Config
  ( srcDirectory
  , testDirectory
  , componentTemplate
  , componentOldTypeScriptTemplate
  , serviceTemplate
  , serviceOldTypeScriptTemplate
  ) where

import qualified System.FilePath.Posix as F
import qualified Text.Mustache as M
import Text.Mustache.Compile (embedSingleTemplate)

-- | Prefix for a JS src file
srcDirectory :: F.FilePath
srcDirectory = "./src/main/resources/com/beckon"

-- | Prefix for a JS spec file
testDirectory :: F.FilePath
testDirectory = "./src/test/javascript/unit"

-- | Mustache template file to be used to generated a component
componentTemplate :: M.Template
componentTemplate = $(embedSingleTemplate "templates/component.mustache")

-- | Old TypeScript Mustache template file
componentOldTypeScriptTemplate :: M.Template
componentOldTypeScriptTemplate =
  $(embedSingleTemplate "templates/component.old-typescript.mustache")

-- | Mustache template file to be used to generated a service
serviceTemplate :: M.Template
serviceTemplate = $(embedSingleTemplate "templates/service.mustache")

-- | Old TypeScript Mustache template file
serviceOldTypeScriptTemplate :: M.Template
serviceOldTypeScriptTemplate =
  $(embedSingleTemplate "templates/service.old-typescript.mustache")

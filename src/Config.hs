{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Config
Description : Prefix and Template files
Copyright   : (c) Mo Kweon
License     : MIT
Maintainer  : kkweon@gmail.com
Stability   : experimental
Portability : POSIX

Template and prefix are defined in this file
-}
module Config
  ( srcDirectory
  , testDirectory
  , componentTemplate
  , serviceTemplate
  )
where

import qualified System.FilePath.Posix         as F
import qualified Text.Mustache                 as M
import           Text.Mustache.Compile          ( embedSingleTemplate )

-- | Prefix for a JS src file
srcDirectory :: F.FilePath
srcDirectory = "./src/main/resources/com/beckon"

-- | Prefix for a JS spec file
testDirectory :: F.FilePath
testDirectory = "./src/test/javascript/unit"

-- | Mustache template file to be used to generated a component
componentTemplate :: M.Template
componentTemplate = $(embedSingleTemplate "templates/component.mustache")

-- | Mustache template file to be used to generated a service
serviceTemplate :: M.Template
serviceTemplate = $(embedSingleTemplate "templates/service.mustache")

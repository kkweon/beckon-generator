{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config
  ( srcDirectory
  , testDirectory
  , componentTemplate
  ) where

import qualified System.FilePath.Posix as F
import qualified Text.Mustache as M
import Text.Mustache.Compile (embedSingleTemplate)

-- | baseBeckon = "./src/main/resources/com/beckon/steel/answerPage/answerPage.js"
srcDirectory :: F.FilePath
srcDirectory = "./src/main/resources/com/beckon"

testDirectory :: F.FilePath
testDirectory = "./src/test/javascript/unit"

componentTemplate :: M.Template
componentTemplate = $(embedSingleTemplate "templates/component.mustache")

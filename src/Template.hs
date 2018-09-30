{-# LANGUAGE OverloadedStrings #-}

module Template where

import qualified Config as C
import qualified Data.Text as T
import qualified NameBuilder as N
import Text.Mustache (ToMustache, (~>), object, substitute, toMustache)

data NgTemplate = NgTemplate
  { moduleName :: T.Text
  , componentName :: T.Text
  , component_name :: T.Text -- component-name
  } deriving (Show)

instance ToMustache NgTemplate where
  toMustache ngTemplate =
    object
      [ "componentName" ~> componentName ngTemplate
      , "component-name" ~> component_name ngTemplate
      , "moduleName" ~> moduleName ngTemplate
      ]

data BeckonFile = BeckonFile
  { target :: FilePath
  , content :: T.Text
  } deriving (Show)

data BeckonGenerated = BeckonGenerated
  { srcFile :: BeckonFile
  , tmplFile :: BeckonFile
  , specFile :: BeckonFile
  } deriving (Show)

getBeckonGeneratedComponent :: T.Text -> Either T.Text BeckonGenerated
getBeckonGeneratedComponent maybeModuleName
  | N.isModuleName maybeModuleName == False =
    Left "Module Name should be something like steel.answerPage"
  | otherwise =
    let beckonModuleName = N.addBeckonPrefix maybeModuleName
        ngTemplate =
          NgTemplate
            { moduleName = beckonModuleName
            , componentName = N.getComponentName beckonModuleName
            , component_name =
                N.toHypenCase (N.getComponentName beckonModuleName)
            }
        processedTemplate =
          T.splitOn "####" (substitute C.componentTemplate ngTemplate)
     in
        getBeckonGenerated beckonModuleName processedTemplate


getBeckonGenerated :: T.Text -> [T.Text] -> Either T.Text BeckonGenerated
getBeckonGenerated beckonModuleName (src:tmpl:spec:[]) =
  let srcFile =
        BeckonFile {target = N.getSrcFilePath beckonModuleName, content = src}
      tmplFile =
        BeckonFile {target = N.getTmplFilePath beckonModuleName, content = tmpl}
      specFile =
        BeckonFile {target = N.getSpecFilePath beckonModuleName, content = spec}
   in Right
        BeckonGenerated
          {srcFile = srcFile, tmplFile = tmplFile, specFile = specFile}
getBeckonGenerated _ _ = Left "Failed to parse template"

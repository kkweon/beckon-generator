{-# LANGUAGE OverloadedStrings #-}

module Template where

import qualified Config as C
import qualified Data.Text as T
import qualified NameBuilder as N
import Text.Mustache (ToMustache, (~>), object, substitute, toMustache)

data NgTemplate = NgTemplate
  { moduleName :: T.Text
  , componentName :: T.Text
  , _ComponentName :: T.Text
  , component_name :: T.Text -- component-name
  } deriving (Show)

instance ToMustache NgTemplate where
  toMustache ngTemplate =
    object
      [ "componentName" ~> componentName ngTemplate
      , "ComponentName" ~> _ComponentName ngTemplate
      , "component-name" ~> component_name ngTemplate
      , "moduleName" ~> moduleName ngTemplate
      ]

data BeckonFile = BeckonFile
  { target :: FilePath
  , content :: T.Text
  } deriving (Show)

data BeckonGeneratedFile
  = BeckonGeneratedComponent { srcFile :: BeckonFile
                             , tmplFile :: BeckonFile
                             , specFile :: BeckonFile }
  | BeckonGeneratedService { srcFile :: BeckonFile
                           , specFile :: BeckonFile }
  deriving (Show)


data GeneratedType = Component
                   | Service deriving (Show)

getBeckonGeneratedFile :: GeneratedType -> T.Text -> Either T.Text BeckonGeneratedFile
getBeckonGeneratedFile generatedType maybeModuleName
  | N.isModuleName maybeModuleName =
    let beckonModuleName = N.addBeckonPrefix maybeModuleName
        ngTemplate =
          NgTemplate
            { moduleName = beckonModuleName
            , componentName = N.getComponentName beckonModuleName
            , _ComponentName = N.get_ComponentName beckonModuleName
            , component_name =
                N.toHypenCase (N.getComponentName beckonModuleName)
            }
        templateFile =
            case generatedType of
                Service -> C.serviceTemplate
                Component -> C.componentTemplate

        processedTemplate =
          T.splitOn "####" (substitute templateFile ngTemplate)
     in _getBeckonGenerated beckonModuleName processedTemplate
  | otherwise = Left "Module Name should be something like steel.answerPage"






-- | Private Function
-- | Build BeckonGeneratedFile
_getBeckonGenerated :: T.Text -> [T.Text] -> Either T.Text BeckonGeneratedFile
_getBeckonGenerated beckonModuleName [src, spec] =
  let srcFile =
        BeckonFile {target = N.getSrcFilePath beckonModuleName, content = src}
      specFile =
        BeckonFile {target = N.getSpecFilePath beckonModuleName, content = spec}
   in Right BeckonGeneratedService {srcFile = srcFile, specFile = specFile}
_getBeckonGenerated beckonModuleName [src, tmpl, spec] =
  let srcFile =
        BeckonFile {target = N.getSrcFilePath beckonModuleName, content = src}
      tmplFile =
        BeckonFile {target = N.getTmplFilePath beckonModuleName, content = tmpl}
      specFile =
        BeckonFile {target = N.getSpecFilePath beckonModuleName, content = spec}
   in Right
        BeckonGeneratedComponent
          {srcFile = srcFile, tmplFile = tmplFile, specFile = specFile}
_getBeckonGenerated _ _ = Left "Failed to parse template"

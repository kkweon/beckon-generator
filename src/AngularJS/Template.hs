{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Template
Description : Template generator
Copyright   : (c) Mo Kweon
License     : MIT
Maintainer  : kkweon@gmail.com
Stability   : experimental
Portability : POSIX

This module contains helper functions dealing with mustach templates
-}
module AngularJS.Template
  ( NgTemplate(..)
  , getBeckonGeneratedFile
  ) where

import qualified AngularJS.AngularJSType
import qualified AngularJS.Config
import AngularJS.Model (specFile, srcFile, tmplFile)
import qualified AngularJS.Model
import qualified AngularJS.NameBuilder
import qualified Common.BeckonFile
import Common.BeckonFile (content, fileType, target)
import qualified Common.ContentType
import qualified Common.FileType
import qualified Data.Text as T
import qualified Text.Mustache
import Text.Mustache (ToMustache, (~>), object, substitute, toMustache)

-- | This type is used to replace a mustache template file
-- The template file contains @{{ moduleName }}@, @{{ componentName }}@, @{{ ComponentName }}@, @{{ component-name }}@
--
-- These variables will be replaced by this type
data NgTemplate = NgTemplate
  { moduleName :: T.Text -- ^ will replace @{{ moduleName }}@ (e.g., @beckon.steel.answerPage@)
  , componentName :: T.Text -- ^ will replace @{{ componentName }}@ (e.g., @answerPage@)
  , _ComponentName :: T.Text -- ^ will replace @{{ ComponentName } }@ (e.g., @AnswerPage@)
  , component_name :: T.Text -- ^ will replace @{{ component-name }}@ (e.g., @answer-page@)
  } deriving (Show)

instance ToMustache NgTemplate where
  toMustache ngTemplate =
    object
      [ "componentName" ~> componentName ngTemplate
      , "ComponentName" ~> _ComponentName ngTemplate
      , "component-name" ~> component_name ngTemplate
      , "moduleName" ~> moduleName ngTemplate
      ]

-- |
--
-- >>> getBeckonGeneratedFile Component "beckon.steel.answerPage"
getBeckonGeneratedFile ::
     Common.FileType.FileType -- ^ JS / OldTS
  -> AngularJS.AngularJSType.AngularJSType -- ^ AngularJS component / service
  -> T.Text -- ^ moduleName
  -> Either T.Text AngularJS.Model.BeckonNgGeneratedFile
getBeckonGeneratedFile fileType angularJSType maybeModuleName
  | AngularJS.NameBuilder.isModuleName maybeModuleName =
    let beckonModuleName = AngularJS.NameBuilder.addBeckonPrefix maybeModuleName
        ngTemplate =
          NgTemplate
            { moduleName = beckonModuleName
            , componentName =
                AngularJS.NameBuilder.getComponentName beckonModuleName
            , _ComponentName =
                AngularJS.NameBuilder.get_ComponentName beckonModuleName
            , component_name =
                AngularJS.NameBuilder.toHypenCase
                  (AngularJS.NameBuilder.getComponentName beckonModuleName)
            }
        templateFile = getTemplateFile fileType angularJSType
        processedTemplate =
          map T.strip . T.splitOn "####" $ substitute templateFile ngTemplate
     in _getBeckonGenerated fileType beckonModuleName processedTemplate
  | otherwise = Left "Module Name should be something like steel.answerPage"

getTemplateFile ::
     Common.FileType.FileType
  -> AngularJS.AngularJSType.AngularJSType
  -> Text.Mustache.Template
getTemplateFile Common.FileType.JavaScript AngularJS.AngularJSType.Component =
  AngularJS.Config.componentTemplate
getTemplateFile Common.FileType.JavaScript AngularJS.AngularJSType.Service =
  AngularJS.Config.serviceTemplate
getTemplateFile Common.FileType.OldTypeScript AngularJS.AngularJSType.Component =
  AngularJS.Config.componentOldTypeScriptTemplate
getTemplateFile Common.FileType.OldTypeScript AngularJS.AngularJSType.Service =
  AngularJS.Config.serviceOldTypeScriptTemplate

-- | Private Function
-- Build BeckonNgGeneratedFile
_getBeckonGenerated ::
     Common.FileType.FileType
  -> T.Text
  -> [T.Text]
  -> Either T.Text AngularJS.Model.BeckonNgGeneratedFile
_getBeckonGenerated fileType beckonModuleName [src, spec] =
  let srcFile =
        Common.BeckonFile.BeckonFile
          { target =
              AngularJS.NameBuilder.getSrcFilePath fileType beckonModuleName
          , content = src
          , fileType = fileType
          }
      specFile =
        Common.BeckonFile.BeckonFile
          { target = AngularJS.NameBuilder.getSpecFilePath beckonModuleName
          , content = spec
          , fileType = fileType
          }
   in Right
        AngularJS.Model.BeckonGeneratedService
          {srcFile = srcFile, specFile = specFile}
_getBeckonGenerated fileType beckonModuleName [src, tmpl, spec] =
  let srcFile =
        Common.BeckonFile.BeckonFile
          { target =
              AngularJS.NameBuilder.getSrcFilePath fileType beckonModuleName
          , content = src
          , fileType = fileType
          }
      tmplFile =
        Common.BeckonFile.BeckonFile
          { target = AngularJS.NameBuilder.getTmplFilePath beckonModuleName
          , content = tmpl
          , fileType = fileType
          }
      specFile =
        Common.BeckonFile.BeckonFile
          { target = AngularJS.NameBuilder.getSpecFilePath beckonModuleName
          , content = spec
          , fileType = fileType
          }
   in Right
        AngularJS.Model.BeckonGeneratedComponent
          {srcFile = srcFile, tmplFile = tmplFile, specFile = specFile}
_getBeckonGenerated _ _ _ = Left "Failed to parse template"

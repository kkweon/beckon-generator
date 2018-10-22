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
module Template
  ( NgTemplate(..)
  , BeckonGeneratedFile(..)
  , BeckonFile(..)
  , GeneratedType(..)
  , getBeckonGeneratedFile
  )
where

import qualified Config                        as C
import qualified Data.Text                     as T
import qualified NameBuilder                   as N
import           Text.Mustache                  ( ToMustache
                                                , (~>)
                                                , object
                                                , substitute
                                                , toMustache
                                                )

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

-- | Indicate a each Beckon File
data BeckonFile = BeckonFile
  { target :: FilePath -- ^ FilePath to be created
  , content :: T.Text -- ^ The content of file to be created
  } deriving (Show)

-- | Either Service / Component is supported
data BeckonGeneratedFile
  = BeckonGeneratedComponent { srcFile :: BeckonFile
                             , tmplFile :: BeckonFile
                             , specFile :: BeckonFile }
  | BeckonGeneratedService { srcFile :: BeckonFile
                           , specFile :: BeckonFile }
  deriving (Show)


data GeneratedType
  = Component
  | Service
  deriving (Show)

-- |
--
-- >>> getBeckonGeneratedFile Component "beckon.steel.answerPage"
getBeckonGeneratedFile
  :: GeneratedType -- ^ Component or Service
  -> T.Text -- ^ moduleName
  -> Either T.Text BeckonGeneratedFile
getBeckonGeneratedFile generatedType maybeModuleName
  | N.isModuleName maybeModuleName
  = let
      beckonModuleName = N.addBeckonPrefix maybeModuleName
      ngTemplate       = NgTemplate
        { moduleName     = beckonModuleName
        , componentName  = N.getComponentName beckonModuleName
        , _ComponentName = N.get_ComponentName beckonModuleName
        , component_name = N.toHypenCase (N.getComponentName beckonModuleName)
        }
      templateFile = case generatedType of
        Service   -> C.serviceTemplate
        Component -> C.componentTemplate
      processedTemplate =
        map T.strip . T.splitOn "####" $ substitute templateFile ngTemplate
    in
      _getBeckonGenerated beckonModuleName processedTemplate
  | otherwise
  = Left "Module Name should be something like steel.answerPage"

-- | Private Function
-- | Build BeckonGeneratedFile
_getBeckonGenerated :: T.Text -> [T.Text] -> Either T.Text BeckonGeneratedFile
_getBeckonGenerated beckonModuleName [src, spec] =
  let srcFile =
        BeckonFile {target = N.getSrcFilePath beckonModuleName, content = src}
      specFile =
        BeckonFile {target = N.getSpecFilePath beckonModuleName, content = spec}
  in  Right BeckonGeneratedService {srcFile = srcFile, specFile = specFile}
_getBeckonGenerated beckonModuleName [src, tmpl, spec] =
  let srcFile =
        BeckonFile {target = N.getSrcFilePath beckonModuleName, content = src}
      tmplFile =
        BeckonFile {target = N.getTmplFilePath beckonModuleName, content = tmpl}
      specFile =
        BeckonFile {target = N.getSpecFilePath beckonModuleName, content = spec}
  in  Right BeckonGeneratedComponent
        { srcFile  = srcFile
        , tmplFile = tmplFile
        , specFile = specFile
        }
_getBeckonGenerated _ _ = Left "Failed to parse template"

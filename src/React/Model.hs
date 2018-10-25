{-# LANGUAGE OverloadedStrings #-}

module React.Model
  ( ReactMustache(..)
  , parseReactMustache
  , ReactOption(..)
  , ReactGenType(..)
  , lowerCaseFirst
  , ReactFiles(..)
  ) where

import qualified Common.BeckonFile as BeckonFile
import Data.Char (toUpper)
import Data.Function ((&))
import qualified Data.Text as T
import Text.Mustache ((~>))
import qualified Text.Mustache

data ReactOption = ReactOption
  { react_name :: T.Text -- ^ "atoms/Temp/BrPageHeader"
  , react_genTypes :: [ReactGenType] -- ^ Src / Spec / Story
  , react_isForce :: Bool -- ^ If True, overwrite
  } deriving (Show)

data ReactGenType
  = Src
  | Spec
  | Story
  deriving (Show)

data ReactMustache = ReactMustache
  { react_moduleName :: T.Text -- ^ Atoms
  , react_componentName :: T.Text -- ^ BrPageHeader
  , react_fullPath :: [T.Text] -- ^ ["Atoms", "Temp1", "BrPageHeader"]
  } deriving (Show, Eq)

instance Text.Mustache.ToMustache ReactMustache where
  toMustache reactMustache =
    Text.Mustache.object
      [ "ModuleName" ~> upperCaseFirstLetter (react_moduleName reactMustache)
      , "ComponentName" ~> react_componentName reactMustache
      ]

upperCaseFirstLetter :: T.Text -> T.Text
upperCaseFirstLetter s
  | T.null s = s
  | otherwise = T.cons (toUpper (T.head s)) (T.tail s)

-- | Given modulename/ComponentName, return ReactMustache
parseReactMustache :: T.Text -> Either T.Text ReactMustache
parseReactMustache text = do
  nameList <- text & T.replace "/" "." & T.splitOn "." & lowerCaseFirst
  handleModuleName nameList

-- | TitleCase First Item
--
-- >>> lowerCaseFirst [ "aToms" ]
-- Right ["atoms"]
lowerCaseFirst :: [T.Text] -> Either T.Text [T.Text]
lowerCaseFirst [] = Left "No module name. Try something like atoms/BrPageHeader"
lowerCaseFirst (head:rest) = Right (T.toLower head : map upperCaseFirstLetter rest)


handleModuleName :: [T.Text] -> Either T.Text ReactMustache
handleModuleName raw@(moduleName:rest) =
  if moduleName `elem` ["atoms", "molecules", "organisms"]
    then checkComponentAndReturn rest
    else Left $
         moduleName <>
         " is not a valid module name. Module name should be one of atoms/molecules/organisms"
  where
    checkComponentAndReturn :: [T.Text] -> Either T.Text ReactMustache
    checkComponentAndReturn [] = Left "No component name is given"
    checkComponentAndReturn [x] = Right $ ReactMustache moduleName x raw
    checkComponentAndReturn (x:xs) = checkComponentAndReturn xs

-- | Basic Data Structure after parsed from Mustache Template
data ReactFiles = ReactFiles
  { srcFile :: BeckonFile.BeckonFile
  , specFile :: BeckonFile.BeckonFile
  , storyFile :: BeckonFile.BeckonFile
  }

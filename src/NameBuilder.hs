{-# LANGUAGE OverloadedStrings #-}

module NameBuilder where

import qualified Config as C
import Data.Char (isUpper, toLower)
import qualified Data.Text as T
import qualified System.FilePath.Posix as F
import System.FilePath.Posix ((<.>), (</>))

-- moduleName : beckon.steel.answerPage
-- componentName: answerPage
-- component-name: answer-page
isModuleName :: T.Text -> Bool
isModuleName xs =
  length splitText >= 2 && isBeckonModule (getPossibleModuleName splitText)
  where
    splitText :: [T.Text]
    splitText = T.splitOn "." xs

getPossibleModuleName :: [T.Text] -> T.Text
getPossibleModuleName ("beckon":x:xs) = x
getPossibleModuleName (x:xs) = x

isBeckonModule :: T.Text -> Bool
isBeckonModule name = T.toLower name `elem` ["steel", "tinder", "flint", "viz"]

getComponentName :: T.Text -> T.Text
getComponentName = last . T.splitOn "."

get_ComponentName :: T.Text -> T.Text
get_ComponentName x = go (T.splitAt 1 (getComponentName x))
  where
    go (first, all) = T.toUpper first <> all

toHypenCase :: T.Text -> T.Text
toHypenCase =
  T.concatMap
    (\c ->
       if isUpper c
         then "-" <> (T.singleton (toLower c))
         else T.singleton c)

stripBeckonFromModuleName :: T.Text -> T.Text
stripBeckonFromModuleName = T.replace "beckon." ""

getSrcFilePath :: T.Text -> F.FilePath
getSrcFilePath moduleName =
  C.srcDirectory </> someName </> componentName <.> ".js"
  where
    someName :: F.FilePath
    someName =
      T.unpack $ T.replace "." "/" (stripBeckonFromModuleName moduleName)
    componentName :: F.FilePath
    componentName =
      T.unpack $ getComponentName (stripBeckonFromModuleName moduleName)

getTmplFilePath :: T.Text -> F.FilePath
getTmplFilePath moduleName =
  C.srcDirectory </> someName </> componentName <.> ".tmpl"
  where
    someName :: F.FilePath
    someName =
      T.unpack $ T.replace "." "/" (stripBeckonFromModuleName moduleName)
    componentName :: F.FilePath
    componentName =
      T.unpack $ getComponentName (stripBeckonFromModuleName moduleName)

getSpecFilePath :: T.Text -> F.FilePath
getSpecFilePath moduleName =
  C.testDirectory </> someName </> componentName <> "Spec" <.> ".js"
  where
    someName :: F.FilePath
    someName =
      T.unpack $ T.replace "." "/" (stripBeckonFromModuleName moduleName)
    componentName :: F.FilePath
    componentName =
      T.unpack $ getComponentName (stripBeckonFromModuleName moduleName)

addBeckonPrefix :: T.Text -> T.Text
addBeckonPrefix moduleName = "beckon." <> (stripBeckonFromModuleName moduleName)

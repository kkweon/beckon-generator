{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : NameBuilder
Description : Parse module name and generate component names
Copyright   : (c) Mo Kweon
License     : MIT
Maintainer  : kkweon@gmail.com
Stability   : experimental
Portability : POSIX
-}
module NameBuilder where

import qualified FileType
import qualified Config                        as C
import           Data.Char                      ( isUpper
                                                , toLower
                                                )
import qualified Data.Text                     as T
import qualified System.FilePath.Posix         as F
import           System.FilePath.Posix          ( (<.>)
                                                , (</>)
                                                )

-- | Check if it's a valid module name
--
-- >>> isModuleName "beckon.steel.answerPage"
-- True
--
-- >>> isModuleName "steel.answerPage"
-- True
--
-- >>> isModuleName "some.answerPage"
-- False
--
-- @{prefix}.{name}@ or @beckon.{prefix}.{name}@
-- and you can add nested files by using more dots
--
-- For example, @{prefix}.{name}.{name2}@ will generate @\/prefix\/name\/name2\/name2.js@
isModuleName :: T.Text -> Bool
isModuleName xs = length splitText >= 2 && isBeckonModule
  (getBeckonModuleName splitText)
 where
  splitText :: [T.Text]
  splitText = T.splitOn "." xs

-- | Get Actual Module Name
-- There are currently 4 special module names
--
-- * steel
-- * tinder
-- * flint
-- * viz
--
-- >>> getBeckonModuleName ["beckon", "steel", "answerPage"]
-- "steel"
--
-- >>> getBeckonModuleName ["steel", "answerPage"]
-- "steel"
getBeckonModuleName :: [T.Text] -> T.Text
getBeckonModuleName ("beckon" : x : xs) = x
getBeckonModuleName (x            : xs) = x

-- | Check if given 'name' is one of ["steel", "tinder", "flint", "viz"]
--
-- >>> isBeckonModule "steel"
-- True
isBeckonModule :: T.Text -> Bool
isBeckonModule name = T.toLower name `elem` ["steel", "tinder", "flint", "viz"]

-- | Given module name, returns a componentName
--
-- >>> getComponentName "steel.appPage"
-- "appPage"
getComponentName :: T.Text -> T.Text
getComponentName = fixHypen . last . T.splitOn "."
 where
  fixHypen :: T.Text -> T.Text
  fixHypen = T.foldr
    (\c acc -> if c == '-' then toUpperFirstLetter acc else T.cons c acc)
    ""

-- | Returns a CamelCase name
--
-- >>> get_ComponentName "steel.appPage"
-- "AppPage"
get_ComponentName :: T.Text -> T.Text
get_ComponentName = toUpperFirstLetter . getComponentName

-- | Internal helper function. Capitalize
--
-- >>> toUpperFirstLetter "helloWorld"
-- "HelloWorld"
toUpperFirstLetter :: T.Text -> T.Text
toUpperFirstLetter = go . T.splitAt 1
  where go (first, all) = T.toUpper first <> all

-- | Internal helper function. Convert camelCase into hypen-case
--
-- >>> toHypenCase "appPage"
-- "app-page"
toHypenCase :: T.Text -> T.Text
toHypenCase = T.concatMap
  (\c -> if isUpper c then "-" <> T.singleton (toLower c) else T.singleton c)

-- | Remove "beckon." from module name to normalize
--
-- >>> stripBeckonFromModuleName "beckon.steel.answerPage"
-- "steel.answerPage"
--
-- >>> stripBeckonFromModuleName "steel.answerPage"
-- "steel.answerPage"
stripBeckonFromModuleName :: T.Text -> T.Text
stripBeckonFromModuleName = T.replace "beckon." ""

-- | Add "beckon." prefix to module name
--
-- >>> addBeckonPrefix "steel.answerPage"
-- "beckon.steel.answerPage"
--
-- >>> addBeckonPrefix "beckon.steel.answerPage"
-- "beckon.steel.answerPage"
addBeckonPrefix :: T.Text -> T.Text
addBeckonPrefix moduleName = "beckon." <> stripBeckonFromModuleName moduleName

-- | Returns a JS src file path
--
-- >>> getSrcFilePath "steel.answerPage"
-- "./src/main/resources/com/beckon/steel/answerPage/answerPage.js"
getSrcFilePath :: FileType.FileType -> T.Text -> F.FilePath
getSrcFilePath fileType moduleName =
  C.srcDirectory
    </> getNamespaceFilePath moduleName
    </> getFileName moduleName
    <.> getExtension fileType
 where
  getExtension :: FileType.FileType -> String
  getExtension FileType.OldTypeScript = ".ts"
  getExtension FileType.JavaScript    = ".js"

-- | Returns a HTML tmpl file path
--
-- >>> getTmplFilePath "steel.answerPage"
-- "./src/main/resources/com/beckon/steel/answerPage/answerPage.tmpl"
getTmplFilePath :: T.Text -> F.FilePath
getTmplFilePath moduleName =
  C.srcDirectory
    </> getNamespaceFilePath moduleName
    </> getFileName moduleName
    <.> ".tmpl"

-- | Returns a JS Spec file path
--
-- >>> getSpecFilePath "steel.answerPage"
-- "./src/test/javascript/unit/steel/answerPage/answerPageSpec.js"
getSpecFilePath :: T.Text -> F.FilePath
getSpecFilePath moduleName =
  C.testDirectory
    </> getNamespaceFilePath moduleName
    </> getFileName moduleName
    <>  "Spec"
    <.> ".js"

-- | From Module name to get namespace name
--
-- >>> getNamespaceFilePath "beckon.steel.answerPage"
-- "steel/answerPage"
getNamespaceFilePath :: T.Text -> F.FilePath
getNamespaceFilePath = T.unpack . T.replace "." "/" . stripBeckonFromModuleName


getFileName :: T.Text -> F.FilePath
getFileName = T.unpack . getComponentName . stripBeckonFromModuleName

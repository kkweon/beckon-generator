{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module React.NameBuilder
  ( getSrcFilePath
  , getSpecFilePath
  , getStoryFilePath
  ) where

import Data.Function ((&))
import qualified Data.Text as T
import qualified React.Model as Model
import qualified System.FilePath.Posix as Posix
import System.FilePath.Posix ((<.>), (</>))

filePathPrefix :: String
filePathPrefix = "./src"

getSrcFilePath :: Model.ReactMustache -> String
getSrcFilePath Model.ReactMustache {Model.react_fullPath} =
  react_fullPath & getReactBaseFilePath & (<.> ".tsx")

getSpecFilePath :: Model.ReactMustache -> String
getSpecFilePath Model.ReactMustache {Model.react_fullPath} =
  react_fullPath & getReactBaseFilePath & (\x -> x <.> "spec" <.> ".tsx")

getStoryFilePath :: Model.ReactMustache -> String
getStoryFilePath Model.ReactMustache {Model.react_fullPath} =
  react_fullPath & getReactBaseFilePath & (\x -> x <.> "stories" <.> ".js")

getReactBaseFilePath :: [T.Text] -> String
getReactBaseFilePath react_fullPath =
  react_fullPath & T.intercalate "/" & T.unpack &
  (\x -> filePathPrefix </> x </> Posix.takeBaseName x)

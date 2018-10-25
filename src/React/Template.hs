{-# LANGUAGE TemplateHaskell, OverloadedStrings, RecordWildCards
  #-}

module React.Template
  ( reactComponentTemplate
  , getReactFilesAfterMustache
  ) where

import qualified Common.BeckonFile as BeckonFile
import qualified Common.FileType as FileType
import qualified Data.Text as T
import qualified React.Model as Model
import qualified React.NameBuilder as NameBuilder
import qualified System.FilePath.Posix as Posix
import qualified Text.Mustache as Mustache
import Text.Mustache.Compile (embedSingleTemplate)

reactComponentTemplate :: Mustache.Template
reactComponentTemplate =
  $(embedSingleTemplate "templates/react/component.tsx.mustache")

breakKey :: T.Text
breakKey = "####"

getReactFilesAfterMustache ::
     Model.ReactMustache -> Either T.Text Model.ReactFiles
getReactFilesAfterMustache reactMustache =
  handleTemplate $ map T.strip $ T.splitOn breakKey text
  where
    text :: T.Text
    text = Mustache.substitute reactComponentTemplate reactMustache
    handleTemplate :: [T.Text] -> Either T.Text Model.ReactFiles
    handleTemplate [srcContent, specContent, storyContent] =
      let srcFile =
            BeckonFile.BeckonFile
              (NameBuilder.getSrcFilePath reactMustache)
              srcContent
              FileType.TypeScript
          specFile =
            BeckonFile.BeckonFile
              (NameBuilder.getSpecFilePath reactMustache)
              specContent
              FileType.TypeScript
          storyFile =
            BeckonFile.BeckonFile
              (NameBuilder.getStoryFilePath reactMustache)
              storyContent
              FileType.JavaScript
       in Right (Model.ReactFiles {..})
    handleTemplate _ =
      Left $
      "Unable to parse React template with a given name: " <>
      T.pack (show reactMustache)

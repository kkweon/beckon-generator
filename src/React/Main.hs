{-# LANGUAGE Arrows, NamedFieldPuns, OverloadedStrings #-}

module React.Main
  ( reactOptionParser
  , handleReactOption
  ) where

import qualified Common.BeckonFile as BeckonFile
import qualified Common.FileIO as FileIO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Options.Applicative as A
import qualified Options.Applicative.Arrows as Arrows
import qualified React.Model as Model
import qualified React.Template as Template

reactOptionParser :: A.Parser Model.ReactOption
reactOptionParser =
  Arrows.runA $
  proc () ->
  do react_name <- Arrows.asA
                     (A.strArgument
                        (A.metavar "module/component" <>
                           A.help
                             "Module name can be atoms/molecules/organisms. (e.g., atoms/BrPageHeader)"))
                     -< ()
     defaultType <- Arrows.asA
                      (A.flag [Model.Src] []
                         (A.long "no-src" <> A.help "Do not generate a src file"))
                      -< ()
     specType <- Arrows.asA
                   (A.flag [] [Model.Spec]
                      (A.long "spec" <> A.help "Generate a spec file"))
                   -< ()
     storyType <- Arrows.asA
                    (A.flag [] [Model.Story]
                       (A.long "story" <> A.help "Genereate a Storybook story file"))
                    -< ()
     react_isForce <- Arrows.asA
                        (A.switch
                           (A.long "force" <> A.short 'f' <>
                              A.help "Overwrite when a target file exists"))
                        -< ()
     let react_genTypes = defaultType ++ specType ++ storyType
     Arrows.returnA -<
       Model.ReactOption react_name react_genTypes react_isForce

-- | Reeact Generator Entry point
handleReactOption :: Model.ReactOption -> IO ()
handleReactOption Model.ReactOption { Model.react_isForce
                                    , Model.react_name
                                    , Model.react_genTypes
                                    } =
  case runMainReact react_isForce react_name react_genTypes of
    Right m -> m
    Left message -> TIO.putStrLn message

runMainReact :: Bool -> T.Text -> [Model.ReactGenType] -> Either T.Text (IO ())
runMainReact react_isForce react_name react_genTypes = do
  reactMustache <- Model.parseReactMustache react_name
  reactFiles <- Template.getReactFilesAfterMustache reactMustache
  let beckonFiles = map (getBeckonFile reactFiles) react_genTypes
  if null beckonFiles
    then Left "Unable to build files"
    else Right $ FileIO.parallelGenerateBeckonFiles react_isForce beckonFiles

getBeckonFile :: Model.ReactFiles -> Model.ReactGenType -> BeckonFile.BeckonFile
getBeckonFile Model.ReactFiles {Model.srcFile} Model.Src = srcFile
getBeckonFile Model.ReactFiles {Model.specFile} Model.Spec = specFile
getBeckonFile Model.ReactFiles {Model.storyFile} Model.Story = storyFile

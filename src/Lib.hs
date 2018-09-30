{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib where

import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified FileIO as F
import qualified NameBuilder as N
import qualified Options.Applicative as A
import qualified Template as TE

handleGenerateComponent :: Arg -> IO ()
handleGenerateComponent Arg {moduleName, specFile, isService} =
  let generatedType =
        if isService
          then TE.Service
          else TE.Component
   in case TE.getBeckonGeneratedFile generatedType moduleName of
        Left errorMsg -> TIO.putStrLn errorMsg
        Right beckonGenerated -> F.generateBeckonFiles specFile beckonGenerated

data Arg = Arg
  { moduleName :: T.Text
  , specFile :: Bool
  , isService :: Bool
  }

arg :: A.Parser Arg
arg =
  Arg <$>
  A.strArgument
    (A.metavar "MODULE NAME" <>
     A.help "Beckon Module Name (e.g., beckon.steel.answerPage)") <*>
  A.switch (A.long "spec" <> A.short 'S' <> A.help "Generate a spec file") <*>
  A.switch (A.long "service" <> A.help "Generate a service file")

greet :: Arg -> IO ()
greet arg = handleGenerateComponent arg

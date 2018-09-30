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

handleGenerateComponent :: Bool -> T.Text -> IO ()
handleGenerateComponent specFile maybeModuleName =
  case TE.getBeckonGeneratedComponent maybeModuleName of
    Left errorMsg -> TIO.putStrLn errorMsg
    Right beckonGenerated -> F.generateBeckonFiles specFile beckonGenerated

data Arg = Arg
  { moduleName :: T.Text
  , specFile :: Bool
  }

arg :: A.Parser Arg
arg =
  Arg <$>
  A.strArgument
    (A.metavar "MODULE NAME" <>
     A.help "Beckon Module Name (e.g., beckon.steel.answerPage)") <*>
  A.switch (A.long "spec" <> A.short 'S' <> A.help "Generate a spec file")

greet :: Arg -> IO ()
greet Arg {moduleName, specFile } = handleGenerateComponent specFile moduleName

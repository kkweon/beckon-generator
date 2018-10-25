module Main where

import qualified CommandLineParser as L
import Options.Applicative as OA

main :: IO ()
main = L.handleArgOptions =<< OA.execParser L.finalParserInfo

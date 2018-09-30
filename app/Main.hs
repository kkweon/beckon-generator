module Main where

import qualified Lib as L
import Options.Applicative as OA

main :: IO ()
main = L.greet =<< OA.execParser opts
  where
    opts =
      OA.info
        (L.arg <**> OA.helper)
        (OA.fullDesc <> OA.progDesc "Generate AngularJS Beckon Component" <>
         OA.header "Beckon AngularJS Component Generator")

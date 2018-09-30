{-# LANGUAGE OverloadedStrings #-}

module TemplateSpec
  ( spec
  ) where

import qualified Config as C
import Template (NgTemplate(..), BeckonFile(..), BeckonGenerated(..))
import qualified Template as T
import Test.Hspec
import qualified Text.Mustache as M
import qualified Data.Text as T

spec :: Spec
spec =
  describe "Template" $ do
    describe "test toMustache" $
      it "test mustache" $ do
        let ngTemplate =
              NgTemplate
                { moduleName = "beckon.steel.answerPage"
                , componentName = "answerPage"
                , component_name = "answer-page"
                }
        let result = M.substitute C.componentTemplate ngTemplate

        length (T.breakOnAll "module(\"beckon.steel.answerPage\")" result) > 0

    describe "getBeckonGeneratedComponent" $
        it "do something" $ do
            let result = T.getBeckonGeneratedComponent "steel.answerPage"

            case result of
                Right bg -> content (tmplFile bg) `shouldBe` "\n\n<div>\n  <h2>Hello answerPage!</h2>\n</div>\n\n"
                Left _ -> True `shouldBe` False

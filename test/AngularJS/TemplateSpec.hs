{-# LANGUAGE OverloadedStrings #-}

module AngularJS.TemplateSpec
  ( spec
  ) where

import qualified AngularJS.AngularJSType
import qualified AngularJS.Config as C
import AngularJS.Model (BeckonNgGeneratedFile(..))
import AngularJS.Template (NgTemplate(..))
import qualified AngularJS.Template as T
import qualified Common.BeckonFile
import qualified Common.FileType
import qualified Data.Text as T
import Test.Hspec
import qualified Text.Mustache as M

spec :: Spec
spec = do
  describe "test toMustache" $
    it "test mustache" $ do
      let ngTemplate =
            NgTemplate
              { moduleName = "beckon.steel.answerPage"
              , componentName = "answerPage"
              , _ComponentName = "AnswerPage"
              , component_name = "answer-page"
              }
      let result = M.substitute C.componentTemplate ngTemplate
      not . null $ T.breakOnAll "module(\"beckon.steel.answerPage\")" result
  describe "getBeckonGeneratedComponent" $
    it "do something" $ do
      let result =
            T.getBeckonGeneratedFile
              Common.FileType.JavaScript
              AngularJS.AngularJSType.Component
              "steel.answerPage"
      case result of
        Right bg ->
          Common.BeckonFile.content (tmplFile bg) `shouldBe`
          "<div>\n  <h2>Hello answer-page!</h2>\n</div>"
        Left _ -> True `shouldBe` False

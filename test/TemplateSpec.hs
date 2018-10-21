{-# LANGUAGE OverloadedStrings #-}

module TemplateSpec
  ( spec
  )
where

import qualified Config                        as C
import qualified Data.Text                     as T
import           Template                       ( BeckonFile(..)
                                                , BeckonGeneratedFile(..)
                                                , NgTemplate(..)
                                                )
import qualified Template                      as T
import           Test.Hspec
import qualified Text.Mustache                 as M

spec :: Spec
spec = do
  describe "test toMustache" $ it "test mustache" $ do
    let ngTemplate = NgTemplate
          { moduleName     = "beckon.steel.answerPage"
          , componentName  = "answerPage"
          , _ComponentName = "AnswerPage"
          , component_name = "answer-page"
          }
    let result = M.substitute C.componentTemplate ngTemplate
    length (T.breakOnAll "module(\"beckon.steel.answerPage\")" result) > 0
  describe "getBeckonGeneratedComponent" $ it "do something" $ do
    let result = T.getBeckonGeneratedFile T.Component "steel.answerPage"
    case result of
      Right bg ->
        content (tmplFile bg)
          `shouldBe` "<div>\n  <h2>Hello answer-page!</h2>\n</div>"
      Left _ -> True `shouldBe` False

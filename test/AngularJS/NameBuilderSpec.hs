{-# LANGUAGE OverloadedStrings #-}

module AngularJS.NameBuilderSpec
  ( spec
  ) where

import qualified AngularJS.NameBuilder as N
import qualified Common.FileType
import Test.Hspec

spec :: Spec
spec = do
  describe "isModuleName" $ do
    it "returns True if module name" $
      N.isModuleName "steel.answerPage" `shouldBe` True
    it "returns False if not module name" $ do
      N.isModuleName "beckon.steel" `shouldBe` True
      N.isModuleName "" `shouldBe` False
  describe "getComponentName" $ do
    it "returns a component name" $
      N.getComponentName "steel.answerPage" `shouldBe` "answerPage"
    it "returns a component name" $
      N.getComponentName "steel.answer-page" `shouldBe` "answerPage"
    it "returns a component name" $
      N.getComponentName "beckon.steel.answerPage" `shouldBe` "answerPage"
  describe "get_ComponentName" $ do
    it "returns a component name" $
      N.get_ComponentName "steel.answerPage" `shouldBe` "AnswerPage"
    it "returns a component name" $
      N.get_ComponentName "beckon.steel.answerPage" `shouldBe` "AnswerPage"
  describe "toHypenCase" $ do
    it "returns hypen-case when given hypenCase" $
      N.toHypenCase "hypenCase" `shouldBe` "hypen-case"
    it "returns hypen-case when given hypen-case" $
      N.toHypenCase "hypen-case" `shouldBe` "hypen-case"
  describe "addBeckonPrefix" $ do
    it "returns beckon module name" $
      N.addBeckonPrefix "beckon.steel.answerPage" `shouldBe`
      "beckon.steel.answerPage"
    it "returns beckon module name" $
      N.addBeckonPrefix "steel.answerPage" `shouldBe` "beckon.steel.answerPage"
  describe "getSrcFilePath" $ do
    it "returns a correct JS file path" $
      N.getSrcFilePath Common.FileType.JavaScript "steel.answerPage" `shouldBe`
      "./src/main/resources/com/beckon/steel/answerPage/answerPage.js"
    it "returns a correct JS file path" $
      N.getSrcFilePath Common.FileType.JavaScript "beckon.steel.answerPage" `shouldBe`
      "./src/main/resources/com/beckon/steel/answerPage/answerPage.js"
    it "returns a correct JS file path" $
      N.getSrcFilePath
        Common.FileType.JavaScript
        "beckon.steel.answerPage.answerPageStore" `shouldBe`
      "./src/main/resources/com/beckon/steel/answerPage/answerPageStore/answerPageStore.js"
    it "returns a correct TS file path" $
      N.getSrcFilePath
        Common.FileType.OldTypeScript
        "beckon.steel.answerPage.answerPageStore" `shouldBe`
      "./src/main/resources/com/beckon/steel/answerPage/answerPageStore/answerPageStore.ts"
  describe "getTmplFilePath" $ do
    it "returns a correct JS file path" $
      N.getTmplFilePath "steel.answerPage" `shouldBe`
      "./src/main/resources/com/beckon/steel/answerPage/answerPage.tmpl"
    it "returns a correct JS file path" $
      N.getTmplFilePath "beckon.steel.answerPage" `shouldBe`
      "./src/main/resources/com/beckon/steel/answerPage/answerPage.tmpl"
    it "returns a correct JS file path" $
      N.getTmplFilePath "beckon.steel.answerPage.answerPageStore" `shouldBe`
      "./src/main/resources/com/beckon/steel/answerPage/answerPageStore/answerPageStore.tmpl"
  describe "getSpecFilePath" $ do
    it "returns a correct JS file path" $
      N.getSpecFilePath "steel.answerPage" `shouldBe`
      "./src/test/javascript/unit/steel/answerPage/answerPageSpec.js"
    it "returns a correct JS file path" $
      N.getSpecFilePath "beckon.steel.answerPage" `shouldBe`
      "./src/test/javascript/unit/steel/answerPage/answerPageSpec.js"
    it "returns a correct JS file path" $
      N.getSpecFilePath "beckon.steel.answerPage.answerPageStore" `shouldBe`
      "./src/test/javascript/unit/steel/answerPage/answerPageStore/answerPageStoreSpec.js"

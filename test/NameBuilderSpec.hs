{-# LANGUAGE OverloadedStrings #-}

module NameBuilderSpec
  ( spec
  ) where

import qualified NameBuilder as N
import Test.Hspec

spec :: Spec
spec =
  describe "NameBuilder" $ do
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
        N.getComponentName "beckon.steel.answerPage" `shouldBe` "answerPage"

    describe "toHypenCase" $ do
      it "returns hypen-case when given hypenCase" $
        N.toHypenCase "hypenCase" `shouldBe` "hypen-case"
      it "returns hypen-case when given hypen-case" $
        N.toHypenCase "hypen-case" `shouldBe` "hypen-case"

    describe "addBeckonPrefix" $ do
      it "returns beckon module name" $
        N.addBeckonPrefix "beckon.steel.answerPage" `shouldBe` "beckon.steel.answerPage"
      it "returns beckon module name" $
        N.addBeckonPrefix "steel.answerPage" `shouldBe` "beckon.steel.answerPage"

    describe "getSrcFilePath" $ do
      it "returns a correct JS file path" $
        N.getSrcFilePath "steel.answerPage" `shouldBe`
        "./src/main/resources/com/beckon/steel/answerPage/answerPage.js"
      it "returns a correct JS file path" $
        N.getSrcFilePath "beckon.steel.answerPage" `shouldBe`
        "./src/main/resources/com/beckon/steel/answerPage/answerPage.js"
      it "returns a correct JS file path" $
        N.getSrcFilePath "beckon.steel.answerPage.answerPageStore" `shouldBe`
        "./src/main/resources/com/beckon/steel/answerPage/answerPageStore/answerPageStore.js"

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

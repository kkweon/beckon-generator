{-# LANGUAGE OverloadedStrings #-}

module React.ModelSpec
  ( spec
  ) where

import qualified React.Model as Model
import Test.Hspec

spec :: Spec
spec = do
  describe "parseReactMustache" $ do
    it "returns a coorect ReactMustache" $
      Model.parseReactMustache "Atoms/BrPageHeader" `shouldBe`
      Right
        (Model.ReactMustache "atoms" "BrPageHeader" ["atoms", "BrPageHeader"])
    it "returns a coorect ReactMustache" $
      Model.parseReactMustache "atoms/Temp/BrPageHeader" `shouldBe`
      Right
        (Model.ReactMustache
           "atoms"
           "BrPageHeader"
           ["atoms", "Temp", "BrPageHeader"])
    it "returns an error message" $
      Model.parseReactMustache "Temp/BrPageHeader" `shouldBe`
      Left
        "temp is not a valid module name. Module name should be one of atoms/molecules/organisms"
    it "returns an error message" $
      Model.parseReactMustache "BrPageHeader" `shouldBe`
      Left
        "brpageheader is not a valid module name. Module name should be one of atoms/molecules/organisms"
  describe "titleCaseFirst" $
    it "should title case the first element" $
    Model.lowerCaseFirst ["aToms"] `shouldBe` Right ["atoms"]

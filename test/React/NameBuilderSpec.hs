{-# LANGUAGE OverloadedStrings #-}

module React.NameBuilderSpec
  ( spec
  ) where

import qualified React.Model as M
import qualified React.NameBuilder as N
import Test.Hspec

mockReactMustache :: M.ReactMustache
mockReactMustache =
  M.ReactMustache
    { M.react_moduleName = "atoms"
    , M.react_componentName = "BrPageHeader"
    , M.react_fullPath = ["atoms", "BrPageHeader"]
    }

mockReactMustache2 :: M.ReactMustache
mockReactMustache2 =
  M.ReactMustache
    { M.react_moduleName = "atoms"
    , M.react_componentName = "BrPageHeader"
    , M.react_fullPath = ["atoms", "Temp", "BrPageHeader"]
    }

spec :: Spec
spec =
  describe "getSrcFilePath" $ do
    it "should return a correct src file path" $ do
      N.getSrcFilePath mockReactMustache `shouldBe`
        "./src/atoms/BrPageHeader/BrPageHeader.tsx"
      N.getSrcFilePath mockReactMustache2 `shouldBe`
        "./src/atoms/Temp/BrPageHeader/BrPageHeader.tsx"
    it "should return a correct src file path" $ do
      N.getStoryFilePath mockReactMustache `shouldBe`
        "./src/atoms/BrPageHeader/BrPageHeader.stories.js"
      N.getStoryFilePath mockReactMustache2 `shouldBe`
        "./src/atoms/Temp/BrPageHeader/BrPageHeader.stories.js"
    it "should return a correct src file path" $ do
      N.getSpecFilePath mockReactMustache `shouldBe`
        "./src/atoms/BrPageHeader/BrPageHeader.spec.tsx"
      N.getSpecFilePath mockReactMustache2 `shouldBe`
        "./src/atoms/Temp/BrPageHeader/BrPageHeader.spec.tsx"

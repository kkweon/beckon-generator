{-# LANGUAGE OverloadedStrings #-}

module React.TemplateSpec
  ( spec
  ) where

import qualified Common.BeckonFile as BeckonFile
import qualified Data.Text as T
import qualified React.Model as Model
import qualified React.Template as Template
import Test.Hspec
import qualified Text.Mustache as Mustache

spec :: Spec
spec = do
  describe "parse template" $
    it "should parse mustache" $ do
      let reactMustache =
            case Model.parseReactMustache "atoms/BrPageHeader" of
              Right r -> r
              _ -> undefined
      let resultText =
            Mustache.substitute Template.reactComponentTemplate reactMustache
      not . null $ T.breakOnAll "<BrPageHeader />" resultText
  describe "parseReactMustache" $ do
    it "should return ReactFiles" $ do
      let reactMustache =
            case Model.parseReactMustache "atoms/BrPageHeader" of
              Right r -> r
              _ -> undefined
      let result =
            case Template.getReactFilesAfterMustache reactMustache of
              Right r -> r
              _ -> undefined
      BeckonFile.target (Model.srcFile result) `shouldBe`
        "./src/atoms/BrPageHeader/BrPageHeader.tsx"
      BeckonFile.target (Model.specFile result) `shouldBe`
        "./src/atoms/BrPageHeader/BrPageHeader.spec.tsx"
      BeckonFile.target (Model.storyFile result) `shouldBe`
        "./src/atoms/BrPageHeader/BrPageHeader.stories.js"
      BeckonFile.content (Model.srcFile result) `shouldBe`
        "import * as React from \"react\";\nimport { ReactChild, ReactChildren } from \"react\";\n\ninterface Props {\n    children: ReactChildren | ReactChild;\n}\n\nexport const BrPageHeader = (props: Props) => (\n    <div>\n        <h2>BrPageHeader is working</h2>\n    </div>\n);"
      BeckonFile.content (Model.specFile result) `shouldBe`
        "import * as React from \"react\";\nimport { BrPageHeader } from \"./BrPageHeader\";\nimport { shallow } from \"enzyme\";\n\ntest(\"renders BrPageHeader correctly\", () => {\n    const tree = shallow(\n        <BrPageHeader />\n    );\n    expect(tree).toMatchSnapshot();\n});"
      BeckonFile.content (Model.storyFile result) `shouldBe`
        "import React from \"react\";\n\nimport { storiesOf } from \"@storybook/react\";\nimport { BrPageHeader } from \"./BrPageHeader\";\n\nimport { action } from \"@storybook/addon-actions\";\nimport { text, boolean } from \"@storybook/addon-knobs\";\n\nstoriesOf(\"Atoms\", module).addWithJSX(`BrPageHeader`, () => (\n    <BrPageHeader />\n));"

    it "should convert to FirstLetterUpperCase" $ do
      let reactMustache =
            case Model.parseReactMustache "atoms/temp/brPageHeader" of
              Right r -> r
              _ -> undefined
      let result =
            case Template.getReactFilesAfterMustache reactMustache of
              Right r -> r
              _ -> undefined
      BeckonFile.target (Model.srcFile result) `shouldBe`
        "./src/atoms/Temp/BrPageHeader/BrPageHeader.tsx"

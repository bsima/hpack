{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Hpack.ExtensionSpec (spec) where

import           Helper
import           System.Directory

import           Hpack.Extension

spec :: Spec
spec = do
  describe "parseDefaults" $ do
    context "when input contains a single dash" $ do
      it "parses a user and a ref" $ do
        parseDefaults "haskell-2017" `shouldBe` Just (Defaults "haskell" "2017")

    context "when input contains multiple dashes" $ do
      it "assumes that the user contains all the additional dashes" $ do
        parseDefaults "haskell-compat-v1" `shouldBe` Just (Defaults "haskell-compat" "v1")

    context "when input does not contain any dashes" $ do
      it "rejects" $ do
        parseDefaults "haskell" `shouldBe` Nothing

    context "when user is null" $ do
      it "rejects" $ do
        parseDefaults "-2017" `shouldBe` Nothing

    context "when ref is null" $ do
      it "rejects" $ do
        parseDefaults "haskell-" `shouldBe` Nothing

  describe "ensureFile" $ do
    let
      file = "foo"
      url = "https://raw.githubusercontent.com/sol/hpack/master/Setup.lhs"

    it "downloads file if missing" $ do
      expected <- readFile "Setup.lhs"
      inTempDirectory $ do
        Found <- ensureFile file url
        readFile file `shouldReturn` expected

    context "with existing file" $ do
      it "does nothing" $ do
        let expected = "contents of existing file"
        inTempDirectory $ do
          writeFile file expected
          Found <- ensureFile file url
          readFile file `shouldReturn` expected

    context "with 404" $ do
      let
        url = "https://raw.githubusercontent.com/sol/hpack/master/Setup.foo"

      it "does not create any files" $ do
        inTempDirectory $ do
          NotFound <- ensureFile file url
          doesFileExist file `shouldReturn` False

{-# LANGUAGE QuasiQuotes #-}

module MarkupParsingSpec where

import Lib.Markup
import Test.Hspec
import Text.RawString.QQ

example1 :: String
example1 =
  [r|
# Heading 

A paragraph 

- Some 
- unordered 
- points

## Heading number 2

The end

|]

exampleResult1 :: Document
exampleResult1 =
  [ Heading H1 "Heading",
    Paragraph "A paragraph",
    UnorderedList ["Some", "unordered", "points"],
    Heading H2 "Heading number 2",
    Paragraph "The end"
  ]

spec :: Spec
spec = do
  describe "Markup parsing test" $ do
    parallel simple
    parallel multiline

multiline :: Spec
multiline = do
  describe "multiline document tests" $ do
    it "Example doc 1" $
      shouldBe
        (parse example1)
        exampleResult1

simple :: Spec
simple = do
  describe "simple tests" $ do
    it "empty" $
      shouldBe
        (parse "")
        []
    it "paragraph" $
      shouldBe
        (parse "hello world")
        [Paragraph "hello world"]
    it "Heading 1" $
      shouldBe
        (parse "# Yolo")
        [Heading H1 "Yolo"]
    it "List" $
      shouldBe
        (parse "- this is a list point")
        [UnorderedList ["this is a list point"]]

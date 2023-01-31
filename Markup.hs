module Markup (Document, Structure (..), HeadingLevel (..), parse) where

import Data.Maybe (maybeToList)

type Document = [Structure]

data HeadingLevel = H1 | H2 | H3 deriving (Eq, Show)

data Structure
  = Heading HeadingLevel String
  | Paragraph String
  | UnorderedList [String]
  deriving (Eq, Show)

-- In case I want something custom with show, I can do this.
-- instance Show Structure where
--   show x =
--     case x of
--       Heading n s -> s
--       Paragraph s -> s
--       UnorderedList s -> concat s
--       CodeBlock s -> concat s
--       OrderedList n s -> concat s

parse :: String -> Document
parse = parseLines Nothing . lines

-- Recursion with state
parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    [] -> maybeToList context
    -- headings
    ('#' : ' ' : line) : rest ->
      maybe id (:) context (Heading H1 (trim line) : parseLines Nothing rest)
    ('#' : '#' : ' ' : line) : rest ->
      maybe id (:) context (Heading H2 (trim line) : parseLines Nothing rest)
    ('#' : '#' : '#' : ' ' : line) : rest ->
      maybe id (:) context (Heading H3 (trim line) : parseLines Nothing rest)
    -- unordered list
    ('-' : ' ' : line) : rest ->
      if trim line == ""
        then maybe id (:) context (parseLines Nothing rest)
        else case context of
          Just (UnorderedList prev) ->
            parseLines (Just $ UnorderedList $ prev <> [trim line]) rest
          _ -> maybe id (:) context (parseLines (Just $ UnorderedList [trim line]) rest)
    -- Para
    currLine : rest ->
      let line = trim currLine
       in if line == ""
            then maybe id (:) context (parseLines Nothing rest)
            else case context of
              Just (Paragraph para) ->
                parseLines (Just $ Paragraph (unwords [para, line])) rest
              _ ->
                maybe id (:) context (parseLines (Just $ Paragraph line) rest)

trim :: String -> String
trim = unwords . words

module Markup (Document, Structure (..)) where

import Data.Maybe (maybeToList)
import Numeric.Natural (Natural)

type Document = [Structure]

data Structure
  = Heading Natural String
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

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    [] -> maybeToList context
    -- headings
    ('#' : ' ' : line) : rest ->
      maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)
    ('#' : '#' : ' ' : line) : rest ->
      maybe id (:) context (Heading 2 (trim line) : parseLines Nothing rest)
    ('#' : '#' : '#' : ' ' : line) : rest ->
      maybe id (:) context (Heading 3 (trim line) : parseLines Nothing rest)
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

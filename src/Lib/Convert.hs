module Lib.Convert (mdToHTML) where

import Lib.Html.Internal qualified as H
import Lib.Markup qualified as M

mdToHTML :: H.Title -> M.Document -> H.Html
mdToHTML title doc = H.html_ title (foldMap convertMarkup doc)

convertMarkup :: M.Structure -> H.Structure
convertMarkup markup =
  case markup of
    M.Heading n txt ->
      case n of
        M.H1 -> H.h1_ txt
        M.H2 -> H.h2_ txt
        M.H3 -> H.h3_ txt
    M.Paragraph txt -> H.p_ txt
    M.UnorderedList list -> H.ul_ (map H.Structure list)

-- my tail recursive implementation
-- mdToHTML :: H.Title -> M.Document -> H.Html
-- mdToHTML t d = H.html_ t (makeStructure mempty d)
--   where
--     makeStructure :: H.Structure -> M.Document -> H.Structure
--     makeStructure acc doc =
--       case doc of
--         [] -> acc
--         (curr : rest) ->
--           case curr of
--             M.Heading n txt ->
--               case n of
--                 M.H1 -> makeStructure (acc <> H.h1_ txt) rest
--                 M.H2 -> makeStructure (acc <> H.h2_ txt) rest
--                 M.H3 -> makeStructure (acc <> H.h3_ txt) rest
--             M.Paragraph txt -> makeStructure (acc <> H.p_ txt) rest
--             M.UnorderedList list -> makeStructure (acc <> H.ul_ (map H.Structure list)) rest

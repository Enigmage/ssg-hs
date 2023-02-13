module Lib.Convert (markupToHtml, convertMarkup) where

import Lib.Html.Internal qualified as H
import Lib.Markup qualified as M

markupToHtml :: H.Title -> M.Document -> H.Html
markupToHtml title doc = H.html_ title (foldMap convertMarkup doc)

convertMarkup :: M.Structure -> H.Structure
convertMarkup markup =
  case markup of
    M.Heading n txt ->
      case n of
        M.H1 -> H.h1_ (H.txt_ txt)
        M.H2 -> H.h2_ (H.txt_ txt)
        M.H3 -> H.h3_ (H.txt_ txt)
    M.Paragraph txt -> H.p_ (H.txt_ txt)
    M.UnorderedList list -> H.ul_ (map H.Structure list)


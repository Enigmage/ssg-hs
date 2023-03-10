module Lib.Convert (markupToHtml, convertMarkup) where

import Lib.Env (Env (..))
import Lib.Html.Internal qualified as H
import Lib.Markup qualified as M
import Prelude hiding (head)

markupToHtml :: Env -> H.Title -> M.Document -> H.Html
markupToHtml env title doc =
  let head =
        H.title_ (eBlogName env <> " - " <> title)

      article = foldMap convertMarkup doc

      websiteTitle =
        H.h1_ (H.link_ "index.html" $ H.txt_ $ eBlogName env)

      body =
        websiteTitle <> article
   in H.html_ head body

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

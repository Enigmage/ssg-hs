module Lib.Html.Internal where

-- import Debug.Trace (traceShowId)

newtype Html = Html String

newtype Structure = Structure String

instance Semigroup Structure where
  (<>) x y = Structure $ getStructureContent x <> getStructureContent y

instance Monoid Structure where
  mempty = Structure ""

type Title = String

getStructureContent :: Structure -> String
getStructureContent (Structure x) = x

render :: Html -> String
render (Html x) = x

html_ :: Title -> Structure -> Html
html_ title content = Html . el "html" $ el "head" (el "title" $ escape title) <> el "body" (getStructureContent content)

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

h2_ :: String -> Structure
h2_ = Structure . el "h2" . escape

h3_ :: String -> Structure
h3_ = Structure . el "h3" . escape

p_ :: String -> Structure
p_ = Structure . el "p" . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . getHtmlList

-- ol_ :: [Structure] -> Structure
-- ol_ = Structure . el "ol" . getHtmlList
--
-- code_ :: String -> Structure
-- code_ = Structure . el "pre" . escape

getHtmlList :: [Structure] -> String
getHtmlList = concatMap (el "li" . getStructureContent)

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
   in concatMap escapeChar

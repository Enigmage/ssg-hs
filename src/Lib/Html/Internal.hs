module Lib.Html.Internal where

-- import Debug.Trace (traceShowId)

newtype Html = Html String

newtype Structure = Structure String

newtype Content = Content String deriving (Show)

instance Semigroup Structure where
  (<>) x y = Structure $ getStructureContent x <> getStructureContent y

instance Monoid Structure where
  mempty = Structure ""

type Title = String

getStructureContent :: Structure -> String
getStructureContent (Structure x) = x

getContentString :: Content -> String
getContentString (Content x) = x

render :: Html -> String
render (Html x) = x

html_ :: Title -> Structure -> Html
html_ title content = Html . el "html" $ el "head" (el "title" $ escape title) <> el "body" (getStructureContent content)

h1_ :: Content -> Structure
h1_ = Structure . el "h1" . escape . getContentString

h2_ :: Content -> Structure
h2_ = Structure . el "h2" . escape . getContentString

h3_ :: Content -> Structure
h3_ = Structure . el "h3" . escape . getContentString

p_ :: Content -> Structure
p_ = Structure . el "p" . escape . getContentString

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . getHtmlList

-- ol_ :: [Structure] -> Structure
-- ol_ = Structure . el "ol" . getHtmlList
--
-- code_ :: String -> Structure
-- code_ = Structure . el "pre" . escape

getHtmlList :: [Structure] -> String
getHtmlList = concatMap (el "li" . getStructureContent)

txt_ :: String -> Content
txt_ = Content . escape

img_ :: FilePath -> String -> Content
img_ src alt = Content $ "<img src=\"" <> src <> "\" alt=\"" <> alt <> "\">"

link_ :: FilePath -> Content -> Content
link_ src text = Content $ elWithAttr "a" ("href=\"" <> src <> "\"") (getContentString text)

b_ :: Content -> Content
b_ text = Content $ el "b" (getContentString text)

i_ :: Content -> Content
i_ text = Content $ el "i" (getContentString text)

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

elWithAttr :: String -> String -> String -> String
elWithAttr tag attr content = "<" <> tag <> " " <> attr <> ">" <> content <> "</" <> tag <> ">"

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

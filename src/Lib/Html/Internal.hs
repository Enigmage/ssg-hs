module Lib.Html.Internal where

-- import Debug.Trace (traceShowId)

newtype Html = Html String

newtype Head = Head String

newtype Structure = Structure String

newtype Content = Content String deriving (Show)

instance Semigroup Head where
  (<>) (Head h1) (Head h2) = Head (h1 <> h2)

instance Monoid Head where
  mempty = Head ""

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

html_ :: Head -> Structure -> Html
html_ (Head header) content = Html . el "html" $ el "head" header <> el "body" (getStructureContent content)

title_ :: String -> Head
title_ = Head . el "title" . escape

stylesheet_ :: FilePath -> Head
stylesheet_ path = Head $ "<link rel=\"stylesheet\" type=\"text/css\" href=\"" <> escape path <> "\">"

meta_ :: String -> String -> Head
meta_ name content =
  Head $ "<meta name=\"" <> escape name <> "\" content=\"" <> escape content <> "\">"

h1_ :: Content -> Structure
h1_ = Structure . el "h1" . getContentString

h2_ :: Content -> Structure
h2_ = Structure . el "h2" . getContentString

h3_ :: Content -> Structure
h3_ = Structure . el "h3" . getContentString

p_ :: Content -> Structure
p_ = Structure . el "p" . getContentString

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
elWithAttr tag attr content = "<" <> tag <> " " <> attr <> ">" <> content <> "<" <> tag <> "/>"

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

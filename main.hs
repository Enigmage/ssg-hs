import Convert (mdToHTML)
import Html (render)
import Markup (parse)
import System.FilePath.Posix (takeBaseName)

-- constructHTML :: Html
-- constructHTML =
--   html_ "<title>Title</title>" $
--     h1_ "Yo"
--       <> p_ "3 < 2"
--       <> ul_ [p_ "yo", p_ "so"]
--       <> h2_ "Hello"

filePath :: String
filePath = "./example.md"

main :: IO ()
main = do
  txt <- readFile filePath
  (putStrLn . render) (mdToHTML (takeBaseName filePath) (parse txt))

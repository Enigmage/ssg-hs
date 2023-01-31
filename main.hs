import Html

constructHTML :: Html
constructHTML =
  html_ "<title>Title</title>" $
    h1_ "Yo"
      <> p_ "3 < 2"
      <> ul_ [p_ "yo", p_ "so"]
      <> h2_ "Hello"

main :: IO ()
main = putStrLn $ render constructHTML

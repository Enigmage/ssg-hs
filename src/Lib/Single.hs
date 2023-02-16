module Lib.Single (processFileContent, convertSingle) where

import Lib.Convert (markupToHtml)
import Lib.Html (Title, render)
import Lib.Markup (parse)
import System.IO (Handle, hGetContents, hPutStrLn)

processFileContent :: Title -> String -> String
processFileContent title = render . markupToHtml title . parse

convertSingle :: Title -> Handle -> Handle -> IO ()
convertSingle title inp out =
  hGetContents inp >>= hPutStrLn out . processFileContent title

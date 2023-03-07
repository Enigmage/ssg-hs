module Lib.Single (processFileContent, convertSingle) where

import Lib.Convert (markupToHtml)
import Lib.Env (defaultEnv)
import Lib.Html (render)
import Lib.Markup (parse)
import System.IO (Handle, hGetContents, hPutStrLn)

processFileContent :: String -> String -> String
processFileContent title = render . markupToHtml defaultEnv title . parse

convertSingle :: String -> Handle -> Handle -> IO ()
convertSingle title inp out =
  hGetContents inp >>= hPutStrLn out . processFileContent title

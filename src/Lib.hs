module Lib (processSingle, processDir) where

import Lib.Convert (markupToHtml)
import Lib.Html (Title, render)
import Lib.Markup (parse)
import System.IO (Handle, hGetContents, hPutStrLn)

processFileContent :: Title -> String -> String
processFileContent title = render . markupToHtml title . parse

processSingle :: Title -> Handle -> Handle -> IO ()
processSingle title inp out =
  hGetContents inp >>= hPutStrLn out . processFileContent title

processDir :: FilePath -> FilePath -> IO ()
processDir = undefined

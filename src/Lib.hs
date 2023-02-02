{-# LANGUAGE LambdaCase #-}

module Lib (main) where

import Control.Monad (when)
import Lib.Convert (mdToHTML)
import Lib.Html (Title, render)
import Lib.Markup (parse)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath.Posix (takeBaseName)

processFileContent :: Title -> String -> String
processFileContent title = render . mdToHTML title . parse

main :: IO ()
main =
  getArgs
    >>= \case
      [inputPath, outputPath] -> do
        fileContent <- readFile inputPath
        exists <- doesFileExist outputPath
        let writeResult = writeFile outputPath (processFileContent (takeBaseName outputPath) fileContent)
        if not exists then writeResult else whenIO confirm writeResult
      _ -> putStrLn "Usage: ssg-hs <input file path> <output file path>"

-- * > chain IO without passing value

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = cond >>= \res -> when res action

confirm :: IO Bool
confirm =
  putStrLn "File already exists. Overwrite?"
    *> getLine
    >>= \case
      "y" -> pure True
      "n" -> pure False
      _ -> putStrLn "Confirm by writing y or n" *> confirm

{-# LANGUAGE LambdaCase #-}

import Control.Monad (when)
import Convert (mdToHTML)
import Html (Title, render)
import Markup (parse)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath.Posix (takeBaseName)

process :: Title -> String -> String
process title = render . mdToHTML title . parse

main :: IO ()
main =
  getArgs
    >>= \case
      [inputPath, outputPath] -> do
        txt <- readFile inputPath
        exists <- doesFileExist outputPath
        let writeResult = writeFile outputPath (process (takeBaseName outputPath) txt)
        if not exists
          then writeResult
          else whenIO confirm writeResult
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

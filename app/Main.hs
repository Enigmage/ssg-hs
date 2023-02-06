module Main (main) where

import Lib qualified
import OptParse
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.FilePath.Posix (takeBaseName)
import System.IO (IOMode (..), hClose, openFile, stdin, stdout)

main :: IO ()
main =
  parse >>= \case
    ConvertDir input output -> putStrLn "not implemented yet"
    ConvertSingle input output -> do
      (title, inputHandler) <-
        case input of
          Stdin ->
            pure ("", stdin)
          InputFile file ->
            (,) (takeBaseName file) <$> openFile file ReadMode
      outputHandler <-
        case output of
          Stdout -> pure stdout
          OutputFile file -> do
            exists <- doesFileExist file
            shouldOpenFile <- if exists then confirm else pure True
            if shouldOpenFile
              then openFile file WriteMode
              else exitFailure

      Lib.processSingle title inputHandler outputHandler
      hClose inputHandler
      hClose outputHandler

confirm :: IO Bool
confirm =
  putStrLn "File already exists. Overwrite? y/n"
    *> getLine
    >>= \case
      "y" -> pure True
      "n" -> pure False
      _ -> putStrLn "Confirm by writing y or n" *> confirm

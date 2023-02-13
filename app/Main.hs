module Main (main) where

import Lib qualified
import OptParse qualified as Op
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.FilePath.Posix (takeBaseName)
import System.IO (Handle, IOMode (..), stdin, stdout, withFile)

main :: IO ()
main =
  Op.parse >>= \case
    Op.ConvertDir input output -> putStrLn "not implemented yet"
    Op.ConvertSingle input output ->
      let withInputHandle :: (String -> Handle -> IO a) -> IO a
          withInputHandle action =
            case input of
              Op.Stdin ->
                action "" stdin
              Op.InputFile file ->
                withFile file ReadMode (action (takeBaseName file))

          withOutputHandle :: (Handle -> IO a) -> IO a
          withOutputHandle action =
            case output of
              Op.Stdout ->
                action stdout
              Op.OutputFile file -> do
                exists <- doesFileExist file
                shouldOpenFile <-
                  if exists
                    then confirm
                    else pure True
                if shouldOpenFile
                  then withFile file WriteMode action
                  else exitFailure
       in withInputHandle (\title inp -> withOutputHandle $ Lib.processSingle title inp)

confirm :: IO Bool
confirm =
  putStrLn "File already exists. Overwrite? y/n"
    *> getLine
    >>= \case
      "y" -> pure True
      "n" -> pure False
      _ ->
        putStrLn "Confirm by writing y or n"
          *> confirm

module Main (main) where

import Lib qualified
import Lib.Util qualified as Utils
import OptParse qualified as Op
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.FilePath.Posix (takeBaseName)
import System.IO (Handle, IOMode (..), stdin, stdout, withFile)

main :: IO ()
main =
  Op.parse >>= \case
    Op.ConvertDir input output env -> Lib.convertDirectory env input output
    Op.ConvertSingle input output -> withInputHandle (\title inp -> withOutputHandle $ Lib.convertSingle title inp)
      where
        withInputHandle :: (String -> Handle -> IO a) -> IO a
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
                  then Utils.confirm "File already exists. Overwrite? y/n"
                  else pure True
              if shouldOpenFile
                then withFile file WriteMode action
                else exitFailure

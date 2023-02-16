module Lib.Util (whenIO, confirm) where

import Control.Monad (when)

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = cond >>= \res -> when res action

confirm :: String -> IO Bool
confirm message =
  putStrLn message
    *> getLine
    >>= \case
      "y" -> pure True
      "n" -> pure False
      _ ->
        putStrLn "Confirm by writing y or n"
          *> confirm message

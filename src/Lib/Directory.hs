module Lib.Directory (buildIndex) where

import Control.Exception (Exception (displayException), SomeException (..), catch, try)
import Control.Monad (when)
import Data.List (partition)
import Data.Traversable (traverse)
import Lib.Convert (convertMarkup, markupToHtml)
import Lib.Html.Internal qualified as H
import Lib.Markup qualified as M
import System.Directory (createDirectory, doesDirectoryExist, listDirectory, removeDirectoryRecursive)
import System.Exit (exitFailure)
import System.FilePath (takeExtension, (<.>), (</>))
import System.FilePath.Posix (takeBaseName)
import System.IO (hPutStrLn, stderr)

data DirContents = DirContents
  { filesToProcess :: [(FilePath, String)],
    filesToCopy :: [FilePath]
  }

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = cond >>= \res -> when res action

markupToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
markupToRenderedHtml list = map (\(fp, fc) -> (fp, H.render fc)) (index : htmlFiles)
  where
    parsedFiles = parseDirFiles list
    htmlFiles = convertFiles parsedFiles
    index = ("index.html", buildIndex parsedFiles)

parseDirFiles :: [(FilePath, String)] -> [(FilePath, M.Document)]
parseDirFiles = map (\(fp, fc) -> (takeBaseName fp <.> "html", M.parse fc))

convertFiles :: [(FilePath, M.Document)] -> [(FilePath, H.Html)]
convertFiles = map (\(fp, fc) -> (fp, markupToHtml (takeBaseName fp) fc))

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

createDirOrExit :: FilePath -> IO ()
createDirOrExit fp =
  whenIO
    (not <$> createOutputDirectory fp)
    (hPutStrLn stderr "Cancelled" *> exitFailure)

createOutputDirectory :: FilePath -> IO Bool
createOutputDirectory dir = do
  dirExists <- doesDirectoryExist dir
  create <-
    if dirExists
      then do
        override <- confirm "Directory alread exists. Overwrite ? y/n"
        when override (removeDirectoryRecursive dir)
        pure override
      else pure True
  when create (createDirectory dir)
  pure create

applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList operation list = do
  traverse
    ( \input -> do
        maybeResult <-
          catch
            (Right <$> operation input)
            ( \(SomeException e) -> do
                pure $ Left (displayException e)
            )
        pure (input, maybeResult)
    )
    list

filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures =
  foldMap $
    \(fname, fc) ->
      case fc of
        Right content -> pure [(fname, content)]
        Left err -> do
          hPutStrLn stderr err
          pure []

getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir = do
  files <- map (inputDir </>) <$> listDirectory inputDir
  let (markupFiles, otherFiles) =
        partition
          ( \fileName ->
              let fileExt = takeExtension fileName in fileExt == ".txt" || fileExt == ".md"
          )
          files
  markupContent <- applyIoOnList readFile markupFiles >>= filterAndReportFailures
  pure $ DirContents {filesToProcess = markupContent, filesToCopy = otherFiles}

buildIndex :: [(FilePath, M.Document)] -> H.Html
buildIndex files =
  let previews =
        map
          ( \(file, doc) ->
              case doc of
                M.Heading M.H1 heading : article ->
                  H.h3_ (H.link_ file (H.txt_ heading))
                    <> foldMap convertMarkup (take 3 article)
                    <> H.p_ (H.link_ file (H.txt_ "..."))
                _ -> H.h3_ (H.link_ file (H.txt_ file))
          )
          files
   in H.html_
        "Blog"
        ( H.h1_ (H.link_ "index.html" (H.txt_ "Blog"))
            <> H.h2_ (H.txt_ "Posts")
            <> mconcat previews
        )

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory inputDir outputDir = undefined

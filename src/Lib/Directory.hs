module Lib.Directory (buildIndex, convertDirectory) where

import Control.Exception (Exception (displayException), SomeException (..), catch)
import Control.Monad (void, when)
import Data.Bifunctor qualified
import Data.Functor (($>))
import Data.List (partition)
import Lib.Convert (convertMarkup, markupToHtml)
import Lib.Html.Internal qualified as H
import Lib.Markup qualified as M
import Lib.Util qualified as Utils
import System.Directory (copyFile, createDirectory, doesDirectoryExist, listDirectory, removeDirectoryRecursive)
import System.Exit (exitFailure)
import System.FilePath (takeExtension, takeFileName, (<.>), (</>))
import System.FilePath.Posix (takeBaseName)
import System.IO (hPutStrLn, stderr)

data DirContents = DirContents
  { filesToProcess :: [(FilePath, String)],
    filesToCopy :: [FilePath]
  }

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory inputDir outputDir = do
  DirContents markupFiles staticFiles <- getDirFilesAndContent inputDir
  createDirOrExit outputDir
  let htmlFiles = markupToRenderedHtml markupFiles
  copyFiles outputDir staticFiles
  writeFiles outputDir htmlFiles
  putStrLn "Done"

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

getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir = do
  files <- map (inputDir </>) <$> listDirectory inputDir
  let (markupFiles, otherFiles) =
        partition
          ( \fileName ->
              let fileExt = takeExtension fileName
               in fileExt == ".txt" || fileExt == ".md"
          )
          files
  markupContent <- applyIoOnList readFile markupFiles >>= filterAndReportFailures
  pure $ DirContents {filesToProcess = markupContent, filesToCopy = otherFiles}

applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList operation =
  traverse
    ( \input -> do
        maybeResult <-
          catch
            (Right <$> operation input)
            (\(SomeException e) -> (pure . Left) $ displayException e)
        pure (input, maybeResult)
    )

filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures =
  foldMap $
    \(fname, fc) ->
      case fc of
        Right content -> pure [(fname, content)]
        Left err -> hPutStrLn stderr err $> [] -- $> perform error-report effect then lift [] to IO (f a -> b -> f b)

copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles outDir files = void $ applyIoOnList copyFromTo files >>= filterAndReportFailures
  where
    copyFromTo file = copyFile file (outDir </> takeFileName file)

writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles outDir files = void $ applyIoOnList writeFileContent files >>= filterAndReportFailures
  where
    writeFileContent (file, content) = writeFile (outDir </> file) content

markupToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
markupToRenderedHtml files = map (Data.Bifunctor.second H.render) (index : htmlFiles)
  where
    parsedFiles = map parseFileContent files
    htmlFiles = map convertFileContent parsedFiles
    index = ("index.html", buildIndex parsedFiles)

parseFileContent :: (FilePath, String) -> (FilePath, M.Document)
parseFileContent (fPath, fContent) = (takeBaseName fPath <.> "html", M.parse fContent)

convertFileContent :: (FilePath, M.Document) -> (FilePath, H.Html)
convertFileContent (fPath, fContent) = (fPath, markupToHtml (takeBaseName fPath) fContent)

createDirOrExit :: FilePath -> IO ()
createDirOrExit fPath =
  Utils.whenIO
    (not <$> createOutputDirectory fPath)
    (hPutStrLn stderr "Cancelled" *> exitFailure)

createOutputDirectory :: FilePath -> IO Bool
createOutputDirectory outputDir = do
  dirExists <- doesDirectoryExist outputDir
  create <-
    if dirExists
      then do
        overwrite <- Utils.confirm "Directory alread exists. Overwrite ? y/n"
        when overwrite (removeDirectoryRecursive outputDir)
        pure overwrite
      else pure True
  when create (createDirectory outputDir)
  pure create

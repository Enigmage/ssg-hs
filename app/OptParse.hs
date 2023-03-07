module OptParse
  ( Options (..),
    SingleInput (..),
    SingleOutput (..),
    parse,
  )
where

import Data.Maybe (fromMaybe)
import Lib.Env
import Options.Applicative

data Options
  = ConvertSingle SingleInput SingleOutput
  | ConvertDir FilePath FilePath Env
  deriving (Show)

data SingleInput
  = Stdin
  | InputFile FilePath
  deriving (Show)

data SingleOutput
  = Stdout
  | OutputFile FilePath
  deriving (Show)

parse :: IO Options
parse = execParser opts

opts :: ParserInfo Options
opts =
  info
    (helper <*> pOptions)
    ( fullDesc
        <> header "ssghs - a static site generator"
        <> progDesc "convert markup to HTML"
    )

pOptions :: Parser Options
pOptions =
  subparser
    ( command
        "convert"
        ( info
            (helper <*> pSingleOption)
            (progDesc "Convert single markup source to HTML")
        )
        <> command
          "convert-dir"
          ( info
              (helper <*> pDirectoryOption)
              (progDesc "Convert markup directory to HTML")
          )
    )

pSingleOption :: Parser Options
pSingleOption = ConvertSingle <$> singleInput <*> singleOutput
  where
    singleInput = fromMaybe Stdin <$> optional inputFile
    singleOutput = fromMaybe Stdout <$> optional outputFile
    inputFile =
      InputFile
        <$> strOption
          ( long "input"
              <> short 'i'
              <> metavar "FILE"
              <> help "Input file path"
          )
    outputFile =
      OutputFile
        <$> strOption
          ( long "output"
              <> short 'o'
              <> metavar "FILE"
              <> help "Output file path"
          )

pDirectoryOption :: Parser Options
pDirectoryOption = ConvertDir <$> inputDir <*> outputDir <*> pEnv
  where
    inputDir =
      strOption
        ( long "input"
            <> short 'i'
            <> metavar "DIRECTORY"
            <> help "Input directory path"
        )
    outputDir =
      strOption
        ( long "output"
            <> short 'o'
            <> metavar "DIRECTORY"
            <> help "Output directory path"
        )

    pEnv = Env <$> pBlogName <*> pStyleSheet

    pBlogName =
      strOption
        ( long "name"
            <> short 'N'
            <> metavar "STRING"
            <> help "Blog Name"
            <> value (eBlogName defaultEnv)
            <> showDefault
        )

    pStyleSheet =
      strOption
        ( long "style"
            <> short 'S'
            <> metavar "FILE"
            <> help "Stylesheet filepath"
            <> value (eStylesheetPath defaultEnv)
            <> showDefault
        )

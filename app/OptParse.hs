module OptParse
  ( Options (..),
    SingleInput (..),
    SingleOutput (..),
    parse,
  )
where

import Data.Maybe (fromMaybe)
import Options.Applicative

data Options
  = ConvertSingle SingleInput SingleOutput
  | ConvertDir FilePath FilePath
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

-- pSingleOptions :: Parser Options
-- pSingleOptions = liftA2 ConvertSingle pInputFile pOutputFile
pSingleOption :: Parser Options
pSingleOption = ConvertSingle <$> singleInput <*> singleOutput
  where
    singleInput = fromMaybe Stdin <$> optional pInputFile
    singleOutput = fromMaybe Stdout <$> optional pOutputFile

pDirectoryOption :: Parser Options
pDirectoryOption = ConvertDir <$> pInputDir <*> pOutputDir
  where
    pInputDir =
      strOption
        ( long "input"
            <> short 'i'
            <> metavar "DIRECTORY"
            <> help "Input directory path"
        )
    pOutputDir =
      strOption
        ( long "output"
            <> short 'o'
            <> metavar "DIRECTORY"
            <> help "Output directory path"
        )

pInputFile :: Parser SingleInput
pInputFile = InputFile <$> parser
  where
    parser =
      strOption
        ( long "input"
            <> short 'i'
            <> metavar "FILE"
            <> help "Input file path"
        )

pOutputFile :: Parser SingleOutput
pOutputFile = OutputFile <$> parser
  where
    parser =
      strOption
        ( long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "Output file path"
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

opts :: ParserInfo Options
opts =
  info
    (helper <*> pOptions)
    ( fullDesc
        <> header "ssghs - a static site generator"
        <> progDesc "convert markup to HTML"
    )

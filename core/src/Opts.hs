module Opts where

import           Data.Text (Text)
import           Options.Applicative

data Opts =
  Options { optsInput :: Input, optsWidth :: Int, optsRibbon :: Double }

data Input = FileInput FilePath Bool
           | StdInput

input :: Parser Input
input = fileInput <|> stdInput
  where
    fileInput :: Parser Input
    fileInput = FileInput
      <$> strOption
        (long "file" <> short 'f' <> metavar "FILENAME" <> help "Input file")
      <*> switch (long "inplace" <> help "Where to modify inplace")

    stdInput :: Parser Input
    stdInput = flag' StdInput (long "stdin" <> help "Read from stdin")

width :: Parser Int
width = option
  auto
  (long "width"
   <> value 80
   <> metavar "W"
   <> help "Pretty print with W columns")

ribbon :: Parser Double
ribbon = option
  auto
  (long "ribbon"
   <> value 1.0
   <> metavar "r"
   <> help "The fraction of the total page width that can be printed on")

opts :: Parser Opts
opts = Options <$> input <*> width <*> ribbon

optsInfo :: ParserInfo Opts
optsInfo = info
  (opts <**> helper)
  (fullDesc
   <> progDesc "pretty print lisp programs"
   <> header "lisp-pretty-printer - a pretty printer for lisp")

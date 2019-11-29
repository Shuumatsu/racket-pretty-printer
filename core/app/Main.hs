{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.IO (stderr, stdin, stdout)
import           Data.Attoparsec.Text (parseOnly, eitherResult, parse
                                     , IResult(..), endOfInput)
import           Data.Text.Prettyprint.Doc.Render.Text (renderIO, renderStrict)
import           Data.Text.Prettyprint.Doc (pretty, hardline, LayoutOptions(..)
                                          , PageWidth(..), layoutSmart
                                          , layoutPretty)
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import           Data.Text.IO (hGetContents, hPutStrLn)
import           Pretty
import           Parser
import           Control.Monad (forM_)
import           Options.Applicative
import           Opts

readContents :: Opts -> IO Text
readContents (Options (FileInput filepath _) _ _) = Text.IO.readFile filepath
readContents (Options StdInput _ _) = hGetContents stdin

main :: IO ()
main = do
  parsedOps <- execParser optsInfo
  let layoutOptions = LayoutOptions
        $ AvailablePerLine (optsWidth parsedOps) (optsRibbon parsedOps)
  contents <- Text.strip <$> readContents parsedOps
  case parseOnly (parseProg <* endOfInput) contents of
    Right exprs -> let prettied = Text.intercalate "\n\n"
                         $ fmap
                           (renderStrict . layoutPretty layoutOptions . pretty)
                           exprs
                   in case parsedOps of
                        (Options (FileInput filepath True) _ _)
                          -> Text.IO.writeFile filepath prettied
                        _ -> Text.IO.putStrLn prettied
    Left err    -> hPutStrLn stderr $ Text.pack err

{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Ast
import Control.Applicative
import Control.Monad (liftM2)
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as Attoparsec
import Data.Text (Text)
import qualified Data.Text as Text

between :: Monad m => m a -> m b -> m c -> m b
between left p right = do
  left
  x <- p
  right
  return x

oneOf :: String -> Parser Char
oneOf xs = satisfy (`elem` xs) <?> "oneOf"

noneOf :: String -> Parser Char
noneOf xs = satisfy (`notElem` xs) <?> "noneOf"

-- all the characters which can be escaped
escape :: Parser Char
escape =
  ( do
      d <- char '\\'
      oneOf "\\\"0nrvtbf"
  )
    <?> "escaped char"

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f" <?> "non-escaped char"

character :: Parser Char
character = nonEscape <|> escape <?> "character"

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-\\/:<=>?@^_~"

endBy :: Parser a -> Parser b -> Parser [a]
endBy p sep = many $
  do
    x <- p
    sep
    return x

parseString :: Parser LispVal
parseString =
  Str . Text.pack
    <$> between (char '"') (many character) (char '"')

parseCharacter :: Parser LispVal
parseCharacter = Character <$> between (char '\'') character (char '\'')

parseAtom :: Parser LispVal
parseAtom =
  ( do
      first <- letter <|> symbol
      rest <- many (letter <|> digit <|> symbol)
      let atom = Text.pack $ first : rest
      return $
        case atom of
          "#t" -> Boolean True
          "#f" -> Boolean False
          _ -> Atom atom
  )
    <?> "parseAtom"

parseDoubleNumber :: Parser LispVal
parseDoubleNumber = DoubleNumber <$> double

parseRational :: Parser LispVal
parseRational = do
  x <- decimal
  string "/"
  RationalNumber x <$> decimal

parseIntNumber :: Parser LispVal
parseIntNumber = IntNumber <$> decimal

parseExpr :: Parser LispVal
parseExpr =
  ( parseComments
      <|> parseLanguage
      <|> parseAtom
      <|> parseCharacter
      <|> parseString
      <|> parseQuoted
      <|> (try parseDoubleNumber <|> try parseRational <|> parseIntNumber)
      <|> parseBracket
      <|> between
        (char '(' >> many space)
        (try parseDottedList <|> parseList)
        (many space >> char ')')
  )
    <?> "parseExpr"

parseList :: Parser LispVal
parseList = ListVal <$> (parseExpr `sepBy` some space) <?> "parseList"

parseBracket :: Parser LispVal
parseBracket =
  Bracket
    <$> between
      (char '[' >> many space)
      (parseExpr `sepBy` some space)
      (many space >> char ']')

parseDottedList :: Parser LispVal
parseDottedList =
  ( do
      head <- parseExpr `endBy` some space
      tail <- char '.' >> some space >> parseExpr
      return $ DottedList head tail
  )
    <?> "parseDottedList"

parseQuoted :: Parser LispVal
parseQuoted =
  ( do
      char '\''
      x <- parseExpr
      return $ ListVal [Atom "quoted", x]
  )
    <?> "parseQuoted"

parseComments :: Parser LispVal
parseComments = do
  comments <- some comment
  r <- eitherP parseExpr (some endOfLine)
  return $
    case r of
      Left expr -> CommentedBlock comments expr
      Right _ -> Comments comments
  where
    comment =
      Text.strip <$> string ";"
        >> liftM2 const (Attoparsec.takeWhile (not . isEndOfLine)) endOfLine

parseLanguage :: Parser LispVal
parseLanguage = do
  asciiCI "#lang "
  lang <- const <$> Attoparsec.takeWhile (not . isEndOfLine) <*> endOfLine
  return $ Languge lang

parseProg :: Parser [LispVal]
parseProg = many (const <$> parseExpr <*> many space)

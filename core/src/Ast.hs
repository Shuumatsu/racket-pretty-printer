module Ast where

import Data.Text (Text)

data LispVal
  = Languge Text
  | Atom Text
  | ListVal [LispVal]
  | DottedList [LispVal] LispVal
  | Bracket [LispVal]
  | IntNumber Integer
  | DoubleNumber Double
  | RationalNumber Integer Integer
  | Str Text
  | Character Char
  | Boolean Bool
  | CommentedBlock [Text] LispVal
  | Comments [Text]
  deriving (Show)

module Ast where

import           Data.Text (Text)

data LispVal =
    Languge Text
  | Atom Text
  | ListVal [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Str Text
  | Character Char
  | Boolean Bool
  | DoubleNumber Double
  | CommentedBlock [Text] LispVal
  | Comments [Text]
  deriving Show
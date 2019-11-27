{-# LANGUAGE OverloadedStrings #-}

module Pretty where

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Data.Text (Text)
import qualified Data.Text as Text
import           Ast

instance Pretty LispVal where
  pretty = prettyLispVal

comment :: Doc ann
comment = pretty (";;; " :: Text)

prettyLispVal :: LispVal -> Doc ann
prettyLispVal (Languge x) = pretty ("#lang " <> x)
prettyLispVal (Comments xs) = vsep
  $ fmap (\x -> comment <> (pretty . Text.strip) x) xs
prettyLispVal (CommentedBlock text x) =
  prettyLispVal (Comments text) <> hardline <> prettyLispVal x
prettyLispVal (Str x) = enclose (pretty '\"') (pretty '\"') (pretty x)
prettyLispVal (Number x) = pretty x
prettyLispVal (Atom x) = pretty x
prettyLispVal (Character x) = pretty x
prettyLispVal (Boolean True) = pretty ("#t" :: Text)
prettyLispVal (Boolean False) = pretty ("#f" :: Text)
prettyLispVal (DoubleNumber x) = pretty x
prettyLispVal (ListVal [Atom "quoted", x]) = pretty '\'' <> prettyLispVal x
prettyLispVal (ListVal ((Atom "quoted"):xs)) = pretty '\''
  <> prettyLispVal (ListVal xs)
prettyLispVal (ListVal xs) =
  if length xs > 1
  then group . parens . align
    $ prettyLispVal (head xs) <+> align (vsep (fmap prettyLispVal (tail xs)))
  else group . parens . align $ vsep (fmap prettyLispVal xs)
prettyLispVal (DottedList xs x) = group
  $ parens . align
  $ vsep (fmap prettyLispVal xs) <+> pretty '.' <+> prettyLispVal x
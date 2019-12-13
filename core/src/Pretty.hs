{-# LANGUAGE OverloadedStrings #-}

module Pretty where

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Double.Conversion.Text (toShortest)
import           Ast

instance Pretty LispVal where
  pretty = group . prettyLispVal

comment :: Doc ann
comment = pretty ("; " :: Text)

values :: [LispVal] -> Doc ann
values xs = group
  $ if length xs > 2
    then prettyLispVal (head xs)
      <> softline
      <> align (vsep (fmap prettyLispVal (tail xs)))
    else align $ vsep (fmap prettyLispVal xs)

prettyLispVal :: LispVal -> Doc ann
prettyLispVal (Languge x) = pretty ("#lang " <> x)
prettyLispVal (Comments xs) = vsep
  $ fmap (\x -> comment <> (pretty . Text.strip) x) xs
prettyLispVal (CommentedBlock text x) =
  prettyLispVal (Comments text) <> hardline <> prettyLispVal x
prettyLispVal (Str x) = enclose (pretty '\"') (pretty '\"') (pretty x)
prettyLispVal (Atom x) = pretty x
prettyLispVal (Character x) = pretty x
prettyLispVal (Boolean True) = pretty ("#t" :: Text)
prettyLispVal (Boolean False) = pretty ("#f" :: Text)
prettyLispVal (Number x) = pretty x
prettyLispVal (DoubleNumber x) = pretty $ toShortest x
prettyLispVal (ListVal [Atom "quoted", x]) = pretty '\'' <> prettyLispVal x
prettyLispVal (ListVal ((Atom "quoted"):xs)) = pretty '\''
  <> prettyLispVal (ListVal xs)
prettyLispVal (ListVal xs) = parens $ values xs
prettyLispVal (Bracket xs) = brackets $ values xs
prettyLispVal (DottedList xs x) = parens . align
  $ vsep (fmap prettyLispVal xs) <> softline <> pretty '.' <+> prettyLispVal x
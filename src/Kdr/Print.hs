{-# LANGUAGE OverloadedStrings #-}
module Kdr.Print
  ( printKVal
  , printKType
  )
where

import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.Vector                   as Vector

import           Kdr.KVal
import           Kdr.KType

printKVal :: KVal -> Text
printKVal (KInt    i    ) = T.pack (show i)
printKVal (KFloat  f    ) = T.pack (show f)
printKVal (KString s    ) = quote s
printKVal (KBool   True ) = "true()"
printKVal (KBool   False) = "false()"
printKVal (KRec name fields) =
  name <> tupled (map fieldToText (Map.toList fields))
printKVal (KList elements) =
  brackets . T.intercalate "," . map printKVal $ Vector.toList elements
printKVal (KSet elements) =
  braces . T.intercalate "," . map printKVal $ Set.toList elements

printKType :: KType -> Text
printKType TInt    = "int"
printKType TFloat  = "float"
printKType TString = "string"
printKType TBool   = "bool"
printKType (TRec name fields) =
  let f (label, ty) = label <> ":" <> printKType ty
  in  name <> tupled (map f (Map.toList fields))
printKType (TList ty) = brackets (printKType ty)
printKType (TSet  ty) = braces (printKType ty)
printKType (TSum records) =
  T.intercalate "|" (map printKType (Map.elems records))
printKType TUnknown = "?"

parens :: Text -> Text
parens t = "(" <> t <> ")"

brackets :: Text -> Text
brackets t = "[" <> t <> "]"

braces :: Text -> Text
braces t = "{" <> t <> "}"

tupled :: [Text] -> Text
tupled = parens . T.intercalate ","

quote :: Text -> Text
quote t = "\"" <> t <> "\""

fieldToText :: (Text, KVal) -> Text
fieldToText (label, value) = label <> ":" <> printKVal value

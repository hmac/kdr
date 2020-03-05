{-# LANGUAGE OverloadedStrings #-}
module Kdr.Parse
  ( parseKVal
  , parseKType
  )
where

import           Data.Attoparsec.Text           ( Parser )
import qualified Data.Attoparsec.Text          as A
import qualified Text.Parser.Token             as Token
import           Control.Applicative

import           Data.Text                      ( Text )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import qualified Data.Vector                   as Vector
import           Data.Vector                    ( Vector )

import           Kdr.KType
import           Kdr.KVal                       ( KVal(..) )

parseKVal :: Text -> Either String KVal
parseKVal = A.parseOnly (kval <* A.endOfInput)

parseKType :: Text -> Either String KType
parseKType = A.parseOnly (ktype <* A.endOfInput)

kval :: Parser KVal
kval =
  fmap (KInt . fromInteger) Token.integer
    <|> fmap KString        Token.stringLiteral
    <|> fmap KFloat         Token.double
    <|> fmap KBool          bool
    <|> fmap KList          klist
    <|> fmap KSet           kset
    <|> fmap (uncurry KRec) krec

ktype :: Parser KType
ktype =
  fmap (uncurry TRec) trec
    <|> fmap TSet  (Token.braces ktype)
    <|> fmap TList (Token.brackets ktype)
    <|> TBool
    <$  "bool"
    <|> TString
    <$  "string"
    <|> TFloat
    <$  "float"
    <|> TInt
    <$  "int"

trec :: Parser (Text, Map Text KType)
trec = record ktype

bool :: Parser Bool
bool = ("true" >> pure True) <|> ("false" >> pure False)

klist :: Parser (Vector KVal)
klist = Vector.fromList <$> Token.brackets (Token.commaSep kval)

kset :: Parser (Set KVal)
kset = Set.fromList <$> Token.braces (Token.commaSep kval)

krec :: Parser (Text, Map Text KVal)
krec = record kval

-- Parses a record value or a record type
-- First argument is the parser for the inner value/type
record :: Parser a -> Parser (Text, Map Text a)
record p = do
  name   <- A.takeWhile1 (/= '(')
  fields <- Map.fromList <$> Token.parens (Token.commaSep field)
  pure (name, fields)
 where
  field = do
    label <- A.takeWhile1 (\c -> c /= ':' && c /= ')')
    _     <- A.char ':'
    val   <- p
    pure (label, val)

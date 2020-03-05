{-# LANGUAGE OverloadedStrings #-}
module Kdr.KVal
  ( KVal(..)
  , int
  , float
  , string
  , bool
  , record
  , list
  , set
  )
where

import           Data.Text                      ( Text )
import           Data.Map.Strict                ( Map )
import           Data.Set                       ( Set )
import           Data.Vector                    ( Vector )
import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.Vector                   as Vector
import           Data.Bifunctor                 ( first )
import           Data.Maybe                     ( isJust )

data KVal = KInt !Int
          | KFloat !Double
          | KString !Text
          | KBool !Bool
          | KRec Text (Map Text KVal)
          | KList (Vector KVal)
          | KSet (Set KVal)
          deriving (Eq, Show, Ord)

-- Construction

int :: Int -> KVal
int = KInt

float :: Double -> KVal
float = KFloat

string :: Text -> KVal
string = KString

bool :: Bool -> KVal
bool = KBool

record :: Text -> [(Text, KVal)] -> KVal
record name fields = KRec name' fields'
 where
  name'   = encodeRecordName name
  fields' = Map.fromList (map (first encodeRecordLabel) fields)

list :: [KVal] -> KVal
list = KList . Vector.fromList

set :: Set KVal -> KVal
set = KSet

encodeRecordName :: Text -> Text
encodeRecordName name | isJust (T.find (`elem` reservedChars) name) = quote name
                      | otherwise = name

encodeRecordLabel :: Text -> Text
encodeRecordLabel = encodeRecordName

reservedChars :: Set Char
reservedChars = Set.fromList "()[]{}:"

quote :: Text -> Text
quote t = "\"" <> t <> "\""

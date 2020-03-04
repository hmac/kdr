{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib
  ( toText
  , KVal
  , int
  , float
  , string
  , record
  , list
  , set
  , bool
  , ToKdr(..)
  , infer
  , TypeError(..)
  )
where

import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Map.Merge.Strict         as Map
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import qualified Data.Vector                   as Vector
import           Data.Vector                    ( Vector )

import           Data.Bifunctor                 ( first )
import           Data.Foldable                  ( foldlM )
import           Data.Maybe                     ( isJust )

import           GHC.Generics

data KVal = KInt !Int
          | KFloat !Float
          | KString !Text
          | KBool !Bool
          | KRec Text (Map Text KVal)
          | KList (Vector KVal)
          | KSet (Set KVal)
          deriving (Eq, Show, Ord)

-- Construction

int :: Int -> KVal
int = KInt

float :: Float -> KVal
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

-- Conversion to text

toText :: KVal -> Text
toText (KInt    i    ) = T.pack (show i)
toText (KFloat  f    ) = T.pack (show f)
toText (KString s    ) = quote s
toText (KBool   True ) = "true()"
toText (KBool   False) = "false()"
toText (KRec name fields) =
  name <> tupled (map fieldToText (Map.toList fields))
toText (KList elements) =
  brackets . T.intercalate "," . map toText $ Vector.toList elements
toText (KSet elements) =
  braces . T.intercalate "," . map toText $ Set.toList elements

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
fieldToText (label, value) = label <> ":" <> toText value

-- Type inference

data KType = TInt
          | TFloat
          | TString
          | TBool
          | TRec Text (Map Text KType)
          | TList KType
          | TSet KType
          | TSum (Map Text KType)
          | TUnknown
          deriving (Eq, Show, Ord)

data TypeError = TypeMismatch KType KType
               | MissingLabel Text KType
  deriving (Eq, Show, Ord)

infer :: KVal -> Either TypeError KType
infer (KInt    _       ) = pure TInt
infer (KFloat  _       ) = pure TFloat
infer (KString _       ) = pure TString
infer (KBool   _       ) = pure TBool
infer (KRec name fields) = fmap (TRec name) (mapM infer fields)
infer (KList elements  ) = do
  elemTypes <- traverse infer (Vector.toList elements)
  case elemTypes of
    []       -> pure (TList TUnknown)
    (e : es) -> do
      elemType <- foldlM combineType e es
      pure (TList elemType)
infer (KSet elements) = do
  elemTypes <- traverse infer (Set.toList elements)
  case elemTypes of
    []       -> pure (TSet TUnknown)
    (e : es) -> do
      elemType <- foldlM combineType e es
      pure (TSet elemType)

combineType :: KType -> KType -> Either TypeError KType
combineType TInt    TInt    = pure TInt
combineType TFloat  TFloat  = pure TFloat
combineType TString TString = pure TString
combineType TBool   TBool   = pure TBool
combineType (TRec name1 elems1) (TRec name2 elems2)
  | name1 == name2
  =
  -- recursively combine the types of each record field
  -- we report an error if any fields are missing from either record, because
  -- our currently semantics don't permit missing fields.
    let errorOnMissing = Map.mapMaybeMissing
          (\label ty -> Just (Left (MissingLabel label ty)))
        mergeField _label v1 v2 = Just (combineType v1 v2)
        combinedElems = Map.merge errorOnMissing
                                  errorOnMissing
                                  (Map.zipWithMaybeMatched mergeField)
                                  elems1
                                  elems2
    in  TRec name1 <$> sequence combinedElems
  | otherwise
  = pure $ TSum $ Map.fromList
    [(name1, TRec name1 elems1), (name2, TRec name2 elems2)]
combineType (TList ty1) (TList ty2      ) = fmap TList (combineType ty1 ty2)
combineType (TSet  ty1) (TSet  ty2      ) = fmap TSet (combineType ty1 ty2)
combineType (TSum  tys) (TRec name elems) = case Map.lookup name tys of
  -- If the sum already has a record by this name, combine them together
  -- Otherwise insert a new entry into the sum
  Just (TRec name2 elems2) -> do
    rec <- combineType (TRec name elems) (TRec name2 elems2)
    pure $ TSum $ Map.insert name rec tys
  Nothing -> pure $ TSum $ Map.insert name (TRec name elems) tys
  Just t  -> error $ "combineType: unexpected type in sum: " <> show t
combineType TUnknown t        = pure t
combineType t        TUnknown = pure t
combineType t1       t2       = Left (TypeMismatch t1 t2)

-- Conversion from other types

class ToKdr a where
  toKdr :: a -> KVal
  default toKdr :: (Generic a, GToKdr (Rep a)) => a -> KVal
  toKdr x = gtoKdr (from x)

instance ToKdr KVal where
  toKdr = id

instance ToKdr Int where
  toKdr = KInt

instance ToKdr Text where
  toKdr = KString

instance ToKdr Bool where
  toKdr = KBool

instance ToKdr a => ToKdr (Vector a) where
  toKdr = KList . Vector.map toKdr

instance ToKdr a => ToKdr [a] where
  toKdr = toKdr . Vector.fromList

instance ToKdr a => ToKdr (Set a) where
  toKdr = KSet . Set.map toKdr

instance ToKdr () where
  toKdr _ = KRec "unit" mempty

class GToKdr f where
  gtoKdr :: f p -> KVal

-- Empty types
-- 'undefined' is used here on purpose: you can't construct an element of an
-- empty type so we can safely assume this code will never run.
instance GToKdr V1 where
  gtoKdr _ = undefined

instance GToKdr U1 where
  gtoKdr U1 = toKdr ()

-- D1 - data type
-- C1 - constructor
-- S1 - record selector

-- A type with a single constructor
instance GToKdr c => GToKdr (D1 d c) where
  gtoKdr (M1 x) = gtoKdr x

-- A particular constructor
instance (RecordToPairs a, Constructor c) => GToKdr (C1 c a) where
  gtoKdr m1 =
    let name  = getConName m1
        pairs = recordToPairs (unM1 m1)
    in  record name pairs

instance (ToKdr c) => GToKdr (K1 i c) where
  gtoKdr (K1 x) = toKdr x

instance (GToKdr a, GToKdr b) => GToKdr (a :+: b) where
  gtoKdr (L1 x) = gtoKdr x
  gtoKdr (R1 x) = gtoKdr x

-- Converts a generic record representation to a list of (Text, KVal) pairs
-- We'll use this to construct a KRec type
class RecordToPairs f where
  recordToPairs :: f p -> [(Text, KVal)]

-- A single record selector becomes a list of a single pair
instance (GToKdr f, Selector c) => RecordToPairs (S1 c f) where
  recordToPairs s = [recordFieldToPair s]

instance RecordToPairs U1 where
  recordToPairs _ = mempty

-- A record is the product of all its converted selectors
instance (RecordToPairs a, RecordToPairs b) => RecordToPairs (a :*: b) where
  recordToPairs (a :*: b) = recordToPairs a <> recordToPairs b

-- Converts a generic representation of a record selector to a pair
recordFieldToPair :: (GToKdr f, Selector c) => S1 c f p -> (Text, KVal)
recordFieldToPair m1 =
  let label = T.pack (selName m1)
      value = gtoKdr (unM1 m1)
  in  (label, value)

-- Gets the constructor name for an arbitrary type
class GetConName f where
  getConName :: f a -> Text

instance (GetConName a, GetConName b) => GetConName (a :+: b) where
  getConName (L1 x) = getConName x
  getConName (R1 x) = getConName x

instance (Constructor c) => GetConName (C1 c a) where
  getConName = T.pack . conName

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Kdr.ToKdr
  ( ToKdr(..)
  )
where

import           GHC.Generics

import           Data.Text                      ( Text )
import           Data.Set                       ( Set )
import           Data.Vector                    ( Vector )
import qualified Data.Text                     as T
import qualified Data.Set                      as Set
import qualified Data.Vector                   as Vector

import           Kdr.KVal

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

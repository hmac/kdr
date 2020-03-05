module Kdr.Infer
  ( infer
  , TypeError(..)
  )
where

import           Data.Text                      ( Text )
import qualified Data.Map.Strict               as Map
import           Data.Map.Merge.Strict         as Map
import qualified Data.Set                      as Set
import qualified Data.Vector                   as Vector

import           Data.Foldable                  ( foldlM )

import           Kdr.KVal
import           Kdr.KType

-- Type inference

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

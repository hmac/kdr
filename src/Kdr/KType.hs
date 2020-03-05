module Kdr.KType
  ( KType(..)
  )
where

import           Data.Text                      ( Text )
import           Data.Map.Strict                ( Map )

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

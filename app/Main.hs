{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import           Lib
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as T
import           GHC.Generics

data Animal = Dog { barks :: Bool, age :: Int, name :: Text }
            | Cat { age :: Int, name :: Text }
            deriving (Generic, Show)

instance ToKdr Animal

data Payment = Payment { state :: PaymentState
                       , amount :: Int
                       , customerId :: Text
                       }
                       deriving (Generic, Show)

instance ToKdr Payment

data PaymentState = Created
                  | Submitted
                  | Paid
                  | Cancelled
                  | Refunded { reason :: RefundReason }
                  deriving (Generic, Show)

instance ToKdr PaymentState

data RefundReason = CustomerRequest
                  | MerchantError
                  | InternalError
                  | Other { description :: Text }
                  deriving (Generic, Show)

instance ToKdr RefundReason

main :: IO ()
main =
  let example1 = list
        [ record "user" [("name", string "alice"), ("age", int 25)]
        , record "user" [("name", string "bob"), ("age", int 24)]
        ]
      example2 = toKdr Dog { barks = True, age = 5, name = "Rover" }
      example3 = toKdr
        [ Payment { state = Created, amount = 15, customerId = "123" }
        , Payment { state      = Refunded (Other "something broke")
                  , amount     = 20
                  , customerId = "123"
                  }
        ]
  in  do
        print example1
        T.putStrLn (toText example1)
        T.putStrLn (toText example2)
        T.putStrLn (toText example3)

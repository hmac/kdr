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
main = do
  input <- T.getContents
  case parse input of
    Left  err -> putStrLn $ "Parse error: " <> err
    Right val -> case infer val of
      Left  tyErr -> putStrLn $ "Type error: " <> show tyErr
      Right ty    -> T.putStrLn $ toText val <> " : " <> tyToText ty

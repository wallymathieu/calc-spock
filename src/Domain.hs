{-# LANGUAGE DeriveGeneric     #-}
module Domain where
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Aeson
import GHC.Generics
data Operation = Plus | Minus
  deriving (Eq, Generic, Show)
instance ToJSON Operation
instance FromJSON Operation
data Calculation = Const Integer | BinaryExpression Operation Calculation Calculation
  deriving (Eq, Generic, Show)
instance ToJSON Calculation
instance FromJSON Calculation  

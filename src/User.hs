{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module User (User(..)) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)

data User = User
  { username :: !Text
  , email:: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

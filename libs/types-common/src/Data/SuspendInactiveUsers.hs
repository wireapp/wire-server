module Data.SuspendInactiveUsers where

import Data.Aeson
import qualified Data.Schema as Schema
import GHC.Generics
import Wire.Arbitrary
import Wire.Data.Timeout
import Prelude

data SuspendInactiveUsers = SuspendInactiveUsers
  { suspendTimeout :: !Timeout
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Schema.Schema SuspendInactiveUsers
  deriving (Arbitrary) via (GenericUniform SuspendInactiveUsers)

instance Schema.ToSchema SuspendInactiveUsers where
  schema =
    Schema.object "SuspendInactiveUsers" $
      SuspendInactiveUsers
        <$> suspendTimeout Schema..= Schema.field "suspendTimeout" Schema.schema

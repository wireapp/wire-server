module Data.SuspendInactiveUsers where

import Wire.Data.Timeout
import Prelude
import GHC.Generics
import qualified Data.Schema as Schema
import Data.Aeson

data SuspendInactiveUsers = SuspendInactiveUsers
  { suspendTimeout :: !Timeout
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Schema.Schema SuspendInactiveUsers

instance Schema.ToSchema SuspendInactiveUsers where
  schema = Schema.object "SuspendInactiveUsers" $
    SuspendInactiveUsers <$>
      suspendTimeout Schema..= Schema.field "suspendTimeout" Schema.schema

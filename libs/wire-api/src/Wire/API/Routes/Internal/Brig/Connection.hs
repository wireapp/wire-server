{-# LANGUAGE RecordWildCards #-}

module Wire.API.Routes.Internal.Brig.Connection where

import Data.Aeson (FromJSON, ToJSON)
import Data.Id
import Data.Schema
import qualified Data.Swagger as S
import Imports
import Wire.API.Connection

data ConnectionsStatusRequest = ConnectionsStatusRequest
  { csrFrom :: ![UserId],
    csrTo :: !(Maybe [UserId])
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema ConnectionsStatusRequest)

instance ToSchema ConnectionsStatusRequest where
  schema =
    object "ConnectionsStatusRequest" $
      ConnectionsStatusRequest
        <$> csrFrom .= field "from" (array schema)
        <*> csrTo .= optField "to" Nothing (array schema)

data ConnectionStatus = ConnectionStatus
  { csFrom :: !UserId,
    csTo :: !UserId,
    csStatus :: !Relation
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema ConnectionStatus)

instance ToSchema ConnectionStatus where
  schema =
    object "ConnectionStatus" $
      ConnectionStatus
        <$> csFrom .= field "from" schema
        <*> csTo .= field "to" schema
        <*> csStatus .= field "status" schema

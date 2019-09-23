{-# OPTIONS_GHC -Wno-orphans #-}

module Stern.Servant.Orphans where

import Imports

import           Brig.Types.Servant         ()
import           Control.Monad.Catch        (throwM)
import           Data.Proxy
import           Data.ByteString.Conversion (List(List))
import           Servant.API
import           Stern.App
import           Stern.Intra
import           Stern.Servant.Types
import           Stern.Types

import "swagger2" Data.Swagger hiding (Header)


instance FromHttpApiData HandlesQuery where
  parseUrlPiece = fmap translate . parseUrlPiece
    where translate (List handles) = HandlesQuery handles

instance FromHttpApiData UserIdsQuery where
  parseUrlPiece = fmap translate . parseUrlPiece
    where translate (List handles) = UserIdsQuery handles


instance ToParamSchema HandlesQuery  where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance ToParamSchema UserIdsQuery where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance ToParamSchema InvoiceId where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance ToSchema ConsentLog
instance ToSchema ConsentValue
instance ToSchema SetLegalHoldStatus
instance ToSchema SetSSOStatus
instance ToSchema TeamBillingInfo
instance ToSchema TeamBillingInfoUpdate
instance ToSchema TeamInfo
instance ToSchema TeamMemberInfo
instance ToSchema UserConnectionsByStatus
instance ToSchema MarketoResult
instance ToSchema UserMetaInfo
instance ToSchema UserProperties

instance ToSchema BlackListStatus

-- 'MonadIntra' instance is trivially MonadCatch/-Throw.  (So the class can likely be greatly
-- simplified or removed entirely once we get rid of the legacy api.)
instance MonadIntra App where
  type StripException App = App
  throwRpcError = throwM
  catchRpcErrors = id


instance ToSchema ConsentLogPlusMarketo

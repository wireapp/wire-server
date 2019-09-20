{-# OPTIONS_GHC -Wno-orphans #-}

module Stern.Servant.Orphans where

import Imports

import           Brig.Types.Servant.Orphans ()
import           Control.Monad.Catch        (catch, throwM)
import           Data.Proxy
import           Data.String.Conversions    (cs)
import           Data.ByteString.Conversion (List(List))
import           Network.HTTP.Types.Status
import           Network.Wai.Utilities
import           Servant.API
import           Servant.Server
import           Stern.App
import           Stern.Intra
import           Stern.Servant.Types
import           Stern.Types
import           Text.Show.Pretty           (ppShow)

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

instance MonadIntra App where
  type StripException App = App
  throwRpcError = throwM
  catchRpcErrors = (`catch` throwM . translateAny)
                 . (`catch` throwM . translateError)
    where
      translateError :: Error -> ServantErr
      translateError e@(Error s l _) = err (statusCode s) (cs l) e

      translateAny :: SomeException -> ServantErr
      translateAny e = err 500 "error" e

      err :: (Show err) => Int -> String -> err -> ServantErr
      err s l e = ServantErr
        { errHTTPCode     = s
        , errReasonPhrase = l
        , errBody         = cs $ ppShow e
        , errHeaders      = [("Content-Type", "text/ascii")]
        }

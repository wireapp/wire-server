{-# OPTIONS_GHC -Wno-orphans #-}

module Stern.Servant.Orphans where

import Imports

import           Brig.Types.Client
import           Brig.Types.Search as Search
import           Brig.Types.Properties
import           Brig.Types.Servant.Orphans ()
import           Brig.Types.User
import           Brig.Types.User.Auth
import           Control.Lens               ((&), (.~), (?~))  -- (%~)
import           Control.Monad.Catch        (catch, throwM)
import           Data.Aeson                 (Value)
import           Data.ByteString.Conversion as BSC
import           Data.LegalHold
import           Data.Proxy
import           Data.Range
import           Data.Singletons.Bool       (reflectBool)
import           Data.String.Conversions    (cs)
import           Data.Text.Ascii
import           Data.Text                  (Text)
import           Galley.Types
import           Galley.Types.Teams
import           Galley.Types.Teams.Intra
import           GHC.TypeLits
import           Gundeck.Types.Notification
import           Network.HTTP.Types.Status
import           Network.Wai.Utilities
import qualified Data.Text                  as Text
import           Servant.API
import           Servant.API.Description    (FoldDescription, reflectDescription)
import           Servant.API.Modifiers      (FoldRequired)
import           Servant.Server
import           Servant.Swagger
import           Servant.Swagger.Internal   (addDefaultResponse400, addParam)
import           Servant.Swagger.UI
import           Servant.Swagger.UI.Core
import           Stern.App
import           Stern.Intra
import           Stern.Servant.Types
import           Stern.Types
import           Text.Show.Pretty           (ppShow)
import qualified Data.Metrics.Servant       as Metrics

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


instance ToSchema (SwaggerSchemaUI' dir api) where
  declareNamedSchema _ = declareNamedSchema (Proxy @NoContent)

instance ToSchema (SwaggerUiHtml dir any) where
  declareNamedSchema _ = declareNamedSchema (Proxy @NoContent)

instance ToSchema Swagger where
  declareNamedSchema _ = declareNamedSchema (Proxy @NoContent)


instance ToSchema ConsentLog
instance ToSchema ConsentValue
instance ToSchema Perm
instance ToSchema Permissions
instance ToSchema PhoneUpdate
instance ToSchema SetLegalHoldStatus
instance ToSchema SetSSOStatus
instance ToSchema Team
instance ToSchema TeamBillingInfo
instance ToSchema TeamBillingInfoUpdate
instance ToSchema TeamBinding
instance ToSchema TeamData
instance ToSchema TeamInfo
instance ToSchema TeamMember
instance ToSchema TeamMemberInfo
instance ToSchema TeamStatus
instance ToSchema UserConnectionsByStatus
instance ToSchema UserLegalHoldStatus
instance ToSchema PropertyValue
instance ToSchema MarketoResult
instance ToSchema (AsciiText Printable)
instance ToSchema UserMetaInfo
instance ToSchema UserProperties
instance ToSchema PropertyKey

instance ToParamSchema typ => ToParamSchema (Range lower upper typ)

instance ToSchema BlackListStatus

deriving instance Generic Search.Contact
instance ToSchema Search.Contact

deriving instance Generic (SearchResult Search.Contact)
instance ToSchema (SearchResult Search.Contact)


instance ToSchema QueuedNotification where
  declareNamedSchema _ = declareNamedSchema (Proxy @Value)  -- TODO

instance ToSchema Conversation where
  declareNamedSchema _ = declareNamedSchema (Proxy @Value)  -- TODO

instance ToSchema Client where
  declareNamedSchema _ = declareNamedSchema (Proxy @Value)  -- TODO

instance ToSchema CookieList where
  declareNamedSchema _ = declareNamedSchema (Proxy @Value)  -- TODO


instance HasSwagger (NoSwagger :> api) where
  toSwagger _ = mempty

-- Copied and mutated from "Servant.Swagger.Internal".  To make this more composable, we'd
-- have to touch the package, I think.  (We should probably do that now, no?  write a function
-- that does what the instance does, plus takes an extra descr string, then writing the two
-- instances is trivial.  then also find a better name for SwaggerDesc.  WithDescription?)
instance ( KnownSymbol desc
         , KnownSymbol sym
         , ToParamSchema a
         , HasSwagger sub
         , SBoolI (FoldRequired mods)
         , KnownSymbol (FoldDescription mods)
         ) => HasSwagger (SwaggerDesc desc (QueryParam' mods sym a) :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
    & addParam param
    & addDefaultResponse400 tname
    where
      tname = Text.pack (symbolVal (Proxy :: Proxy sym))
      descText :: [Text]
        = [ cs $ symbolVal (Proxy :: Proxy desc)
          , cs $ reflectDescription (Proxy :: Proxy mods)
          ]
      transDesc ""   = Nothing
      transDesc desc = Just desc
      param = mempty
        & name .~ tname
        & description .~ (transDesc . Text.strip . Text.unlines $ descText)
        & required ?~ reflectBool (Proxy :: Proxy (FoldRequired mods))
        & schema .~ ParamOther sch
      sch = mempty
        & in_ .~ ParamQuery
        & paramSchema .~ toParamSchema (Proxy :: Proxy a)





instance HasServer api ctx => HasServer (NoSwagger :> api) ctx where
  type ServerT (NoSwagger :> api) m = ServerT api m
  route _ = route (Proxy @api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

instance HasServer (something :> api) ctx => HasServer (SwaggerDesc (sym :: Symbol) something :> api) ctx where
  type ServerT (SwaggerDesc sym something :> api) m = ServerT (something :> api) m
  route _ = route (Proxy @(something :> api))
  hoistServerWithContext _ = hoistServerWithContext (Proxy @(something :> api))


instance Metrics.RoutesToPaths api => Metrics.RoutesToPaths (NoSwagger :> api) where
  getRoutes = mempty

instance Metrics.RoutesToPaths Raw where
  getRoutes = mempty


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

{-# OPTIONS_GHC -Wno-orphans #-}

-- | swagger2 docs for galley generated with servant-swagger.  for now, this module contains
-- all of the servant code as well.
module Galley.API.Swagger where

import Imports

import "swagger2" Data.Swagger hiding (Header(..))
  -- NB: this package depends on both types-common, swagger2, so there is no away around this name
  -- clash other than -XPackageImports.

import Brig.Types.Team.LegalHold
import Control.Lens hiding (allOf)
import Data.Id
import Data.Proxy
import Data.UUID (UUID)
import Servant.API
import Servant.Swagger


swagger :: Swagger
swagger = toSwagger (Proxy @GalleyRoutes)


-- TODO: document exceptions properly.
-- TODO: document zusr authentication thingy somehow.

type GalleyRoutes
     = "teams" :> Capture "tid" TeamId :> "legalhold" :> "settings"
          :> ReqBody '[JSON] NewLegalHoldService :> Post '[JSON] ViewLegalHoldService
  :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> "settings"
          :> Get '[JSON] ViewLegalHoldService
  :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> "settings"
          :> Delete '[JSON] ()


instance ToParamSchema (Id a) where
    toParamSchema _ = toParamSchema (Proxy @UUID)

instance ToSchema (Id a) where
    declareNamedSchema _ = declareNamedSchema (Proxy @UUID)

instance ToSchema NewLegalHoldService where
    declareNamedSchema _ = pure $ NamedSchema (Just "NewLegalHoldService") $ mempty
--        & properties {- :: InsOrdHashMap Text (Referenced Schema) -} .~ _
--        & example {- :: Maybe Value -} .~ Just _
--        & paramSchema {- :: ParamSchema SwaggerKindSchema -} .~ _
--        & required {- :: [ParamName] -} .~ _

        & title {- :: Maybe Text -} .~ Nothing
        & description {- :: Maybe Text -} .~ Nothing
        & allOf {- :: Maybe [Referenced Schema] -} .~ Nothing
        & additionalProperties {- :: Maybe (Referenced Schema) -} .~ Nothing
        & discriminator {- :: Maybe Text -} .~ Nothing
        & readOnly {- :: Maybe Bool -} .~ Nothing
        & xml {- :: Maybe Xml -} .~ Nothing
        & externalDocs {- :: Maybe ExternalDocs -} .~ Nothing
        & maxProperties {- :: Maybe Integer -} .~ Nothing
        & minProperties {- :: Maybe Integer -} .~ Nothing



{-
instance ToJSON NewLegalHoldService where
    toJSON s = object
        $ "base_url"    .= newLegalHoldServiceUrl s
        # "public_key"  .= newLegalHoldServiceKey s
        # "auth_token"  .= newLegalHoldServiceToken s
        # []

instance ToJSON ViewLegalHoldService where
    toJSON s = object
        $ "team_id"     .= viewLegalHoldServiceTeam s
        # "base_url"    .= viewLegalHoldServiceUrl s
        # "fingerprint" .= viewLegalHoldServiceFingerprint s
        # []
-}



instance ToSchema ViewLegalHoldService where
    declareNamedSchema _ = pure $ NamedSchema (Just "ViewLegalHoldService") $ mempty

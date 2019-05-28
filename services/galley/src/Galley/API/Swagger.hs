{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-patterns #-}
-- it's ok to not warn about unmatched patterns; 'validateEveryToJSON' will crash on them
-- before too long.

-- | swagger2 docs for galley generated with servant-swagger.  for now, this module contains
-- all of the servant code as well.
module Galley.API.Swagger where

import Imports

import "swagger2" Data.Swagger hiding (Header(..))
  -- NB: this package depends on both types-common, swagger2, so there is no away around this name
  -- clash other than -XPackageImports.

import Control.Lens
import Brig.Types.Client.Prekey (PrekeyId, Prekey, LastPrekey)
import Brig.Types.Provider
import Brig.Types.Team.LegalHold
import Data.Aeson (Value(..))
import Data.Id
import Data.Misc
import Data.Proxy
import Data.Text as Text (unlines)
import Data.UUID (UUID)
import Servant.API hiding (Header)
import Servant.Swagger


-- TODO: document exceptions properly.

-- TODO: document zusr authentication thingy somehow.

-- TODO: factor out the servant handlers from the functions in Gally.API.LegalHold, and build
--       them together to an Application.  don't run it yet, but that would give us some extra
--       confidence that the swagger docs is in sync with the implementation.


swagger :: Swagger
swagger = toSwagger (Proxy @GalleyRoutes)


type GalleyRoutes = GalleyRoutesPublic :<|> GalleyRoutesInternal

type GalleyRoutesPublic
     = "teams" :> Capture "tid" TeamId :> "legalhold" :> "settings"
          :> ReqBody '[JSON] NewLegalHoldService
          :> Post '[JSON] ViewLegalHoldService
  :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> "settings"
          :> Get '[JSON] ViewLegalHoldService
  :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> "settings"
          :> Verb 'DELETE 204 '[] NoContent

  :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> Capture "uid" UserId
          :> ReqBody '[JSON] RequestNewLegalHoldClient
          :> Post '[JSON] NewLegalHoldClient
  :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> "approve"
          :> Verb 'PUT 204 '[] NoContent
  :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> Capture "uid" UserId
          :> Get '[JSON] UserLegalHoldStatus
  :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> Capture "uid" UserId
          :> Verb 'DELETE 204 '[] NoContent

type GalleyRoutesInternal
     = "i" :> "teams" :> Capture "tid" TeamId :> "legalhold"
          :> Get '[JSON] LegalHoldTeamConfig
  :<|> "i" :> "teams" :> Capture "tid" TeamId :> "legalhold"
          :> ReqBody '[JSON] LegalHoldTeamConfig
          :> Put '[] NoContent


instance ToParamSchema (Id a) where
    toParamSchema _ = toParamSchema (Proxy @UUID)

instance ToSchema (Id a) where
    declareNamedSchema _ = declareNamedSchema (Proxy @UUID)

instance ToSchema HttpsUrl where
    declareNamedSchema _ = declareNamedSchema (Proxy @Text)

instance ToSchema ServiceKeyPEM where
    declareNamedSchema _ = tweak $ declareNamedSchema (Proxy @Text)
      where
        tweak = fmap $ schema . example ?~ pem
        pem = String . Text.unlines $
            [ "-----BEGIN PUBLIC KEY-----"
            , "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAu+Kg/PHHU3atXrUbKnw0"
            , "G06FliXcNt3lMwl2os5twEDcPPFw/feGiAKymxp+7JqZDrseS5D9THGrW+OQRIPH"
            , "WvUBdiLfGrZqJO223DB6D8K2Su/odmnjZJ2z23rhXoEArTplu+Dg9K+c2LVeXTKV"
            , "VPOaOzgtAB21XKRiQ4ermqgi3/njr03rXyq/qNkuNd6tNcg+HAfGxfGvvCSYBfiS"
            , "bUKr/BeArYRcjzr/h5m1In6fG/if9GEI6m8dxHT9JbY53wiksowy6ajCuqskIFg8"
            , "7X883H+LA/d6X5CTiPv1VMxXdBUiGPuC9IT/6CNQ1/LFt0P37ax58+LGYlaFo7la"
            , "nQIDAQAB"
            , "-----END PUBLIC KEY-----"
            ]

instance ToSchema (Fingerprint Rsa) where
    declareNamedSchema _ = declareNamedSchema (Proxy @Text)  -- TODO (at least inject a plausible example)

instance ToSchema ServiceToken where
    declareNamedSchema _ = declareNamedSchema (Proxy @Text)  -- TODO (at least inject a plausible example)

instance ToSchema NewLegalHoldService where
    declareNamedSchema = genericDeclareNamedSchema opts
      where
        opts = defaultSchemaOptions
          { fieldLabelModifier = \case
              "newLegalHoldServiceKey"   -> "public_key"
              "newLegalHoldServiceUrl"   -> "base_url"
              "newLegalHoldServiceToken" -> "auth_token"
          }

instance ToSchema ViewLegalHoldService where
    declareNamedSchema = genericDeclareNamedSchema opts
      where
        opts = defaultSchemaOptions
          { fieldLabelModifier = \case
              "viewLegalHoldServiceFingerprint" -> "fingerprint"
              "viewLegalHoldServiceUrl"         -> "base_url"
              "viewLegalHoldServiceTeam"        -> "team_id"
          }

instance ToSchema LegalHoldTeamConfig where
    declareNamedSchema = genericDeclareNamedSchema opts
      where
        opts = defaultSchemaOptions
          { fieldLabelModifier = \case
              "legalHoldTeamConfigStatus" -> "status"
          }

instance ToSchema LegalHoldStatus where
    declareNamedSchema = tweak . genericDeclareNamedSchema opts
      where
        opts = defaultSchemaOptions
          { constructorTagModifier = \case
              "LegalHoldDisabled" -> "disabled"
              "LegalHoldEnabled"  -> "enabled"
          }

        tweak = fmap $ schema . description ?~ descr
          where
            descr = "determines whether admins of a team " <>
                    "are allowed to enable LH for their users."

instance ToSchema RequestNewLegalHoldClient where
    declareNamedSchema = genericDeclareNamedSchema opts
      where
        opts = defaultSchemaOptions
          { fieldLabelModifier = \case
              "userId" -> "user_id"
              "teamId" -> "team_id"
          }

instance ToSchema NewLegalHoldClient where
    declareNamedSchema = genericDeclareNamedSchema opts
      where
        opts = defaultSchemaOptions
          { fieldLabelModifier = \case
              "newLegalHoldClientPrekeys" -> "prekeys"
              "newLegalHoldClientLastKey" -> "lastkey"
          }

instance ToSchema UserLegalHoldStatus where
    declareNamedSchema = tweak . genericDeclareNamedSchema opts
      where
        opts = defaultSchemaOptions
          { constructorTagModifier = \case
              "UserLegalHoldEnabled"  -> "enabled"
              "UserLegalHoldPending"  -> "pending"
              "UserLegalHoldDisabled" -> "disabled"
          }

        tweak = fmap $ schema . description ?~ descr
          where
            descr = "states whether a user is under legal hold, " <>
                    "or whether legal hold is pending approval."

instance ToSchema PrekeyId where
    declareNamedSchema _ = tweak $ declareNamedSchema (Proxy @Int)
      where
        tweak = fmap $ schema . description ?~ descr
          where
            descr = "in the range [0..65535]."

instance ToSchema Prekey where
    declareNamedSchema = genericDeclareNamedSchema opts
      where
        opts = defaultSchemaOptions
          { fieldLabelModifier = \case
              "prekeyId" -> "id"
              "prekeyKey" -> "key"
          }

instance ToSchema LastPrekey where
    declareNamedSchema _ = declareNamedSchema (Proxy @Prekey)



{-
import Test.Hspec
import Brig.Types.Test.Arbitrary ()

main :: IO ()
main = hspec $ validateEveryToJSON (Proxy @GalleyRoutes)
-}

{-
-- import Data.String.Conversions
-- import System.Process (system)

-- | dump to file and validate online
--
-- TODO: this shouldn't be in the production code.
main :: IO ()
main = do
  writeFile "/tmp/x" . cs $ encode swagger
  void $ system "cat /tmp/x | json_pp && curl -X POST -d @/tmp/x -H 'Content-Type:application/json' http://online.swagger.io/validator/debug | json_pp"
  -- see also: https://github.com/swagger-api/validator-badge

  -- alternatives:
  -- https://github.com/navidsh/maven.swagger.validator
  -- https://editor.swagger.io/  (this finds dangling refs.  good.)
  -- https://apidevtools.org/swagger-parser/online/  (also finds dangling refs, but it's *very slow*)
-}

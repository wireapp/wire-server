{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- it's ok to not warn about unmatched patterns; 'validateEveryToJSON' will crash on them
-- before too long.

-- | swagger2 docs for galley generated with servant-swagger.  for now, this module contains
-- all of the servant code as well.
module Galley.API.Swagger
  ( GalleyRoutes,
    swagger,
  )
where

import Brig.Types.Client.Prekey (LastPrekey, Prekey, PrekeyId)
import Brig.Types.Provider
import Brig.Types.Team.LegalHold
import Control.Lens
import Data.Aeson (Value (..), toJSON)
import Data.HashMap.Strict.InsOrd
import Data.Id
import Data.LegalHold
import Data.Misc
import Data.Proxy
import Data.Swagger hiding (Header (..))
import Data.Text as Text (unlines)
import Data.UUID (UUID)
import Imports
import Servant.API hiding (Header)
import Servant.Swagger
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Gen as QC
import qualified Test.QuickCheck.Random as QC
import Wire.API.Team.Feature

{-
import Data.String.Conversions
import System.Process (system)
import Data.Aeson (encode)
import Test.Hspec (hspec)
import Brig.Types.Test.Arbitrary ()

main :: IO ()
main = do
  writeFile "/tmp/x" . cs $ encode swagger
  void $ system "cat /tmp/x | json_pp && curl -X POST -d @/tmp/x -H 'Content-Type:application/json' http://online.swagger.io/validator/debug | json_pp"
  hspec $ validateEveryToJSON (Proxy @GalleyRoutes)
  -- see also: https://github.com/swagger-api/validator-badge

  -- alternatives:
  -- https://github.com/navidsh/maven.swagger.validator
  -- https://editor.swagger.io/  (this finds dangling refs.  good.)
  -- https://apidevtools.org/swagger-parser/online/  (also finds dangling refs, but it's *very slow*)
-}

swagger :: Swagger
swagger = toSwagger (Proxy @GalleyRoutes)

type GalleyRoutes = GalleyRoutesPublic :<|> GalleyRoutesInternal

-- FUTUREWORK: restructure this for readability and add missing bodies
type GalleyRoutesPublic =
  "teams" :> Capture "tid" TeamId :> "legalhold" :> "settings"
    :> ReqBody '[JSON] NewLegalHoldService
    :> Post '[JSON] ViewLegalHoldService
    :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> "settings"
      :> Get '[JSON] ViewLegalHoldService
    :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> "settings"
      -- :> ReqBody '[JSON] RemoveLegalHoldSettingsRequest
      :> Verb 'DELETE 204 '[] NoContent
    :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> Capture "uid" UserId
      :> Post '[] NoContent
    :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> Capture "uid" UserId :> "approve"
      -- :> ReqBody '[JSON] ApproveLegalHoldForUserRequest
      :> Verb 'PUT 204 '[] NoContent
    :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> Capture "uid" UserId
      :> Get '[JSON] UserLegalHoldStatusResponse
    :<|> "teams" :> Capture "tid" TeamId :> "legalhold" :> Capture "uid" UserId
      -- :> ReqBody '[JSON] DisableLegalHoldForUserRequest
      :> Verb 'DELETE 204 '[] NoContent

type GalleyRoutesInternal =
  "i" :> "teams" :> Capture "tid" TeamId :> "legalhold"
    :> Get '[JSON] TeamFeatureStatus
    :<|> "i" :> "teams" :> Capture "tid" TeamId :> "legalhold"
      :> ReqBody '[JSON] TeamFeatureStatus
      :> Put '[] NoContent

-- FUTUREWORK: move Swagger instances next to the types they describe

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
      pem =
        String . Text.unlines $
          [ "-----BEGIN PUBLIC KEY-----",
            "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAu+Kg/PHHU3atXrUbKnw0",
            "G06FliXcNt3lMwl2os5twEDcPPFw/feGiAKymxp+7JqZDrseS5D9THGrW+OQRIPH",
            "WvUBdiLfGrZqJO223DB6D8K2Su/odmnjZJ2z23rhXoEArTplu+Dg9K+c2LVeXTKV",
            "VPOaOzgtAB21XKRiQ4ermqgi3/njr03rXyq/qNkuNd6tNcg+HAfGxfGvvCSYBfiS",
            "bUKr/BeArYRcjzr/h5m1In6fG/if9GEI6m8dxHT9JbY53wiksowy6ajCuqskIFg8",
            "7X883H+LA/d6X5CTiPv1VMxXdBUiGPuC9IT/6CNQ1/LFt0P37ax58+LGYlaFo7la",
            "nQIDAQAB",
            "-----END PUBLIC KEY-----"
          ]

instance ToSchema (Fingerprint Rsa) where
  declareNamedSchema _ = tweak $ declareNamedSchema (Proxy @Text)
    where
      tweak = fmap $ schema . example ?~ fpr
      fpr = "ioy3GeIjgQRsobf2EKGO3O8mq/FofFxHRqy0T4ERIZ8="

instance ToSchema ServiceToken where
  declareNamedSchema _ = tweak $ declareNamedSchema (Proxy @Text)
    where
      tweak = fmap $ schema . example ?~ tok
      tok = "sometoken"

instance ToSchema NewLegalHoldService where
  declareNamedSchema = genericDeclareNamedSchema opts
    where
      opts =
        defaultSchemaOptions
          { fieldLabelModifier = \case
              "newLegalHoldServiceKey" -> "public_key"
              "newLegalHoldServiceUrl" -> "base_url"
              "newLegalHoldServiceToken" -> "auth_token"
          }

instance ToSchema ViewLegalHoldService where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "ViewLegalHoldService") $
        mempty
          & properties .~ properties_
          & example .~ Just (toJSON example_)
          & required .~ ["status"]
          & minProperties .~ Just 1
          & maxProperties .~ Just 2
          & type_ .~ Just SwaggerObject
    where
      properties_ :: InsOrdHashMap Text (Referenced Schema)
      properties_ =
        fromList
          [ ("status", Inline (toSchema (Proxy @MockViewLegalHoldServiceStatus))),
            ("settings", Inline (toSchema (Proxy @ViewLegalHoldServiceInfo)))
          ]
      example_ =
        ViewLegalHoldService
          (ViewLegalHoldServiceInfo arbitraryExample arbitraryExample arbitraryExample (ServiceToken "sometoken") arbitraryExample)

-- | this type is only introduce locally here to generate the schema for 'ViewLegalHoldService'.
data MockViewLegalHoldServiceStatus = Configured | NotConfigured | Disabled
  deriving (Eq, Show, Generic)

instance ToSchema MockViewLegalHoldServiceStatus where
  declareNamedSchema = genericDeclareNamedSchema opts
    where
      opts = defaultSchemaOptions {constructorTagModifier = camelToUnderscore}

instance ToSchema ViewLegalHoldServiceInfo where
  {- please don't put empty lines here: https://github.com/tweag/ormolu/issues/603
  -- FUTUREWORK: The generic instance uses a reference to the UUID type in TeamId.  This
  -- leads to perfectly valid swagger output, but 'validateEveryToJSON' chokes on it
  -- (unknown schema "UUID").  In order to be able to run those tests, we construct the
  -- 'ToSchema' instance manually.
  -- See also: https://github.com/haskell-servant/servant-swagger/pull/104
  declareNamedSchema = genericDeclareNamedSchema opts
    where
      opts = defaultSchemaOptions
        { fieldLabelModifier = \case
            "viewLegalHoldServiceFingerprint" -> "fingerprint"
            "viewLegalHoldServiceUrl"         -> "base_url"
            "viewLegalHoldServiceTeam"        -> "team_id"
            "viewLegalHoldServiceAuthToken"   -> "auth_token"
            "viewLegalHoldServiceKey"         -> "public_key"
        }
  -}
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "ViewLegalHoldServiceInfo") $
        mempty
          & properties .~ properties_
          & example .~ Just (toJSON example_)
          & required .~ ["team_id", "base_url", "fingerprint", "auth_token", "public_key"]
          & type_ .~ Just SwaggerObject
    where
      properties_ :: InsOrdHashMap Text (Referenced Schema)
      properties_ =
        fromList
          [ ("team_id", Inline (toSchema (Proxy @UUID))),
            ("base_url", Inline (toSchema (Proxy @HttpsUrl))),
            ("fingerprint", Inline (toSchema (Proxy @(Fingerprint Rsa)))),
            ("auth_token", Inline (toSchema (Proxy @(ServiceToken)))),
            ("public_key", Inline (toSchema (Proxy @(ServiceKeyPEM))))
          ]
      example_ =
        ViewLegalHoldService
          (ViewLegalHoldServiceInfo arbitraryExample arbitraryExample arbitraryExample (ServiceToken "sometoken") arbitraryExample)

instance ToSchema TeamFeatureStatus where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "TeamFeatureStatus") $
        mempty
          & properties .~ (fromList [("status", Inline statusValue)])
          & required .~ ["status"]
          & type_ ?~ SwaggerObject
          & description ?~ "whether a given team feature is enabled"
    where
      statusValue =
        mempty
          & enum_ ?~ [String "enabled", String "disabled"]

instance ToSchema RequestNewLegalHoldClient where
  declareNamedSchema = genericDeclareNamedSchema opts
    where
      opts =
        defaultSchemaOptions
          { fieldLabelModifier = \case
              "userId" -> "user_id"
              "teamId" -> "team_id"
          }

instance ToSchema NewLegalHoldClient where
  declareNamedSchema = genericDeclareNamedSchema opts
    where
      opts =
        defaultSchemaOptions
          { fieldLabelModifier = \case
              "newLegalHoldClientPrekeys" -> "prekeys"
              "newLegalHoldClientLastKey" -> "last_prekey"
          }

instance ToSchema UserLegalHoldStatusResponse where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "UserLegalHoldStatusResponse") $
        mempty
          & properties .~ properties_
          & required .~ ["status"]
          & minProperties .~ Just 1
          & maxProperties .~ Just 3
          & type_ .~ Just SwaggerObject
    where
      properties_ :: InsOrdHashMap Text (Referenced Schema)
      properties_ =
        fromList
          [ ("status", Inline (toSchema (Proxy @UserLegalHoldStatus))),
            ("last_prekey", Inline (toSchema (Proxy @LastPrekey))),
            ("client", Inline (toSchema (Proxy @(IdObject ClientId))))
          ]

instance ToSchema a => ToSchema (IdObject a) where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "IdObject a") $
        mempty
          & properties .~ properties_
          & required .~ ["id"]
          & type_ .~ Just SwaggerObject
    where
      properties_ :: InsOrdHashMap Text (Referenced Schema)
      properties_ =
        fromList
          [ ("id", Inline (toSchema (Proxy @a)))
          ]

instance ToSchema UserLegalHoldStatus where
  declareNamedSchema = tweak . genericDeclareNamedSchema opts
    where
      opts =
        defaultSchemaOptions
          { constructorTagModifier = \case
              "UserLegalHoldEnabled" -> "enabled"
              "UserLegalHoldPending" -> "pending"
              "UserLegalHoldDisabled" -> "disabled"
          }
      tweak = fmap $ schema . description ?~ descr
        where
          descr =
            "states whether a user is under legal hold, "
              <> "or whether legal hold is pending approval."

instance ToSchema ClientId where
  declareNamedSchema _ = tweak $ declareNamedSchema (Proxy @Text)
    where
      tweak = fmap $ schema . description ?~ descr
        where
          descr = "A Client Id"

instance ToSchema PrekeyId where
  declareNamedSchema _ = tweak $ declareNamedSchema (Proxy @Int)
    where
      tweak = fmap $ schema . description ?~ descr
        where
          descr = "in the range [0..65535]."

-- FUTUREWORK: can this be also expressed in swagger, not just in the description?

instance ToSchema Prekey where
  declareNamedSchema = genericDeclareNamedSchema opts
    where
      opts =
        defaultSchemaOptions
          { fieldLabelModifier = \case
              "prekeyId" -> "id"
              "prekeyKey" -> "key"
          }

instance ToSchema LastPrekey where
  declareNamedSchema _ = declareNamedSchema (Proxy @Prekey)

----------------------------------------------------------------------
-- helpers

arbitraryExample :: QC.Arbitrary a => a
arbitraryExample = QC.unGen QC.arbitrary (QC.mkQCGen 0) 30

camelToUnderscore :: String -> String
camelToUnderscore = concatMap go . (ix 0 %~ toLower)
  where
    go x = if isUpper x then "_" <> [toLower x] else [x]

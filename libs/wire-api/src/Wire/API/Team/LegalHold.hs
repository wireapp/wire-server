{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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

-- | Swagger schemas for these types are in 'Galley.API.Swagger'.
module Wire.API.Team.LegalHold
  ( NewLegalHoldService (..),
    ViewLegalHoldService (..),
    ViewLegalHoldServiceInfo (..),
    UserLegalHoldStatusResponse (..),
    RemoveLegalHoldSettingsRequest (..),
    DisableLegalHoldForUserRequest (..),
    ApproveLegalHoldForUserRequest (..),
    LegalholdProtectee (..),
  )
where

import Control.Lens (ix, (%~), (.~))
import Data.Aeson hiding (constructorTagModifier, fieldLabelModifier)
import Data.HashMap.Strict.InsOrd
import Data.Id
import Data.Json.Util
import Data.LegalHold
import Data.Misc
import Data.Proxy
import Data.Swagger hiding (info)
import Data.UUID
import Imports
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Gen as QC
import qualified Test.QuickCheck.Random as QC
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))
import Wire.API.Provider
import Wire.API.Provider.Service (ServiceKeyPEM)
import Wire.API.User.Client.Prekey

--------------------------------------------------------------------------------
-- NewLegalHoldService

-- | This type is analogous to 'NewService' for bots.
data NewLegalHoldService = NewLegalHoldService
  { newLegalHoldServiceUrl :: HttpsUrl,
    newLegalHoldServiceKey :: ServiceKeyPEM,
    newLegalHoldServiceToken :: ServiceToken
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewLegalHoldService)

instance ToSchema NewLegalHoldService where
  declareNamedSchema = genericDeclareNamedSchema opts
    where
      opts =
        defaultSchemaOptions
          { fieldLabelModifier = \case
              "newLegalHoldServiceKey" -> "public_key"
              "newLegalHoldServiceUrl" -> "base_url"
              "newLegalHoldServiceToken" -> "auth_token"
              _ -> ""
          }

instance ToJSON NewLegalHoldService where
  toJSON s =
    object $
      "base_url" .= newLegalHoldServiceUrl s
        # "public_key" .= newLegalHoldServiceKey s
        # "auth_token" .= newLegalHoldServiceToken s
        # []

instance FromJSON NewLegalHoldService where
  parseJSON = withObject "NewLegalHoldService" $ \o ->
    NewLegalHoldService
      <$> o .: "base_url"
      <*> o .: "public_key"
      <*> o .: "auth_token"

--------------------------------------------------------------------------------
-- ViewLegalHoldService

data ViewLegalHoldService
  = ViewLegalHoldService ViewLegalHoldServiceInfo
  | ViewLegalHoldServiceNotConfigured
  | ViewLegalHoldServiceDisabled
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ViewLegalHoldService)

-- | this type is only introduce locally here to generate the schema for 'ViewLegalHoldService'.
data MockViewLegalHoldServiceStatus = Configured | NotConfigured | Disabled
  deriving (Eq, Show, Generic)

instance ToSchema MockViewLegalHoldServiceStatus where
  declareNamedSchema = genericDeclareNamedSchema opts
    where
      opts = defaultSchemaOptions {constructorTagModifier = camelToUnderscore}

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

instance ToJSON ViewLegalHoldService where
  toJSON s = case s of
    ViewLegalHoldService settings ->
      object $
        "status" .= String "configured"
          # "settings" .= settings
          # []
    ViewLegalHoldServiceNotConfigured ->
      object $
        "status" .= String "not_configured"
          # []
    ViewLegalHoldServiceDisabled ->
      object $
        "status" .= String "disabled"
          # []

instance FromJSON ViewLegalHoldService where
  parseJSON = withObject "LegalHoldService" $ \o -> do
    status :: Text <- o .: "status"
    case status of
      "configured" -> ViewLegalHoldService <$> (o .: "settings")
      "not_configured" -> pure ViewLegalHoldServiceNotConfigured
      "disabled" -> pure ViewLegalHoldServiceDisabled
      _ -> fail "status (one of configured, not_configured, disabled)"

data ViewLegalHoldServiceInfo = ViewLegalHoldServiceInfo
  { viewLegalHoldServiceTeam :: TeamId,
    viewLegalHoldServiceUrl :: HttpsUrl,
    viewLegalHoldServiceFingerprint :: Fingerprint Rsa,
    viewLegalHoldServiceAuthToken :: ServiceToken,
    viewLegalHoldServiceKey :: ServiceKeyPEM
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ViewLegalHoldServiceInfo)

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

instance ToJSON ViewLegalHoldServiceInfo where
  toJSON info =
    object $
      "team_id" .= viewLegalHoldServiceTeam info
        # "base_url" .= viewLegalHoldServiceUrl info
        # "fingerprint" .= viewLegalHoldServiceFingerprint info
        # "auth_token" .= viewLegalHoldServiceAuthToken info
        # "public_key" .= viewLegalHoldServiceKey info
        # []

instance FromJSON ViewLegalHoldServiceInfo where
  parseJSON = withObject "LegalHoldServiceInfo" $ \o ->
    ViewLegalHoldServiceInfo
      <$> o .: "team_id"
      <*> o .: "base_url"
      <*> o .: "fingerprint"
      <*> o .: "auth_token"
      <*> o .: "public_key"

--------------------------------------------------------------------------------
-- UserLegalHoldStatusResponse

data UserLegalHoldStatusResponse = UserLegalHoldStatusResponse
  { ulhsrStatus :: UserLegalHoldStatus,
    -- | Exists only when status is Pending or Enabled
    ulhsrLastPrekey :: Maybe LastPrekey,
    -- | Exists only when status is Pending or Enabled
    ulhsrClientId :: Maybe ClientId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserLegalHoldStatusResponse)

instance ToSchema UserLegalHoldStatusResponse where
  declareNamedSchema _ = do
    clientSchema <- declareSchemaRef (Proxy @(IdObject ClientId))
    let properties_ :: InsOrdHashMap Text (Referenced Schema)
        properties_ =
          fromList
            [ ("status", Inline (toSchema (Proxy @UserLegalHoldStatus))),
              ("last_prekey", Inline (toSchema (Proxy @LastPrekey))),
              ("client", clientSchema)
            ]
    pure $
      NamedSchema (Just "UserLegalHoldStatusResponse") $
        mempty
          & properties .~ properties_
          & required .~ ["status"]
          & minProperties .~ Just 1
          & maxProperties .~ Just 3
          & type_ .~ Just SwaggerObject

instance ToJSON UserLegalHoldStatusResponse where
  toJSON (UserLegalHoldStatusResponse status lastPrekey' clientId') =
    object $
      "status" .= status
        # "last_prekey" .= lastPrekey'
        # "client" .= (IdObject <$> clientId')
        # []

instance FromJSON UserLegalHoldStatusResponse where
  parseJSON = withObject "UserLegalHoldStatusResponse" $ \o ->
    UserLegalHoldStatusResponse
      <$> o .: "status"
      <*> o .:? "last_prekey"
      <*> (fromIdObject @ClientId <$$> (o .:? "client"))

--------------------------------------------------------------------------------
-- RemoveLegalHoldSettingsRequest

data RemoveLegalHoldSettingsRequest = RemoveLegalHoldSettingsRequest
  { rmlhsrPassword :: Maybe PlainTextPassword
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RemoveLegalHoldSettingsRequest)

instance ToJSON RemoveLegalHoldSettingsRequest where
  toJSON (RemoveLegalHoldSettingsRequest password) =
    object $
      "password" .= password
        # []

instance FromJSON RemoveLegalHoldSettingsRequest where
  parseJSON = withObject "RemoveLegalHoldSettingsRequest" $ \o ->
    RemoveLegalHoldSettingsRequest
      <$> o .:? "password"

--------------------------------------------------------------------------------
-- DisableLegalHoldForUserRequest

data DisableLegalHoldForUserRequest = DisableLegalHoldForUserRequest
  { dlhfuPassword :: Maybe PlainTextPassword
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform DisableLegalHoldForUserRequest)

instance ToJSON DisableLegalHoldForUserRequest where
  toJSON (DisableLegalHoldForUserRequest password) =
    object $
      "password" .= password
        # []

instance FromJSON DisableLegalHoldForUserRequest where
  parseJSON = withObject "DisableLegalHoldForUserRequest" $ \o ->
    DisableLegalHoldForUserRequest
      <$> o .:? "password"

--------------------------------------------------------------------------------
-- ApproveLegalHoldForUserRequest

data ApproveLegalHoldForUserRequest = ApproveLegalHoldForUserRequest
  { alhfuPassword :: Maybe PlainTextPassword
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ApproveLegalHoldForUserRequest)

instance ToJSON ApproveLegalHoldForUserRequest where
  toJSON (ApproveLegalHoldForUserRequest password) =
    object $
      "password" .= password
        # []

instance FromJSON ApproveLegalHoldForUserRequest where
  parseJSON = withObject "ApproveLegalHoldForUserRequest" $ \o ->
    ApproveLegalHoldForUserRequest
      <$> o .:? "password"

----------------------------------------------------------------------
-- helpers

arbitraryExample :: QC.Arbitrary a => a
arbitraryExample = QC.unGen QC.arbitrary (QC.mkQCGen 0) 30

camelToUnderscore :: String -> String
camelToUnderscore = concatMap go . (ix 0 %~ toLower)
  where
    go x = if isUpper x then "_" <> [toLower x] else [x]

-----------------------------------------------------------------------

-- | Bots are not protected to be potentially recorded by legalhold devices.
data LegalholdProtectee
  = ProtectedUser UserId
  | -- | add UserId here if you want to protect bots as well (or just remove and use
    -- 'ProtectedUser', but then you'll loose the user type information).
    UnprotectedBot
  | -- | FUTUREWORK: protection against legalhold when looking up prekeys across federated
    -- instances.
    LegalholdPlusFederationNotImplemented
  deriving (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via (GenericUniform LegalholdProtectee)

instance ToJSON LegalholdProtectee

-- {"tag":"ProtectedUser","contents":"110a187a-be5b-11eb-8f47-370bc8e40f35"}
-- {"tag":"UnprotectedBot"}
-- {"tag":"LegalholdPlusFederationNotImplemented"}
instance FromJSON LegalholdProtectee

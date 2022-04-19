{-# LANGUAGE StrictData #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

-- import Control.Lens (ix, (%~), (.~))
-- import Data.HashMap.Strict.InsOrd
import qualified Data.Aeson.Types as A
import Data.Id
-- import Data.Json.Util
import Data.LegalHold
import Data.Misc
-- import Data.Proxy
import Data.Schema
import qualified Data.Swagger as S hiding (info)
-- import Data.UUID

-- import qualified Test.QuickCheck as QC
-- import qualified Test.QuickCheck.Gen as QC
-- import qualified Test.QuickCheck.Random as QC

import Deriving.Aeson
import Imports
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
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema NewLegalHoldService)

instance ToSchema NewLegalHoldService where
  schema =
    object "NewLegalHoldService" $
      NewLegalHoldService
        <$> newLegalHoldServiceUrl .= field "base_url" schema
        <*> newLegalHoldServiceKey .= field "public_key" schema
        <*> newLegalHoldServiceToken .= field "auth_token" schema

-- declareNamedSchema = genericDeclareNamedSchema opts
--   where
--     opts =
--       defaultSchemaOptions
--         { fieldLabelModifier = \case
--             "newLegalHoldServiceKey" -> "public_key"
--             "newLegalHoldServiceUrl" -> "base_url"
--             "newLegalHoldServiceToken" -> "auth_token"
--             _ -> ""
--         }

--------------------------------------------------------------------------------
-- ViewLegalHoldService

data ViewLegalHoldService
  = ViewLegalHoldService ViewLegalHoldServiceInfo
  | ViewLegalHoldServiceNotConfigured
  | ViewLegalHoldServiceDisabled
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ViewLegalHoldService)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema ViewLegalHoldService)

-- | this type is only introduce locally here to generate the schema for 'ViewLegalHoldService'.
data MockViewLegalHoldServiceStatus = Configured | NotConfigured | Disabled
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema MockViewLegalHoldServiceStatus)

instance ToSchema MockViewLegalHoldServiceStatus where
  schema =
    enum @Text "MockViewLegalHoldServiceStatus" $
      mconcat
        [ element "configured" Configured,
          element "not_configured" NotConfigured,
          element "disabled" Disabled
        ]

data LegalHoldRecord = LegalHoldRecord
  { status :: Text,
    settings :: Maybe ViewLegalHoldServiceInfo
  }

instance ToSchema ViewLegalHoldService where
  schema =
    object "ViewLegalHoldService" $
      toOutput .= recordSchema `withParser` validateViewLegalHoldService
    where
      toOutput :: ViewLegalHoldService -> LegalHoldRecord
      toOutput = \case
        ViewLegalHoldService info -> LegalHoldRecord "configured" (Just info)
        ViewLegalHoldServiceNotConfigured -> LegalHoldRecord "not_configured" Nothing
        ViewLegalHoldServiceDisabled -> LegalHoldRecord "disabled" Nothing

      recordSchema :: ObjectSchema SwaggerDoc LegalHoldRecord
      recordSchema =
        LegalHoldRecord
          <$> status .= field "status" schema
          <*> settings .= maybe_ (optField "settings" schema)

      validateViewLegalHoldService :: LegalHoldRecord -> A.Parser ViewLegalHoldService
      validateViewLegalHoldService (LegalHoldRecord "configured" (Just info)) =
        pure $ ViewLegalHoldService info
      validateViewLegalHoldService (LegalHoldRecord "disabled" _) =
        pure ViewLegalHoldServiceDisabled
      validateViewLegalHoldService (LegalHoldRecord "not_configured" _) =
        pure ViewLegalHoldServiceNotConfigured
      validateViewLegalHoldService _ = fail "status (one of configured, not_configured, disabled)"

-- declareNamedSchema _ =
--   pure $
--     NamedSchema (Just "ViewLegalHoldService") $
--       mempty
--         & properties .~ properties_
--         & example .~ Just (toJSON example_)
--         & required .~ ["status"]
--         & minProperties .~ Just 1
--         & maxProperties .~ Just 2
--         & type_ .~ Just SwaggerObject
--   where
--     properties_ :: InsOrdHashMap Text (Referenced Schema)
--     properties_ =
--       fromList
--         [ ("status", Inline (toSchema (Proxy @MockViewLegalHoldServiceStatus))),
--           ("settings", Inline (toSchema (Proxy @ViewLegalHoldServiceInfo)))
--         ]
--     example_ =
--       ViewLegalHoldService
--         (ViewLegalHoldServiceInfo arbitraryExample arbitraryExample arbitraryExample (ServiceToken "sometoken") arbitraryExample)

-- instance ToJSON ViewLegalHoldService where
--   toJSON s = case s of
--     ViewLegalHoldService settings ->
--       object $
--         "status" .= String "configured"
--           # "settings" .= settings
--           # []
--     ViewLegalHoldServiceNotConfigured ->
--       object $
--         "status" .= String "not_configured"
--           # []
--     ViewLegalHoldServiceDisabled ->
--       object $
--         "status" .= String "disabled"
--           # []

-- instance FromJSON ViewLegalHoldService where
--   parseJSON = withObject "LegalHoldService" $ \o -> do
--     status :: Text <- o .: "status"
--     case status of
--       "configured" -> ViewLegalHoldService <$> (o .: "settings")
--       "not_configured" -> pure ViewLegalHoldServiceNotConfigured
--       "disabled" -> pure ViewLegalHoldServiceDisabled
--       _ -> fail "status (one of configured, not_configured, disabled)"

data ViewLegalHoldServiceInfo = ViewLegalHoldServiceInfo
  { viewLegalHoldServiceTeam :: TeamId,
    viewLegalHoldServiceUrl :: HttpsUrl,
    viewLegalHoldServiceFingerprint :: Fingerprint Rsa,
    viewLegalHoldServiceAuthToken :: ServiceToken,
    viewLegalHoldServiceKey :: ServiceKeyPEM
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ViewLegalHoldServiceInfo)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema ViewLegalHoldServiceInfo)

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
  schema =
    object "ViewLegalHoldServiceInfo" $
      ViewLegalHoldServiceInfo
        <$> viewLegalHoldServiceTeam .= field "team_id" schema
        <*> viewLegalHoldServiceUrl .= field "base_url" schema
        <*> viewLegalHoldServiceFingerprint .= field "fingerprint" schema
        <*> viewLegalHoldServiceAuthToken .= field "auth_token" schema
        <*> viewLegalHoldServiceKey .= field "public_key" schema

-- declareNamedSchema _ =
--   pure $
--     NamedSchema (Just "ViewLegalHoldServiceInfo") $
--       mempty
--         & properties .~ properties_
--         & example .~ Just (toJSON example_)
--         & required .~ ["team_id", "base_url", "fingerprint", "auth_token", "public_key"]
--         & type_ .~ Just SwaggerObject
--   where
--     properties_ :: InsOrdHashMap Text (Referenced Schema)
--     properties_ =
--       fromList
--         [ ("team_id", Inline (toSchema (Proxy @UUID))),
--           ("base_url", Inline (toSchema (Proxy @HttpsUrl))),
--           ("fingerprint", Inline (toSchema (Proxy @(Fingerprint Rsa)))),
--           ("auth_token", Inline (toSchema (Proxy @ServiceToken))),
--           ("public_key", Inline (toSchema (Proxy @ServiceKeyPEM)))
--         ]
--     example_ =
--       ViewLegalHoldService
--         (ViewLegalHoldServiceInfo arbitraryExample arbitraryExample arbitraryExample (ServiceToken "sometoken") arbitraryExample)

-- instance ToJSON ViewLegalHoldServiceInfo where
--   toJSON info =
--     object $
--       "team_id" .= viewLegalHoldServiceTeam info
--         # "base_url" .= viewLegalHoldServiceUrl info
--         # "fingerprint" .= viewLegalHoldServiceFingerprint info
--         # "auth_token" .= viewLegalHoldServiceAuthToken info
--         # "public_key" .= viewLegalHoldServiceKey info
--         # []

-- instance FromJSON ViewLegalHoldServiceInfo where
--   parseJSON = withObject "LegalHoldServiceInfo" $ \o ->
--     ViewLegalHoldServiceInfo
--       <$> o .: "team_id"
--       <*> o .: "base_url"
--       <*> o .: "fingerprint"
--       <*> o .: "auth_token"
--       <*> o .: "public_key"

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
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema UserLegalHoldStatusResponse)

instance ToSchema UserLegalHoldStatusResponse where
  schema =
    object "UserLegalHoldStatusResponse" $
      UserLegalHoldStatusResponse
        <$> ulhsrStatus .= field "status" schema
        <*> ulhsrLastPrekey .= maybe_ (optField "last_prekey" schema)
        <*> (fmap IdObject . ulhsrClientId) .= maybe_ (optField "client" (fromIdObject <$> schema))

-- declareNamedSchema _ = do
--   clientSchema <- declareSchemaRef (Proxy @(IdObject ClientId))
--   let properties_ :: InsOrdHashMap Text (Referenced Schema)
--       properties_ =
--         fromList
--           [ ("status", Inline (toSchema (Proxy @UserLegalHoldStatus))),
--             ("last_prekey", Inline (toSchema (Proxy @LastPrekey))),
--             ("client", clientSchema)
--           ]
--   pure $
--     NamedSchema (Just "UserLegalHoldStatusResponse") $
--       mempty
--         & properties .~ properties_
--         & required .~ ["status"]
--         & minProperties .~ Just 1
--         & maxProperties .~ Just 3
--         & type_ .~ Just SwaggerObject

--------------------------------------------------------------------------------
-- RemoveLegalHoldSettingsRequest

data RemoveLegalHoldSettingsRequest = RemoveLegalHoldSettingsRequest
  { rmlhsrPassword :: Maybe PlainTextPassword
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RemoveLegalHoldSettingsRequest)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema RemoveLegalHoldSettingsRequest)

instance ToSchema RemoveLegalHoldSettingsRequest where
  schema =
    object "RemoveLegalHoldSettingsRequest" $
      RemoveLegalHoldSettingsRequest
        <$> rmlhsrPassword .= maybe_ (optField "password" schema)

--------------------------------------------------------------------------------
-- DisableLegalHoldForUserRequest

data DisableLegalHoldForUserRequest = DisableLegalHoldForUserRequest
  { dlhfuPassword :: Maybe PlainTextPassword
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform DisableLegalHoldForUserRequest)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema DisableLegalHoldForUserRequest)

instance ToSchema DisableLegalHoldForUserRequest where
  schema =
    object "DisableLegalHoldForUserRequest" $
      DisableLegalHoldForUserRequest
        <$> dlhfuPassword .= maybe_ (optField "password" schema)

--------------------------------------------------------------------------------
-- ApproveLegalHoldForUserRequest

data ApproveLegalHoldForUserRequest = ApproveLegalHoldForUserRequest
  { alhfuPassword :: Maybe PlainTextPassword
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ApproveLegalHoldForUserRequest)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema ApproveLegalHoldForUserRequest)

instance ToSchema ApproveLegalHoldForUserRequest where
  schema =
    object "ApproveLegalHoldForUserRequest" $
      ApproveLegalHoldForUserRequest
        <$> alhfuPassword .= maybe_ (optField "password" schema)

----------------------------------------------------------------------
-- helpers

-- arbitraryExample :: QC.Arbitrary a => a
-- arbitraryExample = QC.unGen QC.arbitrary (QC.mkQCGen 0) 30

-- camelToUnderscore :: String -> String
-- camelToUnderscore = concatMap go . (ix 0 %~ toLower)
--   where
--     go x = if isUpper x then "_" <> [toLower x] else [x]

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

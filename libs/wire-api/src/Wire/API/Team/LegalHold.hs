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

import Control.Lens (at, (?~))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Id
import Data.LegalHold
import Data.Misc
import Data.OpenApi qualified as S hiding (info)
import Data.Proxy
import Data.Schema
import Deriving.Aeson
import Imports
import Wire.API.Provider
import Wire.API.Provider.Service (ServiceKeyPEM)
import Wire.API.User.Client.Prekey
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

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
data LHServiceStatus = Configured | NotConfigured | Disabled
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema LHServiceStatus)

instance ToSchema LHServiceStatus where
  schema =
    enum @Text "LHServiceStatus" $
      mconcat
        [ element "configured" Configured,
          element "not_configured" NotConfigured,
          element "disabled" Disabled
        ]

instance ToSchema ViewLegalHoldService where
  schema =
    object "ViewLegalHoldService" $
      toOutput
        .= recordSchema
        `withParser` validateViewLegalHoldService
    where
      toOutput :: ViewLegalHoldService -> (LHServiceStatus, Maybe ViewLegalHoldServiceInfo)
      toOutput = \case
        ViewLegalHoldService info -> (Configured, Just info)
        ViewLegalHoldServiceNotConfigured -> (NotConfigured, Nothing)
        ViewLegalHoldServiceDisabled -> (Disabled, Nothing)

      recordSchema :: ObjectSchema SwaggerDoc (LHServiceStatus, Maybe ViewLegalHoldServiceInfo)
      recordSchema =
        (,)
          <$> fst .= field "status" schema
          <*> snd .= maybe_ (optField "settings" schema)

      validateViewLegalHoldService :: (LHServiceStatus, Maybe ViewLegalHoldServiceInfo) -> A.Parser ViewLegalHoldService
      validateViewLegalHoldService (Configured, Just info) =
        pure $ ViewLegalHoldService info
      validateViewLegalHoldService (Disabled, _) =
        pure ViewLegalHoldServiceDisabled
      validateViewLegalHoldService (NotConfigured, _) =
        pure ViewLegalHoldServiceNotConfigured
      validateViewLegalHoldService _ = fail "status (one of configured, not_configured, disabled)"

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
  schema =
    object "ViewLegalHoldServiceInfo" $
      ViewLegalHoldServiceInfo
        <$> viewLegalHoldServiceTeam .= field "team_id" schema
        <*> viewLegalHoldServiceUrl .= field "base_url" schema
        <*> viewLegalHoldServiceFingerprint .= field "fingerprint" schema
        <*> viewLegalHoldServiceAuthToken .= field "auth_token" schema
        <*> viewLegalHoldServiceKey .= field "public_key" schema

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

--------------------------------------------------------------------------------
-- RemoveLegalHoldSettingsRequest

data RemoveLegalHoldSettingsRequest = RemoveLegalHoldSettingsRequest
  { rmlhsrPassword :: Maybe PlainTextPassword6
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
  { dlhfuPassword :: Maybe PlainTextPassword6
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
  { alhfuPassword :: Maybe PlainTextPassword6
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ApproveLegalHoldForUserRequest)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema ApproveLegalHoldForUserRequest)

instance ToSchema ApproveLegalHoldForUserRequest where
  schema =
    object "ApproveLegalHoldForUserRequest" $
      ApproveLegalHoldForUserRequest
        <$> alhfuPassword .= maybe_ (optField "password" schema)

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

instance ToSchema LegalholdProtectee where
  -- Generated mixed-sums are hard to cover: Just use their existing JSON
  -- representation and add handwritten Swagger docs
  schema = mkSchema docs A.parseJSON (pure . A.toJSON)
    where
      docs :: NamedSwaggerDoc
      docs =
        pure $
          S.NamedSchema (Just "LegalholdProtectee") $
            mempty
              & S.type_ ?~ S.OpenApiObject
              & S.properties . at "tag"
                ?~ S.Inline
                  ( mempty
                      & S.type_ ?~ S.OpenApiString
                      & S.enum_
                        ?~ [ A.toJSON ("ProtectedUser" :: String),
                             A.toJSON ("UnprotectedBot" :: String),
                             A.toJSON ("LegalholdPlusFederationNotImplemented" :: String)
                           ]
                  )
              & S.properties . at "contents"
                ?~ S.Inline
                  ( S.toSchema (Proxy @UserId)
                      & S.description
                        ?~ "A UserId for ProtectedUser, otherwise empty / null."
                  )

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module contains several categories of SCIM-related types:
--
-- * Extensions for @hscim@ types (like 'ScimUserExtra').
-- * Our wrappers over @hscim@ types (like 'ValidScimUser').
-- * Servant-based API types.
-- * Request and response types for SCIM-related endpoints.
module Spar.Scim.Types where

import Imports
import Brig.Types.User       as Brig
import Control.Lens hiding ((.=), Strict)
import Data.Aeson as Aeson
import Data.Aeson.Types as Aeson
import Data.Id
import Servant
import Spar.API.Util
import Spar.Types

import qualified Data.HashMap.Strict              as HM
import qualified Data.Text                        as T
import qualified SAML2.WebSSO                     as SAML
import qualified Web.Scim.Schema.User             as Scim.User
import qualified Web.Scim.Schema.Schema           as Scim
import qualified Web.Scim.Server                  as Scim


----------------------------------------------------------------------------
-- Schemas

userSchemas :: [Scim.Schema]
userSchemas = [Scim.User20, Scim.CustomSchema userExtraURN]

-- | Schema identifier for extra Wire data.
userExtraURN :: Text
userExtraURN = "urn:wire:scim:schemas:profile:1.0"

----------------------------------------------------------------------------
-- @hscim@ extensions and wrappers

-- | Extra Wire-specific data contained in a SCIM user profile.
data ScimUserExtra = ScimUserExtra
  { _sueRichInfo :: RichInfo
  }
  deriving (Eq, Show)

makeLenses ''ScimUserExtra

instance FromJSON ScimUserExtra where
  parseJSON = withObject "ScimUserExtra" $ \(lowercase -> o) -> do
    o .:? T.toLower userExtraURN >>= \case
      Nothing -> pure (ScimUserExtra emptyRichInfo)
      Just (lowercase -> o2) -> do
        _sueRichInfo <- parseRichInfo =<< o2 .: "richinfo"
        pure ScimUserExtra{..}
    where
      lowercase = HM.fromList . map (over _1 T.toLower) . HM.toList

instance ToJSON ScimUserExtra where
  toJSON v = object
    [ userExtraURN .= object
        [ "richInfo" .= _sueRichInfo v
        ]
    ]

-- | Parse 'RichInfo', trying several formats in a row. We have to know how to parse different
-- formats, because not all provisioning agents can send us information in the canonical
-- @ToJSON RichInfo@ format.
--
-- FUTUREWORK: allow strings as well
parseRichInfo :: Aeson.Value -> Aeson.Parser RichInfo
parseRichInfo v =
  normalizeRichInfo <$>
  asum [
    -- Canonical format
      parseJSON @RichInfo v
    -- A list of {type, value} 'RichField's
    , parseJSON @[RichField] v <&> \xs -> RichInfo { richInfoFields = xs }
    -- Otherwise we fail
    , fail "couldn't parse RichInfo"
    ]

-- | SCIM user with 'SAML.UserRef' and mapping to 'Brig.User'.  Constructed by 'validateScimUser'.
--
-- Data contained in '_vsuHandle' and '_vsuName' is guaranteed to a) correspond to the data in
-- the 'Scim.User.User' and b) be valid in regard to our own user schema requirements (only
-- certain characters allowed in handles, etc).
data ValidScimUser = ValidScimUser
  { _vsuUser          :: Scim.User.User ScimUserExtra

    -- SAML SSO
  , _vsuSAMLUserRef   :: SAML.UserRef
      -- ^ (In the future, we may make this a 'Maybe' and allow for
      -- SCIM users without a SAML SSO identity.)

    -- mapping to 'Brig.User'
  , _vsuHandle        :: Handle
  , _vsuName          :: Maybe Name
  , _vsuRichInfo      :: RichInfo
  }
  deriving (Eq, Show)

makeLenses ''ValidScimUser


----------------------------------------------------------------------------
-- Request and response types

-- | Type used for request parameters to 'APIScimTokenCreate'.
data CreateScimToken = CreateScimToken
  { createScimTokenDescr :: Text
  } deriving (Eq, Show)

instance FromJSON CreateScimToken where
  parseJSON = withObject "CreateScimToken" $ \o -> do
    createScimTokenDescr <- o .: "description"
    pure CreateScimToken{..}

instance ToJSON CreateScimToken where
  toJSON CreateScimToken{..} = object
    [ "description" .= createScimTokenDescr
    ]

-- | Type used for the response of 'APIScimTokenCreate'.
data CreateScimTokenResponse = CreateScimTokenResponse
  { createScimTokenResponseToken :: ScimToken
  , createScimTokenResponseInfo  :: ScimTokenInfo
  } deriving (Eq, Show)

instance FromJSON CreateScimTokenResponse where
  parseJSON = withObject "CreateScimTokenResponse" $ \o -> do
    createScimTokenResponseToken <- o .: "token"
    createScimTokenResponseInfo  <- o .: "info"
    pure CreateScimTokenResponse{..}

instance ToJSON CreateScimTokenResponse where
  toJSON CreateScimTokenResponse{..} = object
    [ "token" .= createScimTokenResponseToken
    , "info"  .= createScimTokenResponseInfo
    ]

-- | Type used for responses of endpoints that return a list of SCIM tokens.
-- Wrapped into an object to allow extensibility later on.
--
-- We don't show tokens once they have been created – only their metadata.
data ScimTokenList = ScimTokenList
  { scimTokenListTokens :: [ScimTokenInfo]
  }
  deriving (Eq, Show)

instance FromJSON ScimTokenList where
  parseJSON = withObject "ScimTokenList" $ \o -> do
    scimTokenListTokens <- o .: "tokens"
    pure ScimTokenList{..}

instance ToJSON ScimTokenList where
  toJSON ScimTokenList{..} = object
    [ "tokens" .= scimTokenListTokens
    ]


----------------------------------------------------------------------
-- Servant APIs

type APIScim
     = OmitDocs :> "v2" :> Scim.SiteAPI ScimToken ScimUserExtra
  :<|> "auth-tokens" :> APIScimToken

type APIScimToken
     = Header "Z-User" UserId :> APIScimTokenCreate
  :<|> Header "Z-User" UserId :> APIScimTokenDelete
  :<|> Header "Z-User" UserId :> APIScimTokenList

type APIScimTokenCreate
     = ReqBody '[JSON] CreateScimToken
    :> Post '[JSON] CreateScimTokenResponse

type APIScimTokenDelete
     = QueryParam' '[Required, Strict] "id" ScimTokenId
    :> DeleteNoContent '[JSON] NoContent

type APIScimTokenList
     = Get '[JSON] ScimTokenList

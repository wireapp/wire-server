{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Spar.SCIM.Types where

import Imports
import Brig.Types.User       as Brig
import Control.Lens hiding ((.=), Strict)
import Data.Aeson as Aeson
import Data.Id
import Servant
import Spar.API.Util
import Spar.Types

import qualified SAML2.WebSSO as SAML
import qualified Web.SCIM.Schema.User             as SCIM.User
import qualified Web.SCIM.Server                  as SCIM


-- | SCIM user with some details gathered during validation.  Output type
-- of 'validateSCIMUser', input type of 'createValidSCIMUser'.
data ValidSCIMUser = ValidSCIMUser
  { _vsuUser          :: SCIM.User.User
  , _vsuSAMLSubjectId :: Maybe SAML.NameID
    -- ^ if the user should authenticate via SAML, use this subjId.
  , _vsuHandle        :: Handle
  , _vsuName          :: Maybe Name
  }

makeLenses ''ValidSCIMUser


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
     = OmitDocs :> "v2" :> SCIM.SiteAPI ScimToken
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

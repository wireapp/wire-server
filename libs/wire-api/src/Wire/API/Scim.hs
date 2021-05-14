{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
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

-- | This module contains several categories of SCIM-related types:
--
-- * Extensions for @hscim@ types (like 'ScimUserExtra').
-- * Our wrappers over @hscim@ types (like 'ValidScimUser').
-- * Servant-based API types.
-- * Request and response types for SCIM-related endpoints.
module Wire.API.Scim where

import Control.Lens (Prism', makeLenses, prism', (.~), (?~), mapped)
import Control.Monad.Except (throwError)
import qualified Data.Aeson as Aeson
import qualified Data.CaseInsensitive as CI
import Data.Handle (Handle)
import Data.Id (UserId, TeamId, ScimTokenId)
import qualified Data.Map as Map
import Data.Misc (PlainTextPassword)
import Data.Proxy
import Data.Time.Clock (UTCTime)
import Imports
import qualified SAML2.WebSSO as SAML
import SAML2.WebSSO.Test.Arbitrary ()
import Web.Scim.AttrName (AttrName (..))
import qualified Web.Scim.Class.Auth as Scim.Auth
import qualified Web.Scim.Class.Group as Scim.Group
import qualified Web.Scim.Class.User as Scim.User
import Web.Scim.Filter (AttrPath (..))
import qualified Web.Scim.Schema.Error as Scim
import Web.Scim.Schema.PatchOp (Operation (..), Path (NormalPath))
import qualified Web.Scim.Schema.PatchOp as Scim
import Web.Scim.Schema.Schema (Schema (CustomSchema))
import qualified Web.Scim.Schema.Schema as Scim
import qualified Web.Scim.Schema.User as Scim.User
import Wire.API.User.Profile as BT
import Wire.API.User.Identity (Email)
import Wire.API.Spar (ScimToken, ScimTokenInfo)
import qualified Wire.API.User.RichInfo as RI
import Data.Swagger hiding (Operation)

----------------------------------------------------------------------------
-- Schemas

userSchemas :: [Scim.Schema]
userSchemas =
  [ Scim.User20,
    Scim.CustomSchema RI.richInfoAssocListURN,
    Scim.CustomSchema RI.richInfoMapURN
  ]

----------------------------------------------------------------------------
-- @hscim@ extensions and wrappers

data SparTag

instance Scim.User.UserTypes SparTag where
  type UserId SparTag = UserId
  type UserExtra SparTag = ScimUserExtra
  supportedSchemas = userSchemas

instance Scim.Group.GroupTypes SparTag where
  type GroupId SparTag = ()

instance Scim.Auth.AuthTypes SparTag where
  type AuthData SparTag = ScimToken
  type AuthInfo SparTag = ScimTokenInfo

-- | Wrapper to work around complications with type synonym family application in instances.
--
-- Background: 'SparTag' is used to instantiate the open type families in the classes
-- @Scim.UserTypes@, @Scim.GroupTypes@, @Scim.AuthTypes@.  Those type families are not
-- injective, and in general they shouldn't be: it should be possible to map two tags to
-- different user ids, but the same extra user info.  This makes the type of the 'Cql'
-- instance for @'Scim.StoredUser' tag@ undecidable: if the type checker encounters a
-- constraint that gives it the user id and extra info, it can't compute the tag from that to
-- look up the instance.
--
-- Possible solutions:
--
-- * what we're doing here: wrap the type synonyms we can't instantiate into newtypes in the
--   code using hscim.
--
-- * do not instantiate the type synonym, but its value (in this case
--   @Web.Scim.Schema.Meta.WithMeta (Web.Scim.Schema.Common.WithId (Id U) (Scim.User tag))@
--
-- * Use newtypes instead type in hscim.  This will carry around the tag as a data type rather
--   than applying it, which in turn will enable ghc to type-check instances like @Cql
--   (Scim.StoredUser tag)@.
--
-- * make the type classes parametric in not only the tag, but also all the values of the type
--   families, and add functional dependencies, like this: @class UserInfo tag uid extrainfo |
--   (uid, extrainfo) -> tag, tag -> (uid, extrainfo)@.  this will make writing the instances
--   only a little more awkward, but the rest of the code should change very little, as long
--   as we just apply the type families rather than explicitly imposing the class constraints.
--
-- * given a lot of time: extend ghc with something vaguely similar to @AllowAmbigiousTypes@,
--   where the instance typechecks, and non-injectivity errors are raised when checking the
--   constraint that "calls" the instance.  :)
newtype WrappedScimStoredUser tag = WrappedScimStoredUser
  {fromWrappedScimStoredUser :: Scim.User.StoredUser tag}

-- | See 'WrappedScimStoredUser'.
newtype WrappedScimUser tag = WrappedScimUser
  {fromWrappedScimUser :: Scim.User.User tag}

-- | Extra Wire-specific data contained in a SCIM user profile.
data ScimUserExtra = ScimUserExtra
  { _sueRichInfo :: RI.RichInfo
  }
  deriving (Eq, Show)

makeLenses ''ScimUserExtra

instance Aeson.FromJSON ScimUserExtra where
  parseJSON v = ScimUserExtra <$> Aeson.parseJSON v

instance Aeson.ToJSON ScimUserExtra where
  toJSON (ScimUserExtra rif) = Aeson.toJSON rif

instance Scim.Patchable ScimUserExtra where
  applyOperation (ScimUserExtra (RI.RichInfo rinfRaw)) (Operation o (Just (NormalPath (AttrPath (Just (CustomSchema sch)) (AttrName (CI.mk -> ciAttrName)) Nothing))) val)
    | sch == RI.richInfoMapURN =
      let rinf = RI.richInfoMap $ RI.fromRichInfoAssocList rinfRaw
          unrinf = ScimUserExtra . RI.RichInfo . RI.toRichInfoAssocList . (`RI.RichInfoMapAndList` mempty)
       in unrinf <$> case o of
            Scim.Remove ->
              pure $ Map.delete ciAttrName rinf
            _AddOrReplace ->
              case val of
                (Just (Aeson.String textVal)) ->
                  pure $ Map.insert ciAttrName textVal rinf
                _ -> throwError $ Scim.badRequest Scim.InvalidValue $ Just "rich info values can only be text"
    | sch == RI.richInfoAssocListURN =
      let rinf = RI.richInfoAssocList $ RI.fromRichInfoAssocList rinfRaw
          unrinf = ScimUserExtra . RI.RichInfo . RI.toRichInfoAssocList . (mempty `RI.RichInfoMapAndList`)
          matchesAttrName (RI.RichField k _) = k == ciAttrName
       in unrinf <$> case o of
            Scim.Remove ->
              pure $ filter (not . matchesAttrName) rinf
            _AddOrReplace ->
              case val of
                (Just (Aeson.String textVal)) ->
                  let newField = RI.RichField ciAttrName textVal
                      replaceIfMatchesAttrName f = if matchesAttrName f then newField else f
                      newRichInfo =
                        if not $ any matchesAttrName rinf
                          then rinf ++ [newField]
                          else map replaceIfMatchesAttrName rinf
                   in pure newRichInfo
                _ -> throwError $ Scim.badRequest Scim.InvalidValue $ Just "rich info values can only be text"
    | otherwise = throwError $ Scim.badRequest Scim.InvalidValue $ Just "unknown schema, cannot patch"
  applyOperation _ _ = throwError $ Scim.badRequest Scim.InvalidValue $ Just "invalid patch op for rich info"

-- | SCIM user with all the data spar is actively processing.  Constructed by
-- 'validateScimUser', or manually from data obtained from brig to pass them on to scim peers.
-- The idea is that the type we get back from hscim is too general, and
-- we need a second round of parsing (aka validation), of which 'ValidScimUser' is the result.
--
-- Data contained in '_vsuHandle' and '_vsuName' is guaranteed to a) correspond to the data in
-- the 'Scim.User.User' and b) be valid in regard to our own user schema requirements (only
-- certain characters allowed in handles, etc).
--
-- Note that it's ok for us to ignore parts of the content sent to us, as explained
-- [here](https://tools.ietf.org/html/rfc7644#section-3.3): "Since the server is free to alter
-- and/or ignore POSTed content, returning the full representation can be useful to the
-- client, enabling it to correlate the client's and server's views of the new resource."
data ValidScimUser = ValidScimUser
  { _vsuExternalId :: ValidExternalId,
    _vsuHandle :: Handle,
    _vsuName :: BT.Name,
    _vsuRichInfo :: RI.RichInfo,
    _vsuActive :: Bool
  }
  deriving (Eq, Show)

data ValidExternalId
  = EmailAndUref Email SAML.UserRef
  | UrefOnly SAML.UserRef
  | EmailOnly Email
  deriving (Eq, Show, Generic)

-- | Take apart a 'ValidExternalId', using 'SAML.UserRef' if available, otehrwise 'Email'.
runValidExternalId :: (SAML.UserRef -> a) -> (Email -> a) -> ValidExternalId -> a
runValidExternalId doUref doEmail = \case
  EmailAndUref _ uref -> doUref uref
  UrefOnly uref -> doUref uref
  EmailOnly em -> doEmail em

veidUref :: Prism' ValidExternalId SAML.UserRef
veidUref = prism' UrefOnly $
  \case
    EmailAndUref _ uref -> Just uref
    UrefOnly uref -> Just uref
    EmailOnly _ -> Nothing

veidEmail :: Prism' ValidExternalId Email
veidEmail = prism' EmailOnly $
  \case
    EmailAndUref em _ -> Just em
    UrefOnly _ -> Nothing
    EmailOnly em -> Just em

makeLenses ''ValidScimUser
makeLenses ''ValidExternalId

----------------------------------------------------------------------------
-- Request and response types

-- | Type used for request parameters to 'APIScimTokenCreate'.
data CreateScimToken = CreateScimToken
  { -- | Token description (as memory aid for whoever is creating the token)
    createScimTokenDescr :: !Text,
    -- | User password, which we ask for because creating a token is a "powerful" operation
    createScimTokenPassword :: !(Maybe PlainTextPassword)
  }
  deriving (Eq, Show)

instance Aeson.FromJSON CreateScimToken where
  parseJSON = Aeson.withObject "CreateScimToken" $ \o -> do
    createScimTokenDescr <- o Aeson..: "description"
    createScimTokenPassword <- o Aeson..:? "password"
    pure CreateScimToken {..}

-- Used for integration tests
instance Aeson.ToJSON CreateScimToken where
  toJSON CreateScimToken {..} =
    Aeson.object
      [ "description" Aeson..= createScimTokenDescr,
        "password" Aeson..= createScimTokenPassword
      ]

-- | Type used for the response of 'APIScimTokenCreate'.
data CreateScimTokenResponse = CreateScimTokenResponse
  { createScimTokenResponseToken :: ScimToken,
    createScimTokenResponseInfo :: ScimTokenInfo
  }
  deriving (Eq, Show)

-- Used for integration tests
instance Aeson.FromJSON CreateScimTokenResponse where
  parseJSON = Aeson.withObject "CreateScimTokenResponse" $ \o -> do
    createScimTokenResponseToken <- o Aeson..: "token"
    createScimTokenResponseInfo <- o Aeson..: "info"
    pure CreateScimTokenResponse {..}

instance Aeson.ToJSON CreateScimTokenResponse where
  toJSON CreateScimTokenResponse {..} =
    Aeson.object
      [ "token" Aeson..= createScimTokenResponseToken,
        "info" Aeson..= createScimTokenResponseInfo
      ]

-- | Type used for responses of endpoints that return a list of SCIM tokens.
-- Wrapped into an object to allow extensibility later on.
--
-- We don't show tokens once they have been created – only their metadata.
data ScimTokenList = ScimTokenList
  { scimTokenListTokens :: [ScimTokenInfo]
  }
  deriving (Eq, Show)

instance Aeson.FromJSON ScimTokenList where
  parseJSON = Aeson.withObject "ScimTokenList" $ \o -> do
    scimTokenListTokens <- o Aeson..: "tokens"
    pure ScimTokenList {..}

instance Aeson.ToJSON ScimTokenList where
  toJSON ScimTokenList {..} =
    Aeson.object
      [ "tokens" Aeson..= scimTokenListTokens
      ]

-- Swagger

instance ToParamSchema ScimToken where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance ToSchema ScimToken where
  declareNamedSchema _ =
    declareNamedSchema (Proxy @Text)
      & mapped . schema . description ?~ "Authentication token"

instance ToSchema ScimTokenInfo where
  declareNamedSchema _ = do
    teamSchema <- declareSchemaRef (Proxy @TeamId)
    idSchema <- declareSchemaRef (Proxy @ScimTokenId)
    createdAtSchema <- declareSchemaRef (Proxy @UTCTime)
    idpSchema <- declareSchemaRef (Proxy @SAML.IdPId)
    descrSchema <- declareSchemaRef (Proxy @Text)
    return $
      NamedSchema (Just "ScimTokenInfo") $
        mempty
          & type_ .~ Just SwaggerObject
          & properties
            .~ [ ("team", teamSchema),
                 ("id", idSchema),
                 ("created_at", createdAtSchema),
                 ("idp", idpSchema),
                 ("description", descrSchema)
               ]
          & required .~ ["team", "id", "created_at", "description"]

instance ToSchema CreateScimToken where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy @Text)
    return $
      NamedSchema (Just "CreateScimToken") $
        mempty
          & type_ .~ Just SwaggerObject
          & properties
            .~ [ ("description", textSchema),
                 ("password", textSchema)
               ]
          & required .~ ["description"]

instance ToSchema CreateScimTokenResponse where
  declareNamedSchema _ = do
    tokenSchema <- declareSchemaRef (Proxy @ScimToken)
    infoSchema <- declareSchemaRef (Proxy @ScimTokenInfo)
    return $
      NamedSchema (Just "CreateScimTokenResponse") $
        mempty
          & type_ .~ Just SwaggerObject
          & properties
            .~ [ ("token", tokenSchema),
                 ("info", infoSchema)
               ]
          & required .~ ["token", "info"]

instance ToSchema ScimTokenList where
  declareNamedSchema _ = do
    infoListSchema <- declareSchemaRef (Proxy @[ScimTokenInfo])
    return $
      NamedSchema (Just "ScimTokenList") $
        mempty
          & type_ .~ Just SwaggerObject
          & properties
            .~ [ ("tokens", infoListSchema)
               ]
          & required .~ ["tokens"]

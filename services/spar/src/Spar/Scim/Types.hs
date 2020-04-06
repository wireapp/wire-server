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
module Spar.Scim.Types where

import Brig.Types.User as Brig
import Control.Lens hiding ((#), (.=), Strict)
import Data.Aeson as Aeson
import qualified Data.CaseInsensitive as CI
import Data.Handle (Handle)
import Data.Id
import Data.Json.Util ((#))
import qualified Data.Map as Map
import Data.Misc (PlainTextPassword)
import Imports
import qualified SAML2.WebSSO as SAML
import Servant
import Spar.API.Util
import Spar.Types
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
import qualified Web.Scim.Server as Scim

----------------------------------------------------------------------------
-- Schemas

userSchemas :: [Scim.Schema]
userSchemas =
  [ Scim.User20,
    Scim.CustomSchema richInfoAssocListURN,
    Scim.CustomSchema richInfoMapURN
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

newtype WrappedScimStoredUser tag
  = WrappedScimStoredUser
      {fromWrappedScimStoredUser :: Scim.User.StoredUser tag}

-- | See 'WrappedScimStoredUser'.
newtype WrappedScimUser tag
  = WrappedScimUser
      {fromWrappedScimUser :: Scim.User.User tag}

-- | Extra Wire-specific data contained in a SCIM user profile.
data ScimUserExtra
  = ScimUserExtra
      { _sueRichInfo :: RichInfo
      }
  deriving (Eq, Show)

makeLenses ''ScimUserExtra

instance FromJSON ScimUserExtra where
  parseJSON v = ScimUserExtra <$> parseJSON v

instance ToJSON ScimUserExtra where
  toJSON (ScimUserExtra rif) = toJSON rif

instance Scim.Patchable ScimUserExtra where
  applyOperation (ScimUserExtra rinf) (Operation o (Just (NormalPath (AttrPath (Just (CustomSchema schema)) (AttrName attrName) Nothing))) val)
    | schema == richInfoMapURN =
      let ciAttrName = CI.mk attrName
          theMap = richInfoMap rinf
       in case o of
            Scim.Remove ->
              pure $ ScimUserExtra $ rinf {richInfoMap = Map.delete ciAttrName theMap}
            _AddOrReplace ->
              case val of
                (Just (String textVal)) ->
                  pure $ ScimUserExtra $ rinf {richInfoMap = Map.insert ciAttrName textVal theMap}
                _ -> throwError $ Scim.badRequest Scim.InvalidValue $ Just "rich info values can only be text"
    | schema == richInfoAssocListURN =
      let ciAttrName = CI.mk attrName
          matchesAttrName (RichField k _) = k == ciAttrName
          assocList = richInfoAssocList rinf
       in case o of
            Scim.Remove ->
              pure $ ScimUserExtra $ rinf {richInfoAssocList = filter (not . matchesAttrName) assocList}
            _AddOrReplace ->
              case val of
                (Just (String textVal)) ->
                  let newField = RichField ciAttrName textVal
                      replaceIfMatchesAttrName f = if matchesAttrName f then newField else f
                      newRichInfo =
                        if not $ any matchesAttrName assocList
                          then rinf {richInfoAssocList = assocList ++ [newField]}
                          else rinf {richInfoAssocList = map replaceIfMatchesAttrName assocList}
                   in pure $ ScimUserExtra $ newRichInfo
                _ -> throwError $ Scim.badRequest Scim.InvalidValue $ Just "rich info values can only be text"
    | otherwise = throwError $ Scim.badRequest Scim.InvalidValue $ Just "unknown schema, cannot patch"
  applyOperation _ _ = throwError $ Scim.badRequest Scim.InvalidValue $ Just "invalid patch op for rich info"

-- | SCIM user with 'SAML.UserRef' and mapping to 'Brig.User'.  Constructed by 'validateScimUser'.
--
-- Data contained in '_vsuHandle' and '_vsuName' is guaranteed to a) correspond to the data in
-- the 'Scim.User.User' and b) be valid in regard to our own user schema requirements (only
-- certain characters allowed in handles, etc).
data ValidScimUser
  = ValidScimUser
      { _vsuUser :: Scim.User.User SparTag,
        -- SAML SSO

        -- | (In the future, we may make this a 'Maybe' and allow for
        -- SCIM users without a SAML SSO identity.)
        _vsuSAMLUserRef :: SAML.UserRef,
        _vsuIdp :: IdP,
        -- mapping to 'Brig.User'
        _vsuHandle :: Handle,
        _vsuName :: Maybe Name,
        _vsuRichInfo :: RichInfo
      }
  deriving (Eq, Show)

makeLenses ''ValidScimUser

----------------------------------------------------------------------------
-- Request and response types

-- | Type used for request parameters to 'APIScimTokenCreate'.
data CreateScimToken
  = CreateScimToken
      { -- | Token description (as memory aid for whoever is creating the token)
        createScimTokenDescr :: !Text,
        -- | User password, which we ask for because creating a token is a "powerful" operation
        createScimTokenPassword :: !(Maybe PlainTextPassword)
      }
  deriving (Eq, Show)

instance FromJSON CreateScimToken where
  parseJSON = withObject "CreateScimToken" $ \o -> do
    createScimTokenDescr <- o .: "description"
    createScimTokenPassword <- o .:? "password"
    pure CreateScimToken {..}

-- Used for integration tests
instance ToJSON CreateScimToken where
  toJSON CreateScimToken {..} =
    object $
      "description" .= createScimTokenDescr
        # "password" .= createScimTokenPassword
        # []

-- | Type used for the response of 'APIScimTokenCreate'.
data CreateScimTokenResponse
  = CreateScimTokenResponse
      { createScimTokenResponseToken :: ScimToken,
        createScimTokenResponseInfo :: ScimTokenInfo
      }
  deriving (Eq, Show)

-- Used for integration tests
instance FromJSON CreateScimTokenResponse where
  parseJSON = withObject "CreateScimTokenResponse" $ \o -> do
    createScimTokenResponseToken <- o .: "token"
    createScimTokenResponseInfo <- o .: "info"
    pure CreateScimTokenResponse {..}

instance ToJSON CreateScimTokenResponse where
  toJSON CreateScimTokenResponse {..} =
    object
      [ "token" .= createScimTokenResponseToken,
        "info" .= createScimTokenResponseInfo
      ]

-- | Type used for responses of endpoints that return a list of SCIM tokens.
-- Wrapped into an object to allow extensibility later on.
--
-- We don't show tokens once they have been created – only their metadata.
data ScimTokenList
  = ScimTokenList
      { scimTokenListTokens :: [ScimTokenInfo]
      }
  deriving (Eq, Show)

instance FromJSON ScimTokenList where
  parseJSON = withObject "ScimTokenList" $ \o -> do
    scimTokenListTokens <- o .: "tokens"
    pure ScimTokenList {..}

instance ToJSON ScimTokenList where
  toJSON ScimTokenList {..} =
    object
      [ "tokens" .= scimTokenListTokens
      ]

----------------------------------------------------------------------
-- Servant APIs

type APIScim =
  OmitDocs :> "v2" :> Scim.SiteAPI SparTag
    :<|> "auth-tokens" :> APIScimToken

type APIScimToken =
  Header "Z-User" UserId :> APIScimTokenCreate
    :<|> Header "Z-User" UserId :> APIScimTokenDelete
    :<|> Header "Z-User" UserId :> APIScimTokenList

type APIScimTokenCreate =
  ReqBody '[JSON] CreateScimToken
    :> Post '[JSON] CreateScimTokenResponse

type APIScimTokenDelete =
  QueryParam' '[Required, Strict] "id" ScimTokenId
    :> DeleteNoContent '[JSON] NoContent

type APIScimTokenList =
  Get '[JSON] ScimTokenList

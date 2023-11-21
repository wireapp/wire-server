{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Wire.API.User.Test
  ( WithSanitizedUserIdentity (..),
    coherenizeNewUser,
    coherenizeUser,
    coherenizeUserAccount,
    coherenizeUserIdentity,
    coherenizeUAuthId,
  )
where

import Data.Aeson
import Data.OpenApi
import Data.Proxy
import Imports
import Test.QuickCheck
import Wire.API.User qualified as User
import Wire.API.User.Identity qualified as User.Identity

-- | Some `UserIdentity` values, or values of types containing `UserIdentity`, just as
-- `NewUser`, have values that can be represented in Haskell, but lead to parse errors on the
-- way back in aeson roundtrip tests.  If you give a "sanitized" instance for those types
-- wrapped in `WithSanitizedUserIdentity`, you can run the roundtrip test on the wrapped type.
data WithSanitizedUserIdentity a = WithSanitizedUserIdentity a
  deriving (Eq, Show)

instance ToJSON a => ToJSON (WithSanitizedUserIdentity a) where
  toJSON (WithSanitizedUserIdentity a) = toJSON a

instance FromJSON a => FromJSON (WithSanitizedUserIdentity a) where
  parseJSON = fmap WithSanitizedUserIdentity . parseJSON

instance ToSchema a => ToSchema (WithSanitizedUserIdentity a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @a)

coherenizeNewUser :: User.NewUser -> Gen User.NewUser
coherenizeNewUser = fixUAuth >=> matchOrigin
  where
    fixUAuth nu = do
      ua' <- mapM coherenizeUserIdentity (User.newUserIdentity nu)
      pure nu {User.newUserIdentity = ua'}

    matchOrigin nu = pure $ do
      case (User.newUserIdentity nu, User.newUserOrigin nu) of
        ( Just (User.Identity.UAuthIdentity (User.Identity.UAuthId saml scim eml tid) mbemail),
          Just (User.NewUserOriginTeamUser (User.NewTeamMemberSSO tid'))
          )
            | tid /= tid' ->
                nu {User.newUserIdentity = Just (User.Identity.UAuthIdentity (User.Identity.UAuthId saml scim eml tid') mbemail)}
        _ -> nu

coherenizeUser :: User.User -> Gen User.User
coherenizeUser = fixUAuth >=> matchTeam
  where
    fixUAuth u = do
      ua' <- mapM coherenizeUserIdentity (User.userIdentity u)
      pure u {User.userIdentity = ua'}

    matchTeam u = pure $ do
      case (User.userIdentity u, User.userTeam u) of
        (Just (User.Identity.UAuthIdentity (User.Identity.UAuthId saml scim eml tid) mbemail), Just tid')
          | tid /= tid' ->
              u {User.userIdentity = Just (User.Identity.UAuthIdentity (User.Identity.UAuthId saml scim eml tid') mbemail)}
        _ -> u

coherenizeUserAccount :: User.UserAccount -> Gen User.UserAccount
coherenizeUserAccount (User.UserAccount u s) = User.UserAccount <$> coherenizeUser u <*> pure s

coherenizeUserIdentity :: User.Identity.UserIdentity tf -> Gen (User.Identity.UserIdentity tf)
coherenizeUserIdentity (User.Identity.UAuthIdentity ua eml) =
  (`User.Identity.UAuthIdentity` eml) <$> coherenizeUAuthId ua
coherenizeUserIdentity u = pure u

coherenizeUAuthId :: User.PartialUAuthId -> Gen User.PartialUAuthId
coherenizeUAuthId =
  nonEmptyId
    >=> scimNeedsEmail
  where
    nonEmptyId (User.Identity.UAuthId Nothing Nothing eml tid) = do
      scim <- arbitrary
      pure $ User.Identity.UAuthId Nothing (Just scim) eml tid
    nonEmptyId u = pure u

    scimNeedsEmail (User.Identity.UAuthId Nothing (Just scim) Nothing tid) = do
      eml <- arbitrary
      pure $ User.Identity.UAuthId Nothing (Just scim) (Just eml) tid
    scimNeedsEmail u = pure u

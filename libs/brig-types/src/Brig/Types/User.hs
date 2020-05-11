{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
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

module Brig.Types.User
  ( ManagedByUpdate (..),
    RichInfoUpdate (..),
    PasswordResetPair,

    -- * re-exports
    Pict (..),
    noPict,
    UserHandleInfo (..),
    CheckHandles (..),
    SelfProfile (..),
    connectedProfile,
    publicProfile,
    User (..),
    userEmail,
    userPhone,
    userSSOId,
    UserProfile (..),
    RichInfo (..),
    RichInfoAssocList (..),
    toRichInfoAssocList,
    richInfoMapURN,
    richInfoAssocListURN,
    RichField (..),
    richInfoAssocListSize,
    richInfoMapSize,
    normalizeRichInfo,
    normalizeRichInfoAssocList,
    emptyRichInfoAssocList,
    NewUser (..),
    ExpiresIn,
    NewUserOrigin (..),
    newUserInvitationCode,
    newUserTeam,
    newUserEmail,
    newUserPhone,
    newUserSSOId,
    InvitationCode (..),
    BindingNewTeamUser (..),
    NewTeamUser (..),
    NewUserPublic (..),
    UserUpdate (..),
    LocaleUpdate (..),
    EmailUpdate (..),
    PhoneUpdate (..),
    HandleUpdate (..),
    DeleteUser (..),
    mkDeleteUser,
    VerifyDeleteUser (..),
    mkVerifyDeleteUser,
    DeletionCodeTimeout (..),
    NewPasswordReset (..),
    PasswordResetKey (..),
    PasswordResetCode (..),
    PasswordResetIdentity (..),
    CompletePasswordReset (..),
    PasswordChange (..),
    PasswordReset (..),
    module C,
  )
where

import Brig.Types.Common as C
import Data.Aeson
import Imports
import Wire.API.User
import Wire.API.User.Handle
import Wire.API.User.Password
import Wire.API.User.Profile
import Wire.API.User.RichInfo

-- used internally
newtype ManagedByUpdate = ManagedByUpdate {mbuManagedBy :: ManagedBy} deriving (Eq, Show, Generic)

-- used internally
newtype RichInfoUpdate = RichInfoUpdate {riuRichInfo :: RichInfoAssocList} deriving (Eq, Show, Generic)

instance FromJSON ManagedByUpdate where
  parseJSON = withObject "managed-by-update" $ \o ->
    ManagedByUpdate <$> o .: "managed_by"

instance ToJSON ManagedByUpdate where
  toJSON m = object ["managed_by" .= mbuManagedBy m]

instance FromJSON RichInfoUpdate where
  parseJSON = withObject "rich-info-update" $ \o ->
    RichInfoUpdate <$> o .: "rich_info"

instance ToJSON RichInfoUpdate where
  toJSON (RichInfoUpdate rif) = object ["rich_info" .= rif]

type PasswordResetPair = (PasswordResetKey, PasswordResetCode)

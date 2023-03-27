{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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

-- | This module contains several categories of SCIM-related types:
--
-- * Extensions for @hscim@ types (like 'ScimUserExtra').
-- * Our wrappers over @hscim@ types (like 'ValidScimUser').
-- * Servant-based API types.
-- * Request and response types for SCIM-related endpoints.
module Spar.Scim.Types where

import Brig.Types.Intra (AccountStatus (..))
import Control.Lens (view)
import Imports
import qualified Web.Scim.Schema.Common as Scim
import qualified Web.Scim.Schema.User as Scim.User
import Wire.API.User.RichInfo (RichInfo (..), normalizeRichInfoAssocList)
import Wire.API.User.Scim (ScimUserExtra (..), SparTag, sueRichInfo)

-- TODO: move these somewhere else?
scimActiveFlagFromAccountStatus :: AccountStatus -> Bool
scimActiveFlagFromAccountStatus = \case
  Active -> True
  Suspended -> False
  Deleted -> False
  Ephemeral -> True -- do not treat ephemeral users any different from active ones.
  PendingInvitation -> False

-- | The second argument is constructed from a (potentially missing) json object field, hence
-- @Nothing@ has the same meaning as @Just True@.  This way, we stay consistent between the
-- original status and one after an update.
--
-- FUTUREWORK: 'Ephemeral' shouldn't really be possible here, since there is no use case for
-- it.  (If there was, this is most likely how we would have to implement it, but still.)  We
-- should change the types so that the 'Ephemeral' case can be ruled out by the compiler.
scimActiveFlagToAccountStatus :: AccountStatus -> Maybe Bool -> AccountStatus
scimActiveFlagToAccountStatus oldstatus = \case
  Nothing -> activate
  Just True -> activate
  Just False -> deactivate
  where
    deactivate = case oldstatus of
      Active -> Suspended
      Suspended -> Suspended
      Deleted -> Deleted -- this shouldn't happen, but it's harmless if it does.
      Ephemeral -> Ephemeral -- never suspend ephemeral users via scim.  doesn't really make sense anyway.
      PendingInvitation -> PendingInvitation

    activate = case oldstatus of
      Active -> Active
      Suspended -> Active
      Deleted -> Deleted -- this shouldn't happen, but it's harmless if it does.
      Ephemeral -> Ephemeral
      PendingInvitation -> PendingInvitation -- (do not activate: see 'scimActiveFlagFromAccountStatus')

normalizeLikeStored :: Scim.User.User SparTag -> Scim.User.User SparTag
normalizeLikeStored usr =
  usr
    { Scim.User.extra = tweakExtra $ Scim.User.extra usr,
      Scim.User.active = tweakActive $ Scim.User.active usr,
      Scim.User.phoneNumbers = []
    }
  where
    tweakExtra :: ScimUserExtra -> ScimUserExtra
    tweakExtra = ScimUserExtra . RichInfo . normalizeRichInfoAssocList . unRichInfo . view sueRichInfo

    tweakActive :: Maybe Scim.ScimBool -> Maybe Scim.ScimBool
    tweakActive = Just . Scim.ScimBool . maybe True Scim.unScimBool

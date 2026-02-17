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
module Spar.Scim.Types
  ( -- * Status mapping
    scimActiveFlagFromAccountStatus,
    scimActiveFlagToAccountStatus,

    -- * Normalization
    normalizeLikeStored,

    -- * Patching
    expandPatch,

    -- * Creation status
    ScimUserCreationStatus (..),
  )
where

import Control.Lens (view)
import Imports
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (elements)
import Web.Scim.Filter (AttrPath (..), ValuePath (..))
import qualified Web.Scim.Schema.Common as Scim
import Web.Scim.Schema.PatchOp (Patch (..), PatchOp (..))
import Web.Scim.Schema.Schema (Schema (CustomSchema))
import qualified Web.Scim.Schema.User as Scim.User
import Wire.API.User (AccountStatus (..))
import Wire.API.User.RichInfo (RichInfo (..), normalizeRichInfoAssocList)
import qualified Wire.API.User.RichInfo as RI
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

expandPatch :: Patch SparTag -> Patch SparTag
expandPatch (Patch ops) = Patch (concatMap expandOp ops)
  where
    expandOp op = case op of
      PatchOpAdd mbPath val -> expand mbPath (PatchOpAdd Nothing val) (\p -> PatchOpAdd (Just p) val)
      PatchOpRemove mbPath -> expand mbPath (PatchOpRemove Nothing) (PatchOpRemove . Just)
      PatchOpReplace mbPath val -> expand mbPath (PatchOpReplace Nothing val) (\p -> PatchOpReplace (Just p) val)

    expand Nothing opDef _ = [opDef]
    expand (Just (ValuePath (AttrPath (Just (CustomSchema urn)) attr mbSub) mbFilter)) _ opCtor
      | urn == RI.richInfoMapURN || urn == RI.richInfoAssocListURN =
          let p1 = ValuePath (AttrPath (Just (CustomSchema RI.richInfoMapURN)) attr mbSub) mbFilter
              p2 = ValuePath (AttrPath (Just (CustomSchema RI.richInfoAssocListURN)) attr mbSub) mbFilter
           in [opCtor p1, opCtor p2]
    expand (Just vp) _ opCtor = [opCtor vp]

data ScimUserCreationStatus = ScimUserCreating | ScimUserCreated
  deriving (Eq, Show, Generic)

instance Arbitrary ScimUserCreationStatus where
  arbitrary = elements [ScimUserCreating, ScimUserCreated]

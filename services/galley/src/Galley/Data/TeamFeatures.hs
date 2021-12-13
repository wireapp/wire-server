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

module Galley.Data.TeamFeatures (HasStatusCol (..), HasLockStatusCol (..), MaybeHasLockStatusCol (..)) where

import Imports
import Wire.API.Team.Feature

-- | Because not all so called team features are actually team-level features,
-- not all of them have a corresponding column in the database. Therefore,
-- instead of having a function like:
--
--   statusCol :: TeamFeatureName -> String
--
-- there is a need for turning it into a class and then defining an instance for
-- team features that do have a database column.
class HasStatusCol (a :: TeamFeatureName) where
  statusCol :: String

instance HasStatusCol 'TeamFeatureLegalHold where statusCol = "legalhold_status"

instance HasStatusCol 'TeamFeatureSSO where statusCol = "sso_status"

instance HasStatusCol 'TeamFeatureSearchVisibility where statusCol = "search_visibility_status"

instance HasStatusCol 'TeamFeatureValidateSAMLEmails where statusCol = "validate_saml_emails"

instance HasStatusCol 'TeamFeatureDigitalSignatures where statusCol = "digital_signatures"

instance HasStatusCol 'TeamFeatureAppLock where statusCol = "app_lock_status"

instance HasStatusCol 'TeamFeatureFileSharing where statusCol = "file_sharing"

instance HasStatusCol 'TeamFeatureConferenceCalling where statusCol = "conference_calling"

instance HasStatusCol 'TeamFeatureSelfDeletingMessages where statusCol = "self_deleting_messages_status"

instance HasStatusCol 'TeamFeatureGuestLinks where statusCol = "guest_links_status"

----------------------------------------------------------------------
class HasLockStatusCol (a :: TeamFeatureName) where
  lockStatusCol :: String

class MaybeHasLockStatusCol (a :: TeamFeatureName) where
  maybeLockStatusCol :: Maybe String

instance {-# OVERLAPPABLE #-} HasLockStatusCol a => MaybeHasLockStatusCol a where
  maybeLockStatusCol = Just (lockStatusCol @a)

----------------------------------------------------------------------
instance HasLockStatusCol 'TeamFeatureSelfDeletingMessages where
  lockStatusCol = "self_deleting_messages_lock_status"

instance MaybeHasLockStatusCol 'TeamFeatureGuestLinks where maybeLockStatusCol = Nothing

instance MaybeHasLockStatusCol 'TeamFeatureLegalHold where maybeLockStatusCol = Nothing

instance MaybeHasLockStatusCol 'TeamFeatureSSO where maybeLockStatusCol = Nothing

instance MaybeHasLockStatusCol 'TeamFeatureSearchVisibility where maybeLockStatusCol = Nothing

instance MaybeHasLockStatusCol 'TeamFeatureValidateSAMLEmails where maybeLockStatusCol = Nothing

instance MaybeHasLockStatusCol 'TeamFeatureDigitalSignatures where maybeLockStatusCol = Nothing

instance MaybeHasLockStatusCol 'TeamFeatureAppLock where maybeLockStatusCol = Nothing

instance MaybeHasLockStatusCol 'TeamFeatureFileSharing where maybeLockStatusCol = Nothing

instance MaybeHasLockStatusCol 'TeamFeatureConferenceCalling where maybeLockStatusCol = Nothing

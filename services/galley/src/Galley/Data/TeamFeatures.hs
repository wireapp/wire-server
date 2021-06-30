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

module Galley.Data.TeamFeatures
  ( getFeatureStatusNoConfig,
    setFeatureStatusNoConfig,
    getApplockFeatureStatus,
    setApplockFeatureStatus,
    getClassifiedDomainsStatus,
    setClassifiedDomainsStatus
  )
where

import Cassandra
import Data.Domain (mkDomain, domainText)
import Data.Id
import Galley.Data.Instances ()
import Imports
import System.Logger.Class (MonadLogger, field, msg, warn)
import Wire.API.Team.Feature
  ( TeamFeatureName (..),
    TeamFeatureStatus,
    TeamFeatureStatusNoConfig (..),
    TeamFeatureStatusValue (..),
    TeamFeatureStatusWithConfig (..),
  )
import qualified Wire.API.Team.Feature as Public

toStatusCol :: TeamFeatureName -> String
toStatusCol TeamFeatureLegalHold = "legalhold_status"
toStatusCol TeamFeatureSSO = "sso_status"
toStatusCol TeamFeatureSearchVisibility = "search_visibility_status"
toStatusCol TeamFeatureValidateSAMLEmails = "validate_saml_emails"
toStatusCol TeamFeatureDigitalSignatures = "digital_signatures"
toStatusCol TeamFeatureAppLock = "app_lock_status"
toStatusCol TeamFeatureClassifiedDomains = "classified_domains_status"

getFeatureStatusNoConfig ::
  forall (a :: Public.TeamFeatureName) m.
  ( MonadClient m,
    Public.KnownTeamFeatureName a,
    Public.FeatureHasNoConfig a
  ) =>
  TeamId ->
  m (Maybe (TeamFeatureStatus a))
getFeatureStatusNoConfig tid = do
  let q = query1 (select (Public.knownTeamFeatureName @a)) (params Quorum (Identity tid))
  mStatusValue <- (>>= runIdentity) <$> retry x1 q
  pure $ TeamFeatureStatusNoConfig <$> mStatusValue
  where
    select :: TeamFeatureName -> PrepQuery R (Identity TeamId) (Identity (Maybe TeamFeatureStatusValue))
    select feature = fromString $ "select " <> toStatusCol feature <> " from team_features where team_id = ?"

setFeatureStatusNoConfig ::
  forall (a :: Public.TeamFeatureName) m.
  ( MonadClient m,
    Public.KnownTeamFeatureName a,
    Public.FeatureHasNoConfig a
  ) =>
  TeamId ->
  TeamFeatureStatus a ->
  m (TeamFeatureStatus a)
setFeatureStatusNoConfig tid status = do
  let flag = Public.tfwoStatus status
  retry x5 $ write (update (Public.knownTeamFeatureName @a)) (params Quorum (flag, tid))
  pure status
  where
    update :: TeamFeatureName -> PrepQuery W (TeamFeatureStatusValue, TeamId) ()
    update feature = fromString $ "update team_features set " <> toStatusCol feature <> " = ? where team_id = ?"

getApplockFeatureStatus ::
  (MonadClient m) =>
  TeamId ->
  m (Maybe (TeamFeatureStatus 'Public.TeamFeatureAppLock))
getApplockFeatureStatus tid = do
  let q = query1 select (params Quorum (Identity tid))
  mTuple <- retry x1 q
  pure $
    mTuple >>= \(mbStatusValue, mbEnforce, mbTimeout) ->
      TeamFeatureStatusWithConfig <$> mbStatusValue <*> (Public.TeamFeatureAppLockConfig <$> mbEnforce <*> mbTimeout)
  where
    select :: PrepQuery R (Identity TeamId) (Maybe TeamFeatureStatusValue, Maybe Public.EnforceAppLock, Maybe Int32)
    select =
      fromString $
        "select " <> toStatusCol Public.TeamFeatureAppLock <> ", app_lock_enforce, app_lock_inactivity_timeout_secs "
          <> "from team_features where team_id = ?"

setApplockFeatureStatus ::
  (MonadClient m) =>
  TeamId ->
  TeamFeatureStatus 'Public.TeamFeatureAppLock ->
  m (TeamFeatureStatus 'Public.TeamFeatureAppLock)
setApplockFeatureStatus tid status = do
  let statusValue = Public.tfwcStatus status
      enforce = Public.applockEnforceAppLock . Public.tfwcConfig $ status
      timeout = Public.applockInactivityTimeoutSecs . Public.tfwcConfig $ status
  retry x5 $ write update (params Quorum (statusValue, enforce, timeout, tid))
  pure status
  where
    update :: PrepQuery W (TeamFeatureStatusValue, Public.EnforceAppLock, Int32, TeamId) ()
    update =
      fromString $
        "update team_features set "
          <> toStatusCol Public.TeamFeatureAppLock
          <> " = ?, "
          <> "app_lock_enforce = ?, "
          <> "app_lock_inactivity_timeout_secs = ? "
          <> "where team_id = ?"

getClassifiedDomainsStatus ::
  (MonadClient m, MonadLogger m) =>
  TeamId ->
  m (Maybe (TeamFeatureStatus 'Public.TeamFeatureClassifiedDomains))
getClassifiedDomainsStatus tid = do
  let q = query1 select (params Quorum (Identity tid))
  mTuple <- retry x1 q
  case mTuple of
    Nothing -> pure Nothing
    Just (mbStatusValue, mbDomainsText) -> do
      let eitherDomains = map mkDomain $ concatMap Cassandra.fromSet mbDomainsText
          (domainParseErrors, parsedDomains) = partitionEithers eitherDomains
      unless (null domainParseErrors) $ do
        warn $
          msg ("Some domains failed to parse in classified domains" :: ByteString)
            . field ("errors" :: ByteString) (intercalate ", " domainParseErrors)
            . field ("tid" :: ByteString) (show tid)
      pure $ TeamFeatureStatusWithConfig <$> mbStatusValue <*> Just (Public.TeamFeatureClassifiedDomainsConfig parsedDomains)
  where
    select :: PrepQuery R (Identity TeamId) (Maybe TeamFeatureStatusValue, Maybe (Cassandra.Set Text))
    select =
      fromString $
        "select " <> toStatusCol Public.TeamFeatureClassifiedDomains <> ", classified_domains_domains "
          <> "from team_features where team_id = ?"

setClassifiedDomainsStatus ::
  (MonadClient m) =>
  TeamId ->
  TeamFeatureStatus 'Public.TeamFeatureClassifiedDomains ->
  m (TeamFeatureStatus 'Public.TeamFeatureClassifiedDomains)
setClassifiedDomainsStatus tid status = do
  let statusValue = Public.tfwcStatus status
      domains = Public.classifiedDomainsDomains . Public.tfwcConfig $ status
  retry x5 $ write update (params Quorum (statusValue, Cassandra.Set $ domainText <$> domains, tid))
  pure status
  where
    update :: PrepQuery W (TeamFeatureStatusValue, Cassandra.Set Text, TeamId) ()
    update =
      fromString $
        "update team_features set "
          <> toStatusCol Public.TeamFeatureClassifiedDomains
          <> " = ?, "
          <> "classified_domains_domains = ? "
          <> "where team_id = ?"

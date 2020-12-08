{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}

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

module Work where

import Brig.Types hiding (Client)
import Cassandra
import Data.Conduit
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as C
import Data.Id
import Data.Misc
import Galley.Data.Instances ()
import Imports
import System.Logger (Logger)
import qualified System.Logger as Log
import UnliftIO.Async (pooledMapConcurrentlyN)
import Wire.API.Team.Feature

deriving instance Cql Name

pageSize :: Int32
pageSize = 1000

runCommand :: Logger -> Brig -> ClientState -> ClientState -> IO ()
runCommand l brig spar galley = do
  runConduit $
    zipSources
      (C.sourceList [(1 :: Int32) ..])
      (transPipe (runClient brig) getTeamMembers)
      .| C.mapM
        ( \(i, tids) -> do
            Log.info l (Log.field "number of members processed: " (show (i * pageSize)))
            pure (runIdentity <$> tids)
        )
      .| C.mapM_ (\tids -> runClient galley (writeSsoFlags tids))

getTeamMembers :: ConduitM () [Identity TeamMember] Client ()
getTeamMembers = paginateC cql (paramsP Quorum () pageSize) x5
  where
    cql :: PrepQuery R () (Identity TeamId)
    cql = "select team from idp"

writeSsoFlags :: [TeamId] -> Client ()
writeSsoFlags = mapM_ (`setSSOTeamConfig` TeamFeatureEnabled)
  where
    setSSOTeamConfig :: MonadClient m => TeamId -> TeamFeatureStatusValue -> m ()
    setSSOTeamConfig tid ssoTeamConfigStatus = do
      retry x5 $ write updateSSOTeamConfig (params Quorum (ssoTeamConfigStatus, tid))
    updateSSOTeamConfig :: PrepQuery W (TeamFeatureStatusValue, TeamId) ()
    updateSSOTeamConfig = "update team_features set sso_status = ? where team_id = ?"



module Work where

import Brig.Types.Intra (AccountStatus (..))
import Cassandra
import Cassandra.Util (Writetime, writeTimeToUTC)
import Conduit
import Control.Lens (view, _1, _2)
import Data.Aeson (FromJSON, (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Conduit.List as C
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.UUID
import qualified Database.Bloodhound as ES
import Imports
import System.Logger (Logger)
import qualified System.Logger as Log

runCommand :: Logger -> ClientState -> ES.BHEnv -> String -> String -> IO ()
runCommand l cas es indexStr mappingStr = do
  let index = ES.IndexName $ Text.pack indexStr
      mapping = ES.MappingName $ Text.pack mappingStr
  runConduit $
    transPipe (ES.runBH es) $
      getScrolled index mapping
        .| C.iterM (logProgress l)
        .| C.mapM
          ( \uuids -> do
              fromCas <- runClient cas $ usersInCassandra uuids
              pure (uuids, fromCas)
          )
        .| C.mapM_ (logDifference l)

----------------------------------------------------------------------------
-- Queries

logProgress :: MonadIO m => Logger -> [UUID] -> m ()
logProgress l uuids = Log.info l $ Log.field "Progress" (show $ length uuids)

logDifference :: Logger -> ([UUID], [(UUID, Maybe AccountStatus, Maybe (Writetime ()))]) -> ES.BH IO ()
logDifference l (uuidsFromES, fromCas) = do
  let noStatusUuidsFromCas = filter (isNothing . view _2) fromCas
      deletedUuidsFromCas = filter ((== Just Deleted) . view _2) fromCas
      extraUuids = Set.difference (Set.fromList uuidsFromES) (Set.fromList $ map (view _1) fromCas)
  mapM_ (logUUID l "NoStatus") noStatusUuidsFromCas
  mapM_ (logUUID l "Deleted") deletedUuidsFromCas
  mapM_ (logUUID l "Extra" . (,Nothing,Nothing)) extraUuids

logUUID :: MonadIO m => Logger -> ByteString -> (UUID, Maybe AccountStatus, Maybe (Writetime ())) -> m ()
logUUID l f (uuid, _, time) =
  Log.info l $
    Log.msg f
      . Log.field "uuid" (show uuid)
      . Log.field "write time" (show $ writeTimeToUTC <$> time)

getScrolled :: (ES.MonadBH m, MonadThrow m) => ES.IndexName -> ES.MappingName -> ConduitM () [UUID] m ()
getScrolled index mapping = processRes =<< lift (ES.getInitialScroll index mapping esSearch)
  where
    processRes :: (ES.MonadBH m, MonadThrow m) => Either ES.EsError (ES.SearchResult User) -> ConduitM () [UUID] m ()
    processRes = \case
      Left e -> throwM $ EsError e
      Right res ->
        case map docId $ extractHits res of
          [] -> pure ()
          ids -> do
            yield ids
            processRes
              =<< (\scrollId -> lift (ES.advanceScroll scrollId 120))
              =<< extractScrollId res

esFilter :: ES.Filter
esFilter = ES.Filter $ ES.QueryExistsQuery (ES.FieldName "normalized")

chunkSize :: Int
chunkSize = 10000

esSearch :: ES.Search
esSearch = (ES.mkSearch Nothing (Just esFilter)) {ES.size = ES.Size chunkSize}

extractHits :: ES.SearchResult User -> [User]
extractHits = mapMaybe ES.hitSource . ES.hits . ES.searchHits

extractScrollId :: MonadThrow m => ES.SearchResult a -> m ES.ScrollId
extractScrollId res = maybe (throwM NoScrollId) pure (ES.scrollId res)

usersInCassandra :: [UUID] -> Client [(UUID, Maybe AccountStatus, Maybe (Writetime ()))]
usersInCassandra users = retry x1 $ query cql (params Quorum (Identity users))
  where
    cql :: PrepQuery R (Identity [UUID]) (UUID, Maybe AccountStatus, Maybe (Writetime ()))
    cql = "SELECT id, status, writetime(status) from user where id in ?"

newtype User = User {docId :: UUID}

instance FromJSON User where
  parseJSON = Aeson.withObject "User" $ \o -> User <$> o .: "id"

data WorkError
  = NoScrollId
  | EsError ES.EsError
  deriving (Show, Eq)

instance Exception WorkError

type Name = Text

-- FUTUREWORK: you can avoid this by loading brig-the-service as a library:
-- @"services/brig/src/Brig/Data/Instances.hs:165:instance Cql AccountStatus where"@
instance Cql AccountStatus where
  ctype = Tagged IntColumn

  toCql Active = CqlInt 0
  toCql Suspended = CqlInt 1
  toCql Deleted = CqlInt 2
  toCql Ephemeral = CqlInt 3
  toCql PendingInvitation = CqlInt 4

  fromCql (CqlInt i) = case i of
    0 -> return Active
    1 -> return Suspended
    2 -> return Deleted
    3 -> return Ephemeral
    4 -> return PendingInvitation
    n -> Left $ "unexpected account status: " ++ show n
  fromCql _ = Left "account status: int expected"

{-

select user, json from rich_info where user in $UIDS

CREATE TABLE brig_test.login_codes (
    user uuid PRIMARY KEY,
    code text,
    retries int,
    timeout timestamp
)

CREATE TABLE brig_test.user_cookies (
    user uuid,
    expires timestamp,
    id bigint,
    created timestamp,
    label text,
    succ_id bigint,
    type int,
    PRIMARY KEY (user, expires, id)
)

CREATE TABLE brig_test.user_handle (
    handle text PRIMARY KEY,
    user uuid
)

CREATE TABLE brig_test.team_invitation_email (
    email text,
    team uuid,
    code ascii,
    invitation uuid,
    PRIMARY KEY (email, team)
)

CREATE TABLE brig_test.team_invitation (
    team uuid,
    id uuid,
    code ascii,
    created_at timestamp,
    created_by uuid,
    email text,
    name text,
    phone text,
    role int,
    PRIMARY KEY (team, id)
)

CREATE TABLE brig_test.user (
    id uuid PRIMARY KEY,
    accent list<float>,
    accent_id int,
    activated boolean,
    assets list<frozen<asset>>,
    country ascii,
    email text,
    expires timestamp,
    handle text,
    language ascii,
    managed_by int,
    name text,
    password blob,
    phone text,
    picture list<blob>,
    provider uuid,
    searchable boolean,
    service uuid,
    sso_id text,
    status int,
    team uuid
)

CREATE TABLE brig_test.properties (
    user uuid,
    key ascii,
    value blob,
    PRIMARY KEY (user, key)
)

CREATE TABLE brig_test.prekeys (
    user uuid,
    client text,
    key int,
    data text,
    PRIMARY KEY (user, client, key)
)

CREATE TABLE brig_test.password_reset (
    key ascii PRIMARY KEY,
    code ascii,
    retries int,
    timeout timestamp,
    user uuid
)

CREATE TABLE brig_test.clients (
    user uuid,
    client text,
    class int,
    cookie text,
    ip inet,
    label text,
    lat double,
    lon double,
    model text,
    tstamp timestamp,
    type int,
    PRIMARY KEY (user, client)
)

CREATE TABLE brig_test.budget (
    key text PRIMARY KEY,
    budget int
)

CREATE TABLE brig_test.connection (
    left uuid,
    right uuid,
    conv uuid,
    last_update timestamp,
    message text,
    status int,
    PRIMARY KEY (left, right)
)

CREATE TABLE brig_test.meta (
    id int,
    version int,
    date timestamp,
    descr text,
    PRIMARY KEY (id, version)
)

CREATE TABLE brig_test.activation_keys (
    key ascii PRIMARY KEY,
    challenge ascii,
    code ascii,
    key_text text,
    key_type ascii,
    retries int,
    user uuid
)

CREATE TABLE brig_test.vcodes (
    key ascii,
    scope int,
    account uuid,
    email text,
    phone text,
    retries int,
    value ascii,
    PRIMARY KEY (key, scope)
)

CREATE TABLE brig_test.invitation_info (
    code ascii PRIMARY KEY,
    id uuid,
    inviter uuid
)

CREATE TABLE brig_test.codes (
    user uuid,
    scope int,
    code text,
    retries int,
    PRIMARY KEY (user, scope)
)

CREATE TABLE brig_test.user_keys (
    key text PRIMARY KEY,
    user uuid
)

CREATE TABLE brig_test.invitee_info (
    invitee uuid PRIMARY KEY,
    conv uuid,
    inviter uuid
)

CREATE TABLE brig_test.id_mapping (
    mapped_id uuid PRIMARY KEY,
    remote_domain text,
    remote_id uuid
)

----------------------------------------------------------------------

CREATE TABLE brig_test.team_invitation_info (
    code ascii PRIMARY KEY,
    id uuid,
    team uuid
)

CREATE TABLE brig_test.user_keys_hash (
    key blob PRIMARY KEY,
    key_type int,
    user uuid
)

----------------------------------------------------------------------

CREATE TYPE brig_test.asset (
    typ int,
    key text,
    size int
);

CREATE TABLE brig_test.unique_claims (
    value text PRIMARY KEY,
    claims set<uuid>
)

-}

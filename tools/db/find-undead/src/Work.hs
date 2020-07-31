{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

  fromCql (CqlInt i) = case i of
    0 -> return Active
    1 -> return Suspended
    2 -> return Deleted
    3 -> return Ephemeral
    n -> fail $ "unexpected account status: " ++ show n
  fromCql _ = fail "account status: int expected"

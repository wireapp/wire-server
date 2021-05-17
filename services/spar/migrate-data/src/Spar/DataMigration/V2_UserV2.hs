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
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Spar.DataMigration.V2_UserV2 (migration) where

import Cassandra
import qualified Conduit as C
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as CL
import Data.Id
import qualified Data.Map.Strict as Map
import Data.String.Conversions (cs)
import Imports
import qualified SAML2.WebSSO as SAML
import Spar.Data.Instances ()
import Spar.DataMigration.Types (logger)
import Spar.DataMigration.Types hiding (logger)
import Spar.Types (NormalizedUNameID (unNormalizedUNameID), normalizeQualifiedNameId)
import qualified System.Logger as Log

-- row in spar.user
-- primary key are the first two entries
type OldRow = (SAML.Issuer, CqlSafe SAML.NameID, UserId)

-- row in spar.user_v2
-- primary key are the first two entries
type NewRow = (SAML.Issuer, NormalizedUNameID, SAML.NameID, UserId)

-- Roughly equivalent to a mapping NewRow->[OldRow]. If there are multiple
-- entries per key this means there is a collision of tow old rows to a new row
type MigrationMapInverse = Map (SAML.Issuer, NormalizedUNameID) [(SAML.NameID, UserId)]

type EnvIO = ReaderT Env IO

-- Resolve collisions in MigrationMapInverse
-- Left =~ collision could not be resolved
-- Right =~ collision could be resolved to 1 "winner"
type CollisionResolver =
  (SAML.Issuer, NormalizedUNameID) ->
  [(SAML.NameID, UserId)] ->
  EnvIO (Either [(SAML.NameID, UserId)] (SAML.NameID, UserId))

-- | Use this if you want to paginate without crashing
newtype CqlSafe a = CqlSafe {unCqlSafe :: Either String a}

instance Cql a => Cql (CqlSafe a) where
  ctype = Tagged $ untag (ctype @a)
  toCql _ = error "CqlSafe is not meant for serialization"
  fromCql val =
    case fromCql @a val of
      Left err -> Right $ CqlSafe (Left err)
      Right x -> Right $ CqlSafe (Right x)

migration :: Migration
migration =
  Migration
    { version = MigrationVersion 2,
      text = "Migrate to spar.user_v2",
      action = \env ->
        flip runReaderT env $
          case dryRun env of
            DryRun -> performDryRun
            NoDryRun -> performMigration
    }

performDryRun :: EnvIO ()
performDryRun = do
  migMapInv <- collectMapping
  runConduit $
    filterResolved resolveNothing migMapInv
      .| C.sinkNull

performMigration :: EnvIO ()
performMigration = do
  migMapInv <- collectMapping
  runConduit $
    filterResolved resolveViaActivated migMapInv
      .| CL.mapM_ insertNewRow
  where
    insertNewRow :: NewRow -> EnvIO ()
    insertNewRow newRow = do
      runSpar $ write insert (params Quorum newRow)
      where
        insert :: PrepQuery W NewRow ()
        insert = "INSERT INTO user_v2 (issuer, normalized_uname_id, sso_id, uid) VALUES (?, ?, ?, ?)"

logInfo :: String -> EnvIO ()
logInfo msg = do
  logger_ <- asks logger
  Log.info logger_ (Log.msg msg)

runSpar :: Client a -> EnvIO a
runSpar cl = do
  cass <- asks sparCassandra
  runClient cass cl

collectMapping :: EnvIO MigrationMapInverse
collectMapping = do
  runConduit $
    zipSources (CL.sourceList [(1 :: Int32) ..]) readOldRows
      .| migrateRows
      .| CC.foldl collect Map.empty
  where
    readOldRows :: ConduitT () [OldRow] EnvIO ()
    readOldRows = do
      pSize <- lift $ asks pageSize
      transPipe runSpar $
        paginateC select (paramsP Quorum () pSize) x5
      where
        select :: PrepQuery R () OldRow
        select = "SELECT issuer, sso_id, uid FROM user"

    migrateRows :: ConduitT (Int32, [OldRow]) NewRow EnvIO ()
    migrateRows = do
      mb <- await
      case mb of
        Just (page, rows) -> do
          lift $ logInfo $ "page " <> show page
          for_ rows $ \row@(_, _, uid) -> do
            case migrateRow row of
              Left err -> do
                lift $ logInfo $ "Failed to parse row for user " <> show uid <> " with error " <> err
              Right newRow -> yield newRow
          migrateRows
        Nothing -> pure ()

    collect :: MigrationMapInverse -> NewRow -> MigrationMapInverse
    collect migMapInv (issuer, normNid, nid, uid') = Map.insertWith (<>) (issuer, normNid) [(nid, uid')] migMapInv

    migrateRow :: OldRow -> Either String NewRow
    migrateRow (issuer, nidSafe, uid) =
      unCqlSafe nidSafe <&> \nid -> (issuer, normalizeQualifiedNameId nid, nid, uid)

filterResolved :: CollisionResolver -> MigrationMapInverse -> ConduitT () NewRow EnvIO ()
filterResolved resolver migMapInv =
  C.yieldMany (Map.assocs migMapInv) .| go
  where
    go :: ConduitT ((SAML.Issuer, NormalizedUNameID), [(SAML.NameID, UserId)]) NewRow EnvIO ()
    go = do
      mbAssoc <- await
      for_ mbAssoc $ \(new@(issuer, nid), olds) -> do
        lift (resolver new olds) >>= \case
          Left olds' ->
            lift $ logInfo $ unwords ["Couldnt resolve collisision of", show issuer, cs (unNormalizedUNameID nid), show (fmap snd olds')]
          Right (nameId, uid) -> yield (issuer, nid, nameId, uid)
        go

-- for debugging only
-- TODO: remove
resolveNothing :: CollisionResolver
resolveNothing _ [old] = pure (Right old)
resolveNothing _ olds = pure (Left olds)

resolveViaActivated :: CollisionResolver
resolveViaActivated _ [] = pure (Left [])
resolveViaActivated _ [x] = pure (Right x)
resolveViaActivated _ olds = do
  olds' <- filterM isActivated olds
  case olds' of
    [old] -> pure (Right old)
    _ -> pure (Left olds')
  where
    isActivated (_, _uid) = error "TODO: ask brig's DB if user is acitvated"

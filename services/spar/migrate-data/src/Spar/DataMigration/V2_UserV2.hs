{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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

module Spar.DataMigration.V2_UserV2 (migration) where

import Cassandra
import qualified Conduit as C
import qualified Data.ByteString.UTF8 as UTF8
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as CL
import Data.Id
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (UTCTime)
import Imports
import qualified SAML2.WebSSO as SAML
import SAML2.WebSSO.Types (Issuer (..))
import Spar.Data (NormalizedUNameID (unNormalizedUNameID), normalizeQualifiedNameId)
import Spar.Data.Instances ()
import Spar.DataMigration.Types (logger)
import Spar.DataMigration.Types hiding (logger)
import qualified System.Logger as Log
import URI.ByteString (serializeURIRef')

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

data List2 a = List2 a a [a]
  deriving (Show)

-- Resolve collisions in MigrationMapInverse
-- Left =~ collision could not be resolved
-- Right =~ collision could be resolved to 1 "winner"
type CollisionResolver =
  (SAML.Issuer, NormalizedUNameID) ->
  List2 (SAML.NameID, UserId) ->
  EnvIO (Either (List2 (SAML.NameID, UserId)) (SAML.NameID, UserId))

-- | Use this if you want to paginate without crashing
newtype CqlSafe a = CqlSafe {unCqlSafe :: Either String a}

instance (Cql a) => Cql (CqlSafe a) where
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
    filterResolved combinedResolver migMapInv
      .| C.sinkNull

performMigration :: EnvIO ()
performMigration = do
  migMapInv <- collectMapping
  runConduit $
    filterResolved combinedResolver migMapInv
      .| CL.mapM_ insertNewRow
  where
    insertNewRow :: NewRow -> EnvIO ()
    insertNewRow newRow = do
      runSpar $ write insert (params LocalQuorum newRow)
      where
        insert :: PrepQuery W NewRow ()
        insert = "INSERT INTO user_v2 (issuer, normalized_uname_id, sso_id, uid) VALUES (?, ?, ?, ?)"

logInfo :: String -> EnvIO ()
logInfo msg = do
  logger_ <- asks logger
  Log.info logger_ (Log.msg msg)

logError :: String -> EnvIO ()
logError msg = do
  logger_ <- asks logger
  Log.err logger_ (Log.msg msg)

runSpar :: Client a -> EnvIO a
runSpar cl = do
  cass <- asks sparCassandra
  runClient cass cl

runBrig :: Client a -> EnvIO a
runBrig cl = do
  cass <- asks brigCassandra
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
        paginateC select (paramsP LocalQuorum () pSize) x5
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
                lift $ logError $ "Failed to parse row for user " <> show uid <> " with error " <> err
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
        let yieldOld (nameId, uid) = yield (issuer, nid, nameId, uid)
        let issuerURI = UTF8.toString . serializeURIRef' . _fromIssuer $ issuer
        case olds of
          [] -> pure ()
          [old] -> yieldOld old
          (old1 : old2 : rest) ->
            lift (resolver new (List2 old1 old2 rest)) >>= \case
              Left _ ->
                lift $ logError $ unwords ["Couldnt resolve collisision of", issuerURI, T.unpack (unNormalizedUNameID nid), show olds]
              Right old -> do
                lift $ logInfo $ unwords ["Resolved collision", issuerURI, T.unpack (unNormalizedUNameID nid), show (fmap snd olds), "to", show (snd old)]
                yieldOld old
        go

combineResolver :: CollisionResolver -> CollisionResolver -> CollisionResolver
combineResolver resolver1 resolver2 pair olds =
  resolver1 pair olds >>= \case
    Left olds' -> resolver2 pair olds'
    Right old -> pure (Right old)

resolveViaActivated :: CollisionResolver
resolveViaActivated _ input@(List2 old1 old2 rest) = do
  olds' <- filterM (isActivated . snd) (old1 : old2 : rest)
  case olds' of
    [] -> pure (Left input)
    [old] -> pure (Right old)
    (old1' : old2' : rest') -> pure (Left (List2 old1' old2' rest'))
  where
    isActivated :: UserId -> EnvIO Bool
    isActivated u =
      runBrig $
        (== Just (Identity True))
          <$> retry x1 (query1 activatedSelect (params LocalQuorum (Identity u)))

    activatedSelect :: PrepQuery R (Identity UserId) (Identity Bool)
    activatedSelect = "SELECT activated FROM user WHERE id = ?"

-- Resolve to the user with the latest access token.
resolveViaAccessToken :: CollisionResolver
resolveViaAccessToken _ input@(List2 old1 old2 rest) = do
  tps <- for (old1 : old2 : rest) $ \old@(_nid, uid) -> do
    mt <- latestCookieExpiry uid
    pure (old, mt)
  case strictMaximumBy (compareCookieTime `on` snd) tps of
    Nothing -> pure (Left input)
    Just (old, _) -> pure (Right old)
  where
    latestCookieExpiry :: UserId -> EnvIO (Maybe UTCTime)
    latestCookieExpiry uid =
      runBrig $
        runIdentity <$$> query1 select (params LocalQuorum (Identity uid))
      where
        select :: PrepQuery R (Identity UserId) (Identity UTCTime)
        select = "SELECT expires FROM user_cookies WHERE user = ? ORDER BY expires DESC LIMIT 1"

    -- Returns the element (if it exists) that is stricly greater than any other element.
    strictMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
    strictMaximumBy _ [] = Nothing
    strictMaximumBy _ [y] = Just y
    strictMaximumBy ordering ys = case sortBy (flip ordering) ys of
      [] -> Nothing
      [y] -> Just y
      (y1 : y2 : _) -> case y1 `ordering` y2 of
        LT -> Nothing
        EQ -> Nothing
        GT -> Just y1

    compareCookieTime :: Maybe UTCTime -> Maybe UTCTime -> Ordering
    compareCookieTime Nothing Nothing = EQ
    compareCookieTime Nothing (Just _t) = LT
    compareCookieTime (Just _t) Nothing = GT
    compareCookieTime (Just t1) (Just t2) = t1 `compare` t2

combinedResolver :: CollisionResolver
combinedResolver = resolveViaActivated `combineResolver` resolveViaAccessToken

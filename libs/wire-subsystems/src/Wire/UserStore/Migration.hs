{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.UserStore.Migration where

import Cassandra hiding (Set)
import Cassandra.Util
import Conduit
import Data.Conduit.List qualified as C
import Data.Handle
import Data.Id
import Data.Json.Util (UTCTimeMillis)
import Data.Misc
import Data.Time
import Database.CQL.Protocol (Record (..), TupleType)
import Hasql.Pool.Extended
import Hasql.Statement qualified as Hasql
import Hasql.TH (resultlessStatement)
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions (IsolationLevel (ReadCommitted), Mode (..))
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.Conc
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource
import Polysemy.State
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger.Class qualified as Log
import Wire.API.Password
import Wire.API.PostgresMarshall
import Wire.API.User
import Wire.API.User.RichInfo
import Wire.Migration
import Wire.MigrationLock
import Wire.Postgres
import Wire.Sem.Concurrency
import Wire.Sem.Concurrency.IO (unsafelyPerformConcurrency)
import Wire.Sem.Logger
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
import Wire.UserStore.Migration.Types
import Wire.UserStore.Postgres

migrateUsersLoop ::
  MigrationOptions ->
  ClientState ->
  Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateUsersLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "users"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "users")
    (migrateAllUsers migOpts migCounter migDuration)

type EffectStack =
  [ State Int,
    Input ClientState,
    Input Pool,
    Resource,
    Async,
    Race,
    TinyLog,
    Embed IO,
    Concurrency 'Unsafe,
    Final IO
  ]

interpreter :: ClientState -> Pool -> Log.Logger -> ByteString -> Sem EffectStack a -> IO (Int, a)
interpreter cassClient pgPool logger name =
  runFinal
    . unsafelyPerformConcurrency
    . embedToFinal
    . loggerToTinyLog logger
    . mapLogger (Log.field "migration" name .)
    . raiseUnder
    . interpretRace
    . asyncToIOFinal
    . resourceToIOFinal
    . runInputConst pgPool
    . runInputConst cassClient
    . runState 0

migrateAllUsers ::
  ( Member TinyLog r,
    Member (Input ClientState) r,
    Member (Embed IO) r,
    Member (State Int) r,
    Member (Concurrency Unsafe) r,
    Member (Input Pool) r,
    Member Async r,
    Member Race r,
    Member Resource r
  ) =>
  MigrationOptions -> Prometheus.Counter -> Prometheus.Vector Text Prometheus.Histogram -> ConduitM () Void (Sem r) ()
migrateAllUsers migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllUsers")
  withCount (paginateSem select (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize runIdentity
    .| C.mapM_ (unsafePooledMapConcurrentlyN_ migOpts.parallelism (\uid -> handleLockAndDBErrors "user" (migrateUser migOpts.timeout migCounter migDuration uid)))
  where
    select :: PrepQuery R () (Identity UserId)
    select = "select id from user"

migrateUser ::
  ( PGConstraints r,
    Member TinyLog r,
    Member (Error MigrationLockError) r,
    Member Async r,
    Member Race r,
    Member Resource r,
    Member (Input ClientState) r
  ) =>
  Duration -> Prometheus.Counter -> Prometheus.Vector Text Prometheus.Histogram -> UserId -> Sem r ()
migrateUser migTimeout migCounter migDuration uid =
  withExclusiveMigrationLockAndTimeout migTimeout migDuration [uid] $ do
    cState <- input
    mCassData <- runClient cState $ getUserData uid
    case mCassData of
      Nothing -> pure ()
      Just cassData -> do
        let eithPGRow = mkUserRowPG cassData.id cassData.user cassData.isHandleClaimed cassData.richInfo
        case eithPGRow of
          Left e -> warn $ Log.msg (Log.val "Invalid user found, skipping") . Log.field "id" (idToText cassData.id) . Log.field "error" (show e)
          Right pgRow -> do
            saveToPostgres pgRow cassData.serviceConv
            let mServiceTeam = (.teamId) =<< cassData.serviceConv
            runClient cState $ deleteFromCassandra pgRow.id_ pgRow.handle ((,,mServiceTeam) <$> pgRow.providerId <*> pgRow.serviceId)
            markDeletionComplete pgRow.id_
            liftIO $ Prometheus.incCounter migCounter

getUserData :: UserId -> Client (Maybe RawUserData)
getUserData uid = do
  mUserRow <- asRecord <$$> query1 selectUserRow (params LocalQuorum (Identity uid))
  case mUserRow of
    Nothing -> pure Nothing
    Just user -> do
      serviceConv <- case (,) <$> user.providerId <*> user.serviceId of
        Nothing -> pure Nothing
        Just (pid, sid) -> asRecord <$$> query1 selectServiceConv (params LocalQuorum (pid, sid, uid))
      isHandleClaimed <- case user.handle of
        Nothing -> pure False
        Just h -> do
          mClaimedBy <- runIdentity <$$> query1 selectHandleClaim (params LocalQuorum (Identity h))
          -- TODO: log if the handle is claimed by someone else.
          pure $ mClaimedBy == Just uid
      richInfo <- runIdentity <$$> query1 selectRichInfo (params LocalQuorum (Identity uid))
      pure $ Just RawUserData {id = uid, ..}
  where
    selectUserRow :: PrepQuery R (Identity UserId) (TupleType UserRowCass)
    selectUserRow =
      "SELECT accent_id, activated, country, email, email_unvalidated,\
      \expires, feature_conference_calling, handle, language, managed_by, \
      \name, password, provider, searchable, service,\
      \sso_id, status, supported_protocols, team, text_status,\
      \user_type, assets, picture, writetime(activated)\
      \FROM user WHERE id = ?"

    selectServiceConv :: PrepQuery R (ProviderId, ServiceId, UserId) (TupleType ServiceConv)
    selectServiceConv = "SELECT conv, team FROM service_user WHERE provider = ? AND service = ? AND user = ?"

    selectHandleClaim :: PrepQuery R (Identity Handle) (Identity UserId)
    selectHandleClaim = "SELECT user FROM user_handle WHERE handle = ?"

    selectRichInfo :: PrepQuery R (Identity UserId) (Identity RichInfoAssocList)
    selectRichInfo = "SELECT json FROM rich_info where user = ?"

data InvalidUserError = UserHasNoName | UserHasNoActivated
  deriving (Show)

mkUserRowPG :: UserId -> UserRowCass -> Bool -> Maybe RichInfoAssocList -> Either InvalidUserError UserRowPG
mkUserRowPG id_ cass@UserRowCass {..} isHandleClaimed richInfo = run . runError $ do
  pgName <- note UserHasNoName cass.name
  pgActivated <- note UserHasNoActivated cass.activated
  createdAt <- note UserHasNoActivated $ writetimeToUTC <$> cass.activatedWriteTime
  pure $
    UserRowPG
      { accentId = fromMaybe defaultAccentId cass.accentId,
        userType = fromMaybe UserTypeRegular cass.userType,
        name = pgName,
        activated = pgActivated,
        handle = if isHandleClaimed then cass.handle else Nothing,
        ..
      }

{- ORMOLU_DISABLE -}
type UserTuplePG =
  (UserId, ColourId, Bool, Maybe Country, Maybe EmailAddress,
   Maybe EmailAddress, Maybe UTCTimeMillis, Maybe Int32, Maybe Handle, Maybe Language,
   Maybe ManagedBy, Name, Maybe Password, Maybe ProviderId, Maybe ServiceId,
   Maybe UserSSOId, Maybe AccountStatus, Maybe (Set BaseProtocolTag), Maybe TeamId, Maybe TextStatus,
   UserType, Maybe Pict, Maybe RichInfoAssocList, Maybe Bool, UTCTime
  )

userRowPGToTuple :: UserRowPG -> UserTuplePG
userRowPGToTuple user =
  (user.id_, user.accentId, user.activated, user.country,user.email,
   user.emailUnvalidated, user.expires, user.featureConferenceCalling, user.handle, user.language,
   user.managedBy, user.name, user.password, user.providerId, user.serviceId,
   user.ssoId, user.status, user.supportedProtocols, user.teamId, user.textStatus,
   user.userType, user.pict, user.richInfo, user.searchable, user.createdAt)
{- ORMOLU_ENABLE -}

saveToPostgres :: (PGConstraints r) => UserRowPG -> Maybe ServiceConv -> Sem r ()
saveToPostgres user mServiceConv =
  runTransactionWithRetry ReadCommitted Write $ do
    case user.status of
      Just Deleted ->
        Transaction.statement (user.id_, user.teamId, user.createdAt) insertDeleted
      _ -> do
        Transaction.statement (userRowPGToTuple user) insertUser
        for_ user.assets $ \assets -> do
          Transaction.statement user.id_ deleteAssetsStatement
          Transaction.statement (mkAssetRows user.id_ assets) insertAssetsStatement
        for_ mServiceConv $ \serviceConv ->
          Transaction.statement (user.id_, serviceConv.convId, serviceConv.teamId) insertBotConv
    Transaction.statement user.id_ markPendingDelete
  where
    insertDeleted :: Hasql.Statement (UserId, Maybe TeamId, UTCTime) ()
    insertDeleted =
      lmapPG
        [resultlessStatement|
          INSERT INTO deleted_user
          (id, team, created_at)
          VALUES ($1 :: uuid, $2 :: uuid?, $3 :: timestamptz)
          ON CONFLICT (id) DO NOTHING
        |]
    insertUser :: Hasql.Statement UserTuplePG ()
    insertUser =
      lmapPG
        [resultlessStatement|
          INSERT INTO wire_user
          (id, accent_id, activated, country, email,
           email_unvalidated, expires, feature_conference_calling, handle, language,
           managed_by, name, password, provider, service,
           sso_id, account_status, supported_protocols, team, text_status,
           user_type, picture, rich_info, searchable, created_at
          )
          VALUES
          ($1 :: uuid, $2 :: integer, $3 :: boolean, $4 :: text?, $5 :: text?,
           $6 :: text?, $7 :: timestamptz?, $8 :: integer?, $9 :: text?, $10 :: text?,
           $11 :: integer?, $12 :: text, $13 :: text?, $14 :: uuid?, $15 :: uuid?,
           $16 :: jsonb?, $17 :: integer?, $18 :: integer?, $19 :: uuid?, $20 :: text?,
           $21 :: integer, $22 :: jsonb?, $23 :: jsonb?, $24 :: boolean?, $25 :: timestamptz
          )
          ON CONFLICT (id) DO NOTHING
      |]
    insertBotConv :: Hasql.Statement (UserId, ConvId, Maybe TeamId) ()
    insertBotConv =
      lmapPG
        [resultlessStatement|
          INSERT INTO bot_conv
          (id, conv, conv_team)
          VALUES ($1 :: uuid, $2 :: uuid, $3 :: uuid?)
        |]

    markPendingDelete :: Hasql.Statement UserId ()
    markPendingDelete =
      lmapPG
        [resultlessStatement|
          INSERT INTO user_migration_pending_deletes (id)
          VALUES ($1 :: uuid)
          ON CONFLICT (id) DO NOTHING
        |]

markDeletionComplete :: (PGConstraints r) => UserId -> Sem r ()
markDeletionComplete uid =
  runStatement uid stmt
  where
    stmt :: Hasql.Statement UserId ()
    stmt = lmapPG [resultlessStatement|DELETE FROM user_migration_pending_deletes WHERE id = $1 :: uuid|]

deleteFromCassandra :: UserId -> Maybe Handle -> Maybe (ProviderId, ServiceId, Maybe TeamId) -> Client ()
deleteFromCassandra uid mHandle mService = do
  for_ mHandle $ \handle -> write deleteHandle (params LocalQuorum (Identity handle))
  for_ mService $ \(pid, sid, mTid) -> do
    write deleteServiceUser (params LocalQuorum (pid, sid, uid))
    for_ mTid $ \tid -> write deleteServiceTeam (params LocalQuorum (pid, sid, tid, uid))
  write deleteRichInfo (params LocalQuorum (Identity uid))
  write deleteUser (params LocalQuorum (Identity uid))
  where
    deleteUser :: PrepQuery W (Identity UserId) ()
    deleteUser = "DELETE FROM user WHERE id = ?"

    deleteHandle :: PrepQuery W (Identity Handle) ()
    deleteHandle = "DELETE FROM user_handle WHERE handle = ?"

    deleteServiceUser :: PrepQuery W (ProviderId, ServiceId, UserId) ()
    deleteServiceUser = "DELETE FROM service_user WHERE provider = ? AND service = ? AND user = ?"

    deleteServiceTeam :: PrepQuery W (ProviderId, ServiceId, TeamId, UserId) ()
    deleteServiceTeam = "DELETE FROM service_team WHERE provider = ? AND service = ? AND team = ? AND user = ?"

    deleteRichInfo :: PrepQuery W (Identity UserId) ()
    deleteRichInfo = "DELETE FROM rich_info WHERE user = ?"

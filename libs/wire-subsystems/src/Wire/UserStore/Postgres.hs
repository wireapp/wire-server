{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wwarn #-}

module Wire.UserStore.Postgres where

import Cassandra (GeneralPaginationState (PaginationStatePostgres), PageWithState (..), paginationStatePostgres)
import Control.Error (lastMay)
import Data.Handle
import Data.Id
import Data.Json.Util
import Data.Map qualified as Map
import Data.Qualified (Qualified (qUnqualified))
import Data.Time
import Data.Tuple.Extra (fst3)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hasql.Pipeline qualified as Pipeline
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions
import Imports
import Polysemy
import Wire.API.Asset hiding (Asset)
import Wire.API.Password
import Wire.API.PostgresMarshall
import Wire.API.Team.Feature (FeatureStatus)
import Wire.API.User hiding (DeleteUser)
import Wire.API.User.RichInfo
import Wire.API.User.Search
import Wire.Postgres
import Wire.StoredUser
import Wire.UserStore
import Wire.UserStore.IndexUser

interpretUserStoreCassandra :: (PGConstraints r) => InterpreterFor UserStore r
interpretUserStoreCassandra =
  interpret $ \case
    CreateUser new mbConv -> createUserImpl new mbConv
    ActivateUser uid identity -> activateUserImpl uid identity
    DeactivateUser uid -> deactivateUserImpl uid
    GetUsers uids -> getUsersImpl uids
    DoesUserExist uid -> doesUserExistImpl uid
    GetIndexUser uid -> getIndexUserImpl uid
    GetIndexUsersPaginated pageSize mPagingState -> getIndexUsersPaginatedImpl pageSize (paginationStatePostgres =<< mPagingState)
    UpdateUser uid update -> updateUserImpl uid update
    UpdateEmail uid email -> updateEmailImpl uid (Just email)
    DeleteEmail uid -> updateEmailImpl uid Nothing
    UpdateEmailUnvalidated uid email -> updateEmailUnvalidatedImpl uid (Just email)
    DeleteEmailUnvalidated uid -> updateEmailUnvalidatedImpl uid Nothing
    LookupName uid -> lookupNameImpl uid
    LookupHandle hdl -> lookupHandleImpl hdl
    GlimpseHandle hdl -> lookupHandleImpl hdl
    UpdateUserHandleEither uid update -> updateUserHandleEitherImpl uid update
    UpdateSSOId uid ssoId -> updateSSOIdImpl uid ssoId
    UpdateManagedBy uid managedBy -> updateManagedByImpl uid managedBy
    UpdateAccountStatus uid accountStatus -> updateAccountStatusImpl uid accountStatus
    UpdateRichInfo uid richInfo -> updateRichInfoImpl uid richInfo
    UpdateFeatureConferenceCalling uid feat -> updateFeatureConferenceCallingImpl uid feat
    LookupFeatureConferenceCalling uid -> lookupFeatureConferenceCallingImpl uid
    DeleteUser user -> deleteUserImpl user
    LookupStatus uid -> lookupStatusImpl uid
    IsActivated uid -> isActivatedImpl uid
    LookupLocale uid -> lookupLocaleImpl uid
    GetUserTeam uid -> getUserTeamImpl uid
    UpdateUserTeam uid tid -> updateUserTeamImpl uid tid
    GetRichInfo uid -> getRichInfoImpl uid
    LookupRichInfos uids -> lookupRichInfosImpl uids
    UpsertHashedPassword uid pw -> upsertHashedPasswordImpl uid pw
    LookupHashedPassword uid -> lookupHashedPasswordImpl uid
    GetUserAuthenticationInfo uid -> getUserAuthenticationInfoImpl uid
    SetUserSearchable uid searchable -> setUserSearchableImpl uid searchable
    DeleteServiceUser pid sid bid -> deleteServiceUserImpl pid sid bid
    LookupServiceUsers pid sid mPagingState -> lookupServiceUsersImpl pid sid (paginationStatePostgres =<< mPagingState)
    LookupServiceUsersForTeam pid sid tid mPagingState -> lookupServiceUsersForTeamImpl pid sid tid (paginationStatePostgres =<< mPagingState)

{- ORMOLU_DISABLE -}
type InsertUserRow =
  ( UserId, Name, Maybe TextStatus, Pict, Maybe EmailAddress,
    Maybe UserSSOId, ColourId, Maybe Password, Bool, AccountStatus,
    Maybe UTCTimeMillis, Language, Maybe Country, Maybe ProviderId, Maybe ServiceId,
    Maybe Handle, Maybe TeamId, ManagedBy, Set BaseProtocolTag, Bool,
    UserType
  )
type
  SelectUserRow =
  ( UserId, Name, Maybe TextStatus, Maybe Pict, Maybe EmailAddress, Maybe EmailAddress,
    Maybe UserSSOId, ColourId, Bool, Maybe AccountStatus,
    Maybe UTCTimeMillis, Maybe Language, Maybe Country, Maybe ProviderId, Maybe ServiceId,
    Maybe Handle, Maybe TeamId, Maybe ManagedBy, Maybe (Set BaseProtocolTag), Maybe Bool,
    UserType
  )

storedUserFromRow :: SelectUserRow -> StoredUser
storedUserFromRow (id_, name, textStatus, pict, email, emailUnvalidated,
                   ssoId, accentId, activated, status,
                   expires, language, country, providerId, serviceId,
                   handle, teamId, managedBy, supportedProtocols, searchable,
                   userTypeInDB)
                   = StoredUser{ id = id_,
                                 assets = Nothing,
                                 userType = Just userTypeInDB,
                                 ..
                               }

type SelectIndexUserRow =
  (UserId, Maybe TeamId, Name, Maybe AccountStatus, Maybe Handle,
   Maybe EmailAddress, Maybe EmailAddress, ColourId, Bool, Maybe ServiceId,
   Maybe ManagedBy, Maybe UserSSOId, Maybe Bool, UTCTime, UTCTime)

indexUserFromRow :: SelectIndexUserRow -> IndexUser
indexUserFromRow ( uid, teamId, name, accountStatus, handle,
                   email, unverifiedEmail, colourId, activated, serviceId,
                   managedBy, ssoId, searchable, createdAt, updatedAt
                 ) = IndexUser{userId = uid, ..}
{- ORMOLU_ENABLE -}

createUserImpl :: (PGConstraints r) => NewStoredUser -> Maybe (ConvId, Maybe TeamId) -> Sem r ()
createUserImpl new mbConv =
  runTransaction Serializable Write $ do
    Transaction.statement userRow insertUser
    Transaction.statement new.id deleteAssetsStatement
    Transaction.statement (mkAssetRows new.id new.assets) insertAssetsStatement
    for_ mbConv $ \(convId, mTeamId) -> do
      Transaction.statement (new.id, convId, mTeamId) insertBotConv
  where
    userRow =
      ( new.id,
        new.name,
        new.textStatus,
        new.pict,
        new.email,
        new.ssoId,
        new.accentId,
        new.password,
        new.activated,
        new.status,
        new.expires,
        new.language,
        new.country,
        new.providerId,
        new.serviceId,
        new.handle,
        new.teamId,
        new.managedBy,
        new.supportedProtocols,
        new.searchable,
        new.userType
      )

    insertUser :: Hasql.Statement InsertUserRow ()
    insertUser =
      lmapPG
        [resultlessStatement|
           INSERT INTO wire_user
           (id, name, text_status, picture, email,
           sso_id, accent_id, password, activated, account_status,
           expires, language, country, provider, service,
           handle, team, managed_by, supported_protocols, searchable,
           user_type)
           VALUES
           ($1 :: uuid, $2 :: text, $3 :: text?, $4 :: jsonb, $5 :: text?,
            $6 :: jsonb?, $7 :: integer, $8 :: text?, $9 :: boolean, $10 :: integer,
	    $11 :: timestamptz?, $12 :: text, $13 :: text?, $14 :: uuid?, $15 :: uuid?,
            $16 :: text?, $17 :: uuid?, $18 :: integer, $19 :: integer, $20 :: boolean,
            $21 :: integer)
           ON CONFLICT (id) DO UPDATE
           SET name = EXCLUDED.name,
               text_status = EXCLUDED.text_status,
               picture = EXCLUDED.picture,
               email = EXCLUDED.email,
               sso_id = EXCLUDED.sso_id,
               accent_id = EXCLUDED.accent_id,
               password = EXCLUDED.password,
               activated = EXCLUDED.activated,
               account_status = EXCLUDED.account_status,
               expires = EXCLUDED.expires,
               language = EXCLUDED.language,
               country = EXCLUDED.country,
               provider = EXCLUDED.provider,
               service = EXCLUDED.service,
               handle = EXCLUDED.handle,
               team = EXCLUDED.team,
               managed_by = EXCLUDED.managed_by,
               supported_protocols = EXCLUDED.supported_protocols,
               searchable = EXCLUDED.searchable,
               user_type = EXCLUDED.user_type
        |]

    insertBotConv :: Hasql.Statement (UserId, ConvId, Maybe TeamId) ()
    insertBotConv =
      lmapPG
        [resultlessStatement|
          INSERT INTO bot_conv
          (id, conv, conv_team)
          VALUES
          ($1 :: uuid, $2 :: uuid, $3 :: uuid?)
          ON CONFLICT (id) DO UPDATE
          SET conv = EXCLUDED.conv,
              conv_team = EXCLUDED.conv_team
        |]

mkAssetRows :: UserId -> [Asset] -> ([UserId], [Int32], [AssetKey], [Maybe AssetSize])
mkAssetRows uid assets =
  unzip4 $
    map (\asset -> (uid, 0, asset.assetKey, asset.assetSize)) assets

insertAssetsStatement :: Hasql.Statement ([UserId], [Int32], [AssetKey], [Maybe AssetSize]) ()
insertAssetsStatement =
  lmapPG @(Vector _, Vector _, Vector _, Vector _)
    [resultlessStatement|
       INSERT INTO asset
       (user_id, typ, key, size)
       SELECT * FROM UNNEST ($1 :: uuid[], $2 :: integer[], $3 :: text[], $4 :: integer?[])
     |]

deleteAssetsStatement :: Hasql.Statement UserId ()
deleteAssetsStatement =
  lmapPG
    [resultlessStatement|DELETE FROM asset where user_id = $1 :: uuid|]

getUsersImpl :: (PGConstraints r) => [UserId] -> Sem r [StoredUser]
getUsersImpl uids = do
  (userRows, assetRows) <-
    runPipeline $
      (,)
        <$> Pipeline.statement uids selectUsers
        <*> Pipeline.statement uids selectAssets
  let assetMap =
        foldr
          (\(uid, _, key, size) -> Map.insertWith (<>) uid [ImageAsset key size])
          Map.empty
          assetRows
  pure $
    map
      ( \row ->
          let storedUser = storedUserFromRow row
           in storedUser {assets = Map.lookup storedUser.id assetMap} :: StoredUser
      )
      userRows
  where
    selectUsers :: Hasql.Statement [UserId] [SelectUserRow]
    selectUsers =
      dimapPG @(Vector _)
        [vectorStatement|
          SELECT
          id :: uuid, name :: text, text_status :: text?, picture :: jsonb?, email :: text?, email_unvalidated :: text?,
          sso_id :: jsonb?, accent_id :: integer, activated :: boolean, account_status :: integer?,
          expires :: timestamptz?, language :: text?, country :: text?, provider :: uuid?, service :: uuid?,
          handle :: text?, team :: uuid?, managed_by :: integer?, supported_protocols :: integer?, searchable :: boolean?,
          user_type :: integer
          FROM wire_user
          WHERE id = ANY($1 :: uuid[])
        |]

    -- TODO: Implement this, but make some test fail before implementing
    -- selectDeletedUsers :: Hasql.Statement [UserId] [UserId]
    -- selectDeletedUsers = pure []

    selectAssets :: Hasql.Statement [UserId] [(UserId, Int32, AssetKey, Maybe AssetSize)]
    selectAssets =
      dimapPG @(Vector _)
        [vectorStatement|
          SELECT user_id :: uuid, typ :: integer, key :: text, size :: integer?
          FROM asset
          WHERE user_id = ANY($1 :: uuid[])
        |]

doesUserExistImpl :: (PGConstraints r) => UserId -> Sem r Bool
doesUserExistImpl uid =
  runStatement uid check
  where
    check :: Hasql.Statement UserId Bool
    check =
      lmapPG
        [singletonStatement|
          SELECT EXISTS (
            SELECT 1 FROM wire_user WHERE id = $1 :: uuid
            UNION ALL
            SELECT 1 FROM deleted_user WHERE id = $1 :: uuid
          ) :: bool
        |]

activateUserImpl :: (PGConstraints r) => UserId -> UserIdentity -> Sem r ()
activateUserImpl uid (emailIdentity -> email) =
  runStatement (uid, email) update
  where
    update :: Hasql.Statement (UserId, Maybe EmailAddress) ()
    update =
      lmapPG
        [resultlessStatement|
          UPDATE wire_user
          SET activated = true,
              email = $2 :: text?
          WHERE id = $1 :: uuid
        |]

deactivateUserImpl :: (PGConstraints r) => UserId -> Sem r ()
deactivateUserImpl uid =
  runStatement uid update
  where
    update :: Hasql.Statement UserId ()
    update =
      lmapPG
        [resultlessStatement|
          UPDATE wire_user
          SET activated = false
          WHERE id = $1 :: uuid
        |]

getIndexUserImpl :: (PGConstraints r) => UserId -> Sem r (Maybe IndexUser)
getIndexUserImpl uid = do
  indexUserFromRow <$$> runStatement uid selectUser
  where
    selectUser :: Hasql.Statement UserId (Maybe SelectIndexUserRow)
    selectUser =
      dimapPG
        [maybeStatement|
          SELECT
          id :: uuid, team :: uuid?, name :: text, account_status :: integer?, handle :: text?,
          email :: text?, email_unvalidated :: text?, accent_id :: integer, activated :: Bool, service :: uuid?,
          managed_by :: integer?, sso_id :: jsonb?, searchable :: boolean?, created_at :: timestamptz, updated_at :: timestamptz
          FROM wire_user
          WHERE id = $1 :: uuid
        |]

getIndexUsersPaginatedImpl :: (PGConstraints r) => Int32 -> Maybe UserId -> Sem r (PageWithState UserId IndexUser)
getIndexUsersPaginatedImpl lim mState = do
  rows <- case mState of
    Nothing -> runStatement lim selectStart
    Just startId -> runStatement (startId, lim) selectFrom
  let results = indexUserFromRow <$> rows
  pure
    PageWithState
      { pwsResults = results,
        pwsState = PaginationStatePostgres . (.userId) <$> lastMay results
      }
  where
    selectStart :: Hasql.Statement Int32 [SelectIndexUserRow]
    selectStart =
      dimapPG
        [vectorStatement|
          SELECT
          id :: uuid, team :: uuid?, name :: text, account_status :: integer?, handle :: text?,
          email :: text?, email_unvalidated :: text?, accent_id :: integer, activated :: Bool, service :: uuid?,
          managed_by :: integer?, sso_id :: jsonb?, searchable :: boolean?, created_at :: timestamptz, updated_at :: timestamptz
          FROM wire_user
          ORDER BY id ASC
          LIMIT ($1 :: integer)
        |]

    selectFrom :: Hasql.Statement (UserId, Int32) [SelectIndexUserRow]
    selectFrom =
      dimapPG
        [vectorStatement|
          SELECT
          id :: uuid, team :: uuid?, name :: text, account_status :: integer?, handle :: text?,
          email :: text?, email_unvalidated :: text?, accent_id :: integer, activated :: Bool, service :: uuid?,
          managed_by :: integer?, sso_id :: jsonb?, searchable :: boolean?, created_at :: timestamptz, updated_at :: timestamptz
          FROM wire_user
          WHERE id > ($1 :: uuid)
          ORDER BY id ASC
          LIMIT ($2 :: integer)
        |]

updateUserImpl :: (PGConstraints r) => UserId -> StoredUserUpdate -> Sem r ()
updateUserImpl uid MkStoredUserUpdate {..} =
  runTransaction ReadCommitted Write $ do
    Transaction.statement
      (uid, name, textStatus, pict, accentId, lLanguage <$> locale, lCountry =<< locale, supportedProtocols)
      updateUserFields
    for_ assets $ \newAssets -> do
      Transaction.statement uid deleteAssetsStatement
      Transaction.statement (mkAssetRows uid newAssets) insertAssetsStatement
  where
    updateUserFields :: Hasql.Statement (UserId, Maybe Name, Maybe TextStatus, Maybe Pict, Maybe ColourId, Maybe Language, Maybe Country, Maybe (Set BaseProtocolTag)) ()
    updateUserFields =
      lmapPG
        [resultlessStatement|
          UPDATE wire_user
          SET name =                COALESCE($2 :: text?,    name),
              text_status =         COALESCE($3 :: text?,    text_status),
              picture =             COALESCE($4 :: jsonb?,   picture),
              accent_id =           COALESCE($5 :: integer?, accent_id),
              language =            COALESCE($6 :: text?,    language),
              country =             COALESCE($7 :: text?,    country),
              supported_protocols = COALESCE($8 :: integer?, supported_protocols)
          WHERE id = ($1 :: uuid)
        |]

updateEmailUnvalidatedImpl :: (PGConstraints r) => UserId -> Maybe EmailAddress -> Sem r ()
updateEmailUnvalidatedImpl uid email =
  runStatement (uid, email) update
  where
    update :: Hasql.Statement (UserId, Maybe EmailAddress) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE wire_user SET email_unvalidated = ($2 :: text?) WHERE id = ($1 :: uuid)|]

updateEmailImpl :: (PGConstraints r) => UserId -> Maybe EmailAddress -> Sem r ()
updateEmailImpl uid email =
  runStatement (uid, email) update
  where
    update :: Hasql.Statement (UserId, Maybe EmailAddress) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE wire_user SET email = ($2 :: text?) WHERE id = ($1 :: uuid)|]

lookupNameImpl :: (PGConstraints r) => UserId -> Sem r (Maybe Name)
lookupNameImpl uid = runStatement uid select
  where
    select :: Hasql.Statement UserId (Maybe Name)
    select =
      dimapPG
        [maybeStatement|
          SELECT name :: text
          FROM wire_user
          WHERE id = $1 :: uuid
        |]

lookupHandleImpl :: (PGConstraints r) => Handle -> Sem r (Maybe UserId)
lookupHandleImpl h = runStatement h selectUserIdByHandleStatement

selectUserIdByHandleStatement :: Hasql.Statement Handle (Maybe UserId)
selectUserIdByHandleStatement =
  dimapPG
    [maybeStatement|
      SELECT id :: uuid
      FROM wire_user
      WHERE handle = $1 :: text
    |]

updateUserHandleEitherImpl :: (PGConstraints r) => UserId -> StoredUserHandleUpdate -> Sem r (Either StoredUserUpdateError ())
updateUserHandleEitherImpl uid upd =
  runTransaction ReadCommitted Write $ do
    mOwner <- Transaction.statement upd.new selectUserIdByHandleStatement
    case mOwner of
      Just uid' | uid' /= uid -> pure $ Left StoredUserUpdateHandleExists
      Just _ -> pure $ Right ()
      Nothing -> Right <$> Transaction.statement (uid, upd.new) update
  where
    update :: Hasql.Statement (UserId, Handle) ()
    update =
      lmapPG
        [resultlessStatement|
          UPDATE wire_user
          SET handle = $2 :: text
          WHERE id = $1 :: uuid
        |]

deleteUserImpl :: (PGConstraints r) => User -> Sem r ()
deleteUserImpl user =
  runTransaction ReadCommitted Write $ do
    let uid = user.userQualifiedId.qUnqualified
    Transaction.statement uid delete
    Transaction.statement uid noteDeleted
  where
    delete :: Hasql.Statement UserId ()
    delete =
      lmapPG
        [resultlessStatement|
          DELETE FROM wire_user
          WHERE id = $1 :: uuid
        |]

    noteDeleted :: Hasql.Statement (UserId) ()
    noteDeleted =
      lmapPG
        [resultlessStatement|
          INSERT INTO deleted_user
          (id)
          VALUES ($1 :: uuid)
          ON CONFLICT (id) DO NOTHING
        |]

-- TODO: This probably needs to work for deleted users
lookupStatusImpl :: (PGConstraints r) => UserId -> Sem r (Maybe AccountStatus)
lookupStatusImpl uid =
  join <$> runStatement uid select
  where
    select :: Hasql.Statement UserId (Maybe (Maybe AccountStatus))
    select =
      dimapPG
        [maybeStatement|SELECT account_status :: integer? FROM wire_user WHERE id = $1 :: uuid|]

-- TODO: This probably needs to work for deleted users
isActivatedImpl :: (PGConstraints r) => UserId -> Sem r Bool
isActivatedImpl uid =
  fromMaybe False <$> runStatement uid select
  where
    select :: Hasql.Statement UserId (Maybe Bool)
    select =
      lmapPG
        [maybeStatement|SELECT activated :: bool FROM wire_user WHERE id = $1 :: uuid|]

lookupLocaleImpl :: (PGConstraints r) => UserId -> Sem r (Maybe (Maybe Language, Maybe Country))
lookupLocaleImpl uid =
  runStatement uid select
  where
    select :: Hasql.Statement UserId (Maybe (Maybe Language, Maybe Country))
    select =
      dimapPG
        [maybeStatement|SELECT language :: text?, country :: text? FROM wire_user WHERE id = $1 :: uuid|]

-- TODO: This probably needs to work for deleted users
getUserTeamImpl :: (PGConstraints r) => UserId -> Sem r (Maybe TeamId)
getUserTeamImpl uid =
  join <$> runStatement uid select
  where
    select :: Hasql.Statement UserId (Maybe (Maybe TeamId))
    select =
      dimapPG
        [maybeStatement|SELECT team :: uuid? FROM wire_user WHERE id = $1 :: uuid|]

updateUserTeamImpl :: (PGConstraints r) => UserId -> TeamId -> Sem r ()
updateUserTeamImpl uid tid =
  runStatement (uid, tid) update
  where
    update :: Hasql.Statement (UserId, TeamId) ()
    update =
      dimapPG
        [resultlessStatement|UPDATE wire_user SET team = $2 :: uuid WHERE id = $1 :: uuid|]

-- TODO: This used to work for deleted users, see what breaks if it doesn't, because it really shouldn't.
getRichInfoImpl :: (PGConstraints r) => UserId -> Sem r (Maybe RichInfoAssocList)
getRichInfoImpl uid =
  join <$> runStatement (uid) select
  where
    select :: Hasql.Statement (UserId) (Maybe (Maybe RichInfoAssocList))
    select =
      dimapPG
        [maybeStatement|SELECT rich_info :: json? FROM wire_user WHERE id = $1 :: uuid|]

updateRichInfoImpl :: (PGConstraints r) => UserId -> RichInfoAssocList -> Sem r ()
updateRichInfoImpl uid richInfo =
  runStatement (uid, richInfo) update
  where
    update :: Hasql.Statement (UserId, RichInfoAssocList) ()
    update =
      dimapPG
        [resultlessStatement|UPDATE wire_user SET rich_info = $2 :: jsonb WHERE id = $1 :: uuid|]

lookupRichInfosImpl :: (PGConstraints r) => [UserId] -> Sem r [(UserId, RichInfo)]
lookupRichInfosImpl uids =
  mapMaybe (\(uid, mbRi) -> (uid,) . RichInfo <$> mbRi) <$> runStatement uids select
  where
    select :: Hasql.Statement [UserId] [(UserId, Maybe RichInfoAssocList)]
    select =
      dimapPG @(Vector _)
        [vectorStatement|SELECT id :: uuid, rich_info :: json? FROM wire_user WHERE id = ANY($1 :: uuid[])|]

upsertHashedPasswordImpl :: (PGConstraints r) => UserId -> Password -> Sem r ()
upsertHashedPasswordImpl uid pw = runStatement (uid, pw) upsert
  where
    upsert :: Hasql.Statement (UserId, Password) ()
    upsert =
      lmapPG
        [resultlessStatement|UPDATE wire_user
                             SET password = $2 :: text
                             WHERE id = $1 :: uuid
                            |]

lookupHashedPasswordImpl :: (PGConstraints r) => UserId -> Sem r (Maybe Password)
lookupHashedPasswordImpl uid = join <$> runStatement uid select
  where
    select :: Hasql.Statement UserId (Maybe (Maybe Password))
    select =
      dimapPG
        [maybeStatement|SELECT password :: text? from wire_user where id = $1 :: uuid|]

-- TODO: This used to work for deleted users, see what breaks if it doesn't, because it really shouldn't.
getUserAuthenticationInfoImpl :: (PGConstraints r) => UserId -> Sem r (Maybe (Maybe Password, AccountStatus))
getUserAuthenticationInfoImpl uid =
  withDefaultAccountStatus <$$> runStatement (uid) select
  where
    withDefaultAccountStatus :: (a, Maybe AccountStatus) -> (a, AccountStatus)
    withDefaultAccountStatus (a, mStatus) = (a, fromMaybe Active mStatus)

    select :: Hasql.Statement (UserId) (Maybe (Maybe Password, Maybe AccountStatus))
    select =
      dimapPG
        [maybeStatement|SELECT password :: bytea?, account_status :: integer? FROM wire_user WHERE id = $1 :: uuid|]

-- TODO: This used to work for deleted users, see what breaks if it doesn't, because it really shouldn't.
setUserSearchableImpl :: (PGConstraints r) => UserId -> SetSearchable -> Sem r ()
setUserSearchableImpl uid (SetSearchable searchable) =
  runStatement (uid, searchable) update
  where
    update :: Hasql.Statement (UserId, Bool) ()
    update =
      dimapPG
        [resultlessStatement|UPDATE wire_user SET searchable = $2 :: boolean WHERE id = $1 :: uuid|]

deleteServiceUserImpl :: (PGConstraints r) => ProviderId -> ServiceId -> BotId -> Sem r ()
deleteServiceUserImpl _ _ bid =
  runStatement (botUserId bid) delete
  where
    delete :: Hasql.Statement (UserId) ()
    delete =
      lmapPG
        [resultlessStatement|DELETE FROM bot_conv where id = $1 :: uuid|]

lookupServiceUsersImpl :: (PGConstraints r) => ProviderId -> ServiceId -> Maybe BotId -> Sem r (PageWithState BotId (BotId, ConvId, Maybe TeamId))
lookupServiceUsersImpl _ _ mBotId = do
  bots <- case mBotId of
    Nothing -> runStatement () selectStart
    Just bid -> runStatement bid selectFrom
  pure
    PageWithState
      { pwsState = PaginationStatePostgres . fst3 <$> (bots V.!? (V.length bots - 1)),
        pwsResults = V.toList bots
      }
  where
    selectStart :: Hasql.Statement () (Vector (BotId, ConvId, Maybe TeamId))
    selectStart =
      dimapPG
        [vectorStatement|
          SELECT id :: uuid, conv :: uuid, conv_team :: uuid?
          FROM bot_conv
          ORDER BY id
          LIMIT 100
        |]

    selectFrom :: Hasql.Statement (BotId) (Vector (BotId, ConvId, Maybe TeamId))
    selectFrom =
      dimapPG
        [vectorStatement|
          SELECT id :: uuid, conv :: uuid, conv_team :: uuid?
          FROM bot_conv
          WHERE id > $1 :: uuid
          ORDER BY id
          LIMIT 100
        |]

lookupServiceUsersForTeamImpl :: (PGConstraints r) => ProviderId -> ServiceId -> TeamId -> Maybe BotId -> Sem r (PageWithState BotId (BotId, ConvId))
lookupServiceUsersForTeamImpl _ _ tid mBotId = do
  bots <- case mBotId of
    Nothing -> runStatement (tid) selectStart
    Just bid -> runStatement (tid, bid) selectFrom
  pure
    PageWithState
      { pwsState = PaginationStatePostgres . fst <$> (bots V.!? (V.length bots - 1)),
        pwsResults = V.toList bots
      }
  where
    selectStart :: Hasql.Statement (TeamId) (Vector (BotId, ConvId))
    selectStart =
      dimapPG
        [vectorStatement|
          SELECT id :: uuid, conv :: uuid
          FROM bot_conv
          WHERE conv_team = $1 :: uuid
          ORDER BY id
          LIMIT 100
        |]

    selectFrom :: Hasql.Statement (TeamId, BotId) (Vector (BotId, ConvId))
    selectFrom =
      dimapPG
        [vectorStatement|
          SELECT id :: uuid, conv :: uuid
          FROM bot_conv
          WHERE conv_team = $1 :: uuid
          AND   id > $2 :: uuid
          ORDER BY id
          LIMIT 100
        |]

updateSSOIdImpl :: (PGConstraints r) => UserId -> Maybe UserSSOId -> Sem r Bool
updateSSOIdImpl uid ssoid =
  isJust . join <$> runStatement (uid, ssoid) update
  where
    update :: Hasql.Statement (UserId, Maybe UserSSOId) (Maybe (Maybe TeamId))
    update =
      dimapPG
        [maybeStatement|
          UPDATE wire_user
          SET sso_id = $2 :: jsonb?
          WHERE id = $1 :: uuid
          AND team IS NOT NULL
          RETURNING team :: uuid?
        |]

updateManagedByImpl :: (PGConstraints r) => UserId -> ManagedBy -> Sem r ()
updateManagedByImpl uid managedBy =
  runStatement (uid, managedBy) update
  where
    update :: Hasql.Statement (UserId, ManagedBy) ()
    update =
      lmapPG
        [resultlessStatement|
          UPDATE wire_user
          SET managed_by = $2 :: integer
          WHERE id = $1 :: uuid
        |]

updateAccountStatusImpl :: (PGConstraints r) => UserId -> AccountStatus -> Sem r ()
updateAccountStatusImpl uid status =
  runStatement (uid, status) update
  where
    update :: Hasql.Statement (UserId, AccountStatus) ()
    update =
      lmapPG
        [resultlessStatement|
          UPDATE wire_user
          SET account_status = $2 :: integer
          WHERE id = $1 :: uuid
        |]

updateFeatureConferenceCallingImpl :: (PGConstraints r) => UserId -> Maybe FeatureStatus -> Sem r ()
updateFeatureConferenceCallingImpl uid featureStatus =
  runStatement (uid, featureStatus) update
  where
    update :: Hasql.Statement (UserId, Maybe FeatureStatus) ()
    update =
      lmapPG
        [resultlessStatement|
          UPDATE wire_user
          SET feature_conference_calling = $2 :: integer?
          WHERE id = $1 :: uuid
        |]

lookupFeatureConferenceCallingImpl :: (PGConstraints r) => UserId -> Sem r (Maybe FeatureStatus)
lookupFeatureConferenceCallingImpl uid = join <$> runStatement uid select
  where
    select :: Hasql.Statement UserId (Maybe (Maybe FeatureStatus))
    select =
      dimapPG
        [maybeStatement|SELECT feature_conference_calling :: integer? FROM wire_user WHERE id = $1 :: uuid|]

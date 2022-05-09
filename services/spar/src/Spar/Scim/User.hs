{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

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

-- For @instance UserDB Spar@

-- | Doing operations with users via SCIM.
--
-- Provides a 'Scim.Class.User.UserDB' instance.
module Spar.Scim.User
  ( validateScimUser',
    synthesizeScimUser,
    toScimStoredUser',
    mkValidExternalId,
    scimFindUserByEmail,
  )
where

import Brig.Types.Common (parseEmail)
import Brig.Types.Intra (AccountStatus, UserAccount (accountStatus, accountUser))
import Brig.Types.User (ManagedBy (..), Name (..), User (..))
import qualified Brig.Types.User as BT
import qualified Control.Applicative as Applicative (empty)
import Control.Lens (view, (^.))
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Trans.Except (mapExceptT)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Crypto.Hash (Digest, SHA256, hashlazy)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import Data.Handle (Handle (Handle), parseHandle)
import Data.Id (Id (..), TeamId, UserId, idToText)
import Data.Json.Util (UTCTimeMillis, fromUTCTimeMillis, toUTCTimeMillis)
import Data.String.Conversions (cs)
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import Imports
import Network.URI (URI, parseURI)
import Polysemy
import Polysemy.Input
import qualified SAML2.WebSSO as SAML
import Spar.App (GetUserResult (..), getUserIdByScimExternalId, getUserIdByUref, validateEmail, validateEmailIfExists)
import qualified Spar.Intra.BrigApp as Brig
import Spar.Scim.Auth ()
import Spar.Scim.Types (normalizeLikeStored)
import qualified Spar.Scim.Types as ST
import Spar.Sem.BrigAccess as BrigAccess
import Spar.Sem.GalleyAccess (GalleyAccess)
import Spar.Sem.IdPConfigStore (IdPConfigStore)
import qualified Spar.Sem.IdPConfigStore as IdPConfigStore
import Spar.Sem.SAMLUserStore (SAMLUserStore)
import qualified Spar.Sem.SAMLUserStore as SAMLUserStore
import Spar.Sem.ScimExternalIdStore (ScimExternalIdStore)
import qualified Spar.Sem.ScimExternalIdStore as ScimExternalIdStore
import Spar.Sem.ScimUserTimesStore (ScimUserTimesStore)
import qualified Spar.Sem.ScimUserTimesStore as ScimUserTimesStore
import qualified System.Logger.Class as Log
import System.Logger.Message (Msg)
import qualified URI.ByteString as URIBS
import Util.Logging (logFunction, logHandle, logTeam, logUser, sha256String)
import qualified Web.Scim.Class.User as Scim
import Web.Scim.Filter (Filter (..), rAttrPath, rCompareOp)
import qualified Web.Scim.Filter as Scim
import qualified Web.Scim.Handler as Scim
import qualified Web.Scim.Schema.Common as Scim
import qualified Web.Scim.Schema.Error as Scim
import qualified Web.Scim.Schema.ListResponse as Scim
import qualified Web.Scim.Schema.Meta as Scim
import qualified Web.Scim.Schema.ResourceType as Scim
import qualified Web.Scim.Schema.User as Scim
import qualified Web.Scim.Schema.User as Scim.User (schemas)
import Wire.API.User (Email)
import Wire.API.User.IdentityProvider (IdP)
import qualified Wire.API.User.RichInfo as RI
import Wire.API.User.Saml (Opts, derivedOpts, derivedOptsScimBaseURI, richInfoLimit)
import Wire.API.User.Scim (ScimTokenInfo (..))
import qualified Wire.API.User.Scim as ST
import Wire.Sem.Logger (Logger)
import qualified Wire.Sem.Logger as Logger
import Wire.Sem.Now (Now)
import qualified Wire.Sem.Now as Now
import Wire.Sem.Random (Random)
import qualified Wire.Sem.Random as Random

----------------------------------------------------------------------------
-- UserDB instance

instance
  Members
    '[ Logger (Msg -> Msg),
       Logger String,
       Random,
       Input Opts,
       Now,
       GalleyAccess,
       BrigAccess,
       ScimExternalIdStore,
       ScimUserTimesStore,
       IdPConfigStore,
       SAMLUserStore
     ]
    r =>
  Scim.UserDB ST.SparTag (Sem r)
  where
  getUsers ::
    ScimTokenInfo ->
    Maybe Scim.Filter ->
    Scim.ScimHandler (Sem r) (Scim.ListResponse (Scim.StoredUser ST.SparTag))
  getUsers _ Nothing = do
    throwError $ Scim.badRequest Scim.TooMany (Just "Please specify a filter when getting users.")
  getUsers tokeninfo@ScimTokenInfo {stiTeam, stiIdP} (Just filter') =
    logScim
      ( logFunction "Spar.Scim.User.getUsers"
          . logTokenInfo tokeninfo
          . logFilter filter'
      )
      logScimUserIds
      $ do
        mIdpConfig <- maybe (pure Nothing) (lift . IdPConfigStore.getConfig) stiIdP
        case filter' of
          Scim.FilterAttrCompare (Scim.AttrPath schema attrName _subAttr) Scim.OpEq (Scim.ValString val)
            | Scim.isUserSchema schema -> do
              x <- runMaybeT $ case attrName of
                "username" -> scimFindUserByHandle mIdpConfig stiTeam val
                "externalid" -> scimFindUserByEmail mIdpConfig stiTeam val
                _ -> throwError (Scim.badRequest Scim.InvalidFilter (Just "Unsupported attribute"))
              pure $ Scim.fromList (toList x)
            | otherwise -> throwError $ Scim.badRequest Scim.InvalidFilter (Just "Unsupported schema")
          _ -> throwError $ Scim.badRequest Scim.InvalidFilter (Just "Operation not supported")

  getUser ::
    ScimTokenInfo ->
    UserId ->
    Scim.ScimHandler (Sem r) (Scim.StoredUser ST.SparTag)
  getUser tokeninfo@ScimTokenInfo {stiTeam, stiIdP} uid =
    logScim
      ( logFunction "Spar.Scim.User.getUser"
          . logUser uid
          . logTokenInfo tokeninfo
      )
      logScimUserId
      $ do
        mIdpConfig <- maybe (pure Nothing) (lift . IdPConfigStore.getConfig) stiIdP
        let notfound = Scim.notFound "User" (idToText uid)
        runMaybeT (getUserById mIdpConfig stiTeam uid) >>= maybe (throwError notfound) pure

  postUser ::
    ScimTokenInfo ->
    Scim.User ST.SparTag ->
    Scim.ScimHandler (Sem r) (Scim.StoredUser ST.SparTag)
  postUser tokinfo user = createValidScimUser tokinfo =<< validateScimUser "post" tokinfo user

  putUser ::
    ScimTokenInfo ->
    UserId ->
    Scim.User ST.SparTag ->
    Scim.ScimHandler (Sem r) (Scim.StoredUser ST.SparTag)
  putUser tokinfo uid newScimUser =
    updateValidScimUser tokinfo uid =<< validateScimUser "put" tokinfo newScimUser

  deleteUser :: ScimTokenInfo -> UserId -> Scim.ScimHandler (Sem r) ()
  deleteUser tokeninfo uid =
    deleteScimUser tokeninfo uid

----------------------------------------------------------------------------
-- User creation and validation

-- | Validate a raw SCIM user record and extract data that we care about. See also:
-- 'ValidScimUser''.
validateScimUser ::
  forall m r.
  (m ~ Scim.ScimHandler (Sem r)) =>
  Members '[Input Opts, IdPConfigStore] r =>
  Text ->
  -- | Used to decide what IdP to assign the user to
  ScimTokenInfo ->
  Scim.User ST.SparTag ->
  m ST.ValidScimUser
validateScimUser errloc tokinfo user = do
  mIdpConfig <- tokenInfoToIdP tokinfo
  richInfoLimit <- lift $ inputs richInfoLimit
  validateScimUser' errloc mIdpConfig richInfoLimit user

tokenInfoToIdP :: Member IdPConfigStore r => ScimTokenInfo -> Scim.ScimHandler (Sem r) (Maybe IdP)
tokenInfoToIdP ScimTokenInfo {stiIdP} = do
  maybe (pure Nothing) (lift . IdPConfigStore.getConfig) stiIdP

-- | Validate a handle (@userName@).
validateHandle :: MonadError Scim.ScimError m => Text -> m Handle
validateHandle txt = case parseHandle txt of
  Just h -> pure h
  Nothing ->
    throwError $
      Scim.badRequest
        Scim.InvalidValue
        (Just (txt <> "is not a valid Wire handle"))

-- | Map the SCIM data on the spar and brig schemata, and throw errors if the SCIM data does
-- not comply with the standard / our constraints. See also: 'ValidScimUser'.
--
-- Checks like "is this handle claimed already?" are not performed. Only schema checks.
--
-- __Mapped fields:__
--
--   * @userName@ is mapped to our 'userHandle'.
--
--   * @displayName@ is mapped to our 'userDisplayName'. We don't use the @name@ field, as it
--     provides a rather poor model for names.
--
--   * The @externalId@ is used to construct a 'SAML.UserRef'. If it looks like an email
--     address, the constructed 'SAML.UserRef' will have @nameid-format:emailAddress@,
--     otherwise the format will be @unspecified@.
--
-- FUTUREWORK: We may need to make the SAML NameID type derived from the available SCIM data
-- configurable on a per-team basis in the future, to accomodate different legal uses of
-- @externalId@ by different teams.
--
-- __Emails and phone numbers:__ we'd like to ensure that only verified emails and phone
-- numbers end up in our database, and implementing verification requires design decisions
-- that we haven't made yet. We store them in our SCIM blobs, but don't syncronize them with
-- Brig. See <https://github.com/wireapp/wire-server/pull/559#discussion_r247466760>.
validateScimUser' ::
  forall m.
  (MonadError Scim.ScimError m) =>
  -- | Error location (call site, for debugging)
  Text ->
  -- | IdP that the resulting user will be assigned to
  Maybe IdP ->
  -- | Rich info limit
  Int ->
  Scim.User ST.SparTag ->
  m ST.ValidScimUser
validateScimUser' errloc midp richInfoLimit user = do
  unless (isNothing $ Scim.password user) $
    throwError $
      Scim.badRequest
        Scim.InvalidValue
        (Just $ "Setting user passwords is not supported for security reasons. (" <> errloc <> ")")
  veid <- mkValidExternalId midp (Scim.externalId user)
  handl <- validateHandle . Text.toLower . Scim.userName $ user
  -- FUTUREWORK: 'Scim.userName' should be case insensitive; then the toLower here would
  -- be a little less brittle.
  uname <- do
    let err msg = throwError . Scim.badRequest Scim.InvalidValue . Just $ cs msg <> " (" <> errloc <> ")"
    either err pure $ Brig.mkUserName (Scim.displayName user) veid
  richInfo <- validateRichInfo (Scim.extra user ^. ST.sueRichInfo)
  let active = Scim.active user
  pure $ ST.ValidScimUser veid handl uname richInfo (maybe True Scim.unScimBool active)
  where
    -- Validate rich info (@richInfo@). It must not exceed the rich info limit.
    validateRichInfo :: RI.RichInfo -> m RI.RichInfo
    validateRichInfo richInfo = do
      let sze = RI.richInfoSize richInfo
      when (sze > richInfoLimit) $
        throwError $
          ( Scim.badRequest
              Scim.InvalidValue
              ( Just . cs $
                  show [RI.richInfoMapURN, RI.richInfoAssocListURN]
                    <> " together exceed the size limit: max "
                    <> show richInfoLimit
                    <> " characters, but got "
                    <> show sze
                    <> " ("
                    <> cs errloc
                    <> ")"
              )
          )
            { Scim.status = Scim.Status 413
            }
      pure richInfo

-- | Given an 'externalId' and an 'IdP', construct a 'ST.ValidExternalId'.
--
-- This is needed primarily in 'validateScimUser', but also in 'updateValidScimUser' to
-- recover the 'SAML.UserRef' of the scim user before the update from the database.
mkValidExternalId ::
  forall m.
  (MonadError Scim.ScimError m) =>
  Maybe IdP ->
  Maybe Text ->
  m ST.ValidExternalId
mkValidExternalId _ Nothing = do
  throwError $
    Scim.badRequest
      Scim.InvalidValue
      (Just "externalId is required")
mkValidExternalId Nothing (Just extid) = do
  let err =
        Scim.badRequest
          Scim.InvalidValue
          (Just "externalId must be a valid email address or (if there is a SAML IdP) a valid SAML NameID")
  maybe (throwError err) (pure . ST.EmailOnly) $ parseEmail extid
mkValidExternalId (Just idp) (Just extid) = do
  let issuer = idp ^. SAML.idpMetadata . SAML.edIssuer
  subject <- validateSubject extid
  let uref = SAML.UserRef issuer subject
  pure $ case parseEmail extid of
    Just email -> ST.EmailAndUref email uref
    Nothing -> ST.UrefOnly uref
  where
    -- Validate a subject ID (@externalId@).
    validateSubject :: Text -> m SAML.NameID
    validateSubject txt = do
      unameId :: SAML.UnqualifiedNameID <- do
        let eEmail = SAML.mkUNameIDEmail txt
            unspec = SAML.mkUNameIDUnspecified txt
        pure . either (const unspec) id $ eEmail
      case SAML.mkNameID unameId Nothing Nothing Nothing of
        Right nameId -> pure nameId
        Left err ->
          throwError $
            Scim.badRequest
              Scim.InvalidValue
              (Just $ "Can't construct a subject ID from externalId: " <> Text.pack err)

logScim ::
  forall r a.
  (Member (Logger (Msg -> Msg)) r) =>
  (Msg -> Msg) ->
  (a -> (Msg -> Msg)) ->
  Scim.ScimHandler (Sem r) a ->
  Scim.ScimHandler (Sem r) a
logScim context postcontext action =
  flip mapExceptT action $ \action' -> do
    eith <- action'
    case eith of
      Left e -> do
        let errorMsg =
              case Scim.detail e of
                Just d -> d
                Nothing -> cs (Aeson.encode e)
        Logger.warn $ context . Log.msg errorMsg
        pure (Left e)
      Right x -> do
        Logger.info $ context . postcontext x . Log.msg @Text "call without exception"
        pure (Right x)

logEmail :: Email -> (Msg -> Msg)
logEmail email =
  Log.field "email_sha256" (sha256String . cs . show $ email)

logVSU :: ST.ValidScimUser -> (Msg -> Msg)
logVSU (ST.ValidScimUser veid handl _name _richInfo _active) =
  maybe id logEmail (veidEmail veid)
    . logHandle handl

logTokenInfo :: ScimTokenInfo -> (Msg -> Msg)
logTokenInfo ScimTokenInfo {stiTeam} = logTeam stiTeam

logScimUserId :: Scim.StoredUser ST.SparTag -> (Msg -> Msg)
logScimUserId = logUser . Scim.id . Scim.thing

logScimUserIds :: Scim.ListResponse (Scim.StoredUser ST.SparTag) -> (Msg -> Msg)
logScimUserIds lresp = foldl' (.) id (logScimUserId <$> Scim.resources lresp)

veidEmail :: ST.ValidExternalId -> Maybe Email
veidEmail (ST.EmailAndUref email _) = Just email
veidEmail (ST.UrefOnly _) = Nothing
veidEmail (ST.EmailOnly email) = Just email

-- in ScimTokenHash (cs @ByteString @Text (convertToBase Base64 digest))

-- | Creates a SCIM User.
--
-- User is created in Brig first, and then in SCIM and SAML.
--
-- Rationale: If brig user creation fails halfway, we don't have SCIM records that
-- point to inactive users. This stops people from logging in into inactive users.
--
-- TODO(fisx): weird corner case: what happens when users are created suspended, but are
-- supposed to validate their email?  should emails still be validated?  will that work on
-- suspended users?  (i think it won't, but i haven't checked.)  easy solution would be to
-- disallow creation of suspended users.
--
-- FUTUREWORK(fisx): race conditions.  details in source commends marked with @{}@.
--
-- FUTUREWORK(arianvp): Get rid of manual lifting. Needs to be SCIM instances for ExceptT
-- This is the pain and the price you pay for the horribleness called MTL
createValidScimUser ::
  forall m r.
  (m ~ Scim.ScimHandler (Sem r)) =>
  Members
    '[ Random,
       Now,
       Input Opts,
       Logger (Msg -> Msg),
       Logger String,
       GalleyAccess,
       BrigAccess,
       ScimExternalIdStore,
       ScimUserTimesStore,
       SAMLUserStore
     ]
    r =>
  ScimTokenInfo ->
  ST.ValidScimUser ->
  m (Scim.StoredUser ST.SparTag)
createValidScimUser tokeninfo@ScimTokenInfo {stiTeam} vsu@(ST.ValidScimUser veid handl name richInfo _) =
  logScim
    ( logFunction "Spar.Scim.User.createValidScimUser"
        . logVSU vsu
        . logTokenInfo tokeninfo
    )
    logScimUserId
    $ do
      -- ensure uniqueness constraints of all affected identifiers.
      -- {if we crash now, retry POST will just work}
      assertExternalIdUnused stiTeam veid
      assertHandleUnused handl
      -- {if we crash now, retry POST will just work, or user gets told the handle
      -- is already in use and stops POSTing}

      -- Generate a UserId will be used both for scim user in spar and for brig.
      buid <-
        lift $ do
          buid <-
            ST.runValidExternalIdEither
              ( \uref ->
                  do
                    -- FUTUREWORK: outsource this and some other fragments from
                    -- `createValidScimUser` into a function `createValidScimUserBrig` similar
                    -- to `createValidScimUserSpar`?
                    uid <- Id <$> Random.uuid
                    BrigAccess.createSAML uref uid stiTeam name ManagedByScim
              )
              ( \email -> do
                  BrigAccess.createNoSAML email stiTeam name
              )
              veid

          Logger.debug ("createValidScimUser: brig says " <> show buid)

          -- {If we crash now, we have an active user that cannot login. And can not
          -- be bound this will be a zombie user that needs to be manually cleaned
          -- up.  We should consider making setUserHandle part of createUser and
          -- making it transactional.  If the user redoes the POST A new standalone
          -- user will be created.}
          BrigAccess.setHandle buid handl
          BrigAccess.setRichInfo buid richInfo
          pure buid

      -- {If we crash now,  a POST retry will fail with 409 user already exists.
      -- Azure at some point will retry with GET /Users?filter=userName eq handle
      -- and then issue a PATCH containing the rich info and the externalId.}

      -- By now, vsu that was passed to 'createValidScimUser' may be outdated.  Eg., if user is
      -- invited via scim, we have @active == True@ above, but brig has stored the account in
      -- @AccountStatus == PendingActivation@, which translates to @active == False@.  So we need
      -- to reload the Account from brig.
      storedUser <- do
        acc <-
          lift (BrigAccess.getAccount Brig.WithPendingInvitations buid)
            >>= maybe (throwError $ Scim.serverError "Server error: user vanished") pure
        synthesizeStoredUser acc veid
      lift $ Logger.debug ("createValidScimUser: spar says " <> show storedUser)

      -- {(arianvp): these two actions we probably want to make transactional.}
      createValidScimUserSpar stiTeam buid storedUser veid

      -- If applicable, trigger email validation procedure on brig.
      lift $
        ST.runValidExternalIdEither
          (validateEmailIfExists buid)
          (\_ -> pure () {- nothing to do; user is sent an invitation that validates the address implicitly -})
          veid

      -- TODO: suspension via scim is brittle, and may leave active users behind: if we don't
      -- reach the following line due to a crash, the user will be active.
      lift $ do
        old <- BrigAccess.getStatus buid
        let new = ST.scimActiveFlagToAccountStatus old (Scim.unScimBool <$> active)
            active = Scim.active . Scim.value . Scim.thing $ storedUser
        when (new /= old) $ BrigAccess.setStatus buid new
      pure storedUser

-- | Store scim timestamps, saml credentials, scim externalId locally in spar.  Table
-- `spar.scim_external` gets an entry iff there is no `UserRef`: if there is, we don't do a
-- lookup in that table either, but compute the `externalId` from the `UserRef`.
createValidScimUserSpar ::
  forall m r.
  ( (m ~ Scim.ScimHandler (Sem r)),
    Member ScimExternalIdStore r,
    Member ScimUserTimesStore r,
    Member SAMLUserStore r
  ) =>
  TeamId ->
  UserId ->
  Scim.StoredUser ST.SparTag ->
  ST.ValidExternalId ->
  m ()
createValidScimUserSpar stiTeam uid storedUser veid = lift $ do
  ScimUserTimesStore.write storedUser
  -- This uses the "both" variant to always write all applicable index tables, even if
  -- `spar.scim_external` is never consulted as long as there is an IdP.  This is hoped to
  -- mitigate logic errors in this code and corner cases.  (eg., if the IdP is later removed?)
  ST.runValidExternalIdBoth
    (>>)
    (`SAMLUserStore.insert` uid)
    (\email -> ScimExternalIdStore.insert stiTeam email uid)
    veid

-- TODO(arianvp): how do we get this safe w.r.t. race conditions / crashes?
updateValidScimUser ::
  forall m r.
  Members
    '[ Random,
       Input Opts,
       Logger (Msg -> Msg),
       Logger String,
       Now,
       GalleyAccess,
       BrigAccess,
       ScimExternalIdStore,
       ScimUserTimesStore,
       IdPConfigStore,
       SAMLUserStore
     ]
    r =>
  (m ~ Scim.ScimHandler (Sem r)) =>
  ScimTokenInfo ->
  UserId ->
  ST.ValidScimUser ->
  m (Scim.StoredUser ST.SparTag)
updateValidScimUser tokinfo@ScimTokenInfo {stiTeam} uid newValidScimUser =
  logScim
    ( logFunction "Spar.Scim.User.updateValidScimUser"
        . logVSU newValidScimUser
        . logUser uid
        . logTokenInfo tokinfo
    )
    logScimUserId
    $ do
      oldScimStoredUser :: Scim.StoredUser ST.SparTag <-
        Scim.getUser tokinfo uid
      oldValidScimUser :: ST.ValidScimUser <-
        validateScimUser "recover-old-value" tokinfo . Scim.value . Scim.thing $ oldScimStoredUser

      -- assertions about new valid scim user that cannot be checked in 'validateScimUser' because
      -- they differ from the ones in 'createValidScimUser'.
      assertExternalIdNotUsedElsewhere stiTeam (newValidScimUser ^. ST.vsuExternalId) uid
      assertHandleNotUsedElsewhere uid (newValidScimUser ^. ST.vsuHandle)

      if oldValidScimUser == newValidScimUser
        then pure oldScimStoredUser
        else lift $ do
          newScimStoredUser :: Scim.StoredUser ST.SparTag <-
            updScimStoredUser (synthesizeScimUser newValidScimUser) oldScimStoredUser

          do
            let old = oldValidScimUser ^. ST.vsuExternalId
                new = newValidScimUser ^. ST.vsuExternalId
            when (old /= new) $ do
              updateVsuUref stiTeam uid old new

          when (newValidScimUser ^. ST.vsuName /= oldValidScimUser ^. ST.vsuName) $ do
            BrigAccess.setName uid (newValidScimUser ^. ST.vsuName)

          when (oldValidScimUser ^. ST.vsuHandle /= newValidScimUser ^. ST.vsuHandle) $ do
            BrigAccess.setHandle uid (newValidScimUser ^. ST.vsuHandle)

          when (oldValidScimUser ^. ST.vsuRichInfo /= newValidScimUser ^. ST.vsuRichInfo) $ do
            BrigAccess.setRichInfo uid (newValidScimUser ^. ST.vsuRichInfo)

          BrigAccess.getStatusMaybe uid >>= \case
            Nothing -> pure ()
            Just old -> do
              let new = ST.scimActiveFlagToAccountStatus old (Just $ newValidScimUser ^. ST.vsuActive)
              when (new /= old) $ BrigAccess.setStatus uid new

          ScimUserTimesStore.write newScimStoredUser
          pure newScimStoredUser

updateVsuUref ::
  Members
    '[ GalleyAccess,
       BrigAccess,
       ScimExternalIdStore,
       SAMLUserStore
     ]
    r =>
  TeamId ->
  UserId ->
  ST.ValidExternalId ->
  ST.ValidExternalId ->
  Sem r ()
updateVsuUref team uid old new = do
  case (veidEmail old, veidEmail new) of
    (mo, mn@(Just email)) | mo /= mn -> validateEmail (Just team) uid email
    _ -> pure ()

  old & ST.runValidExternalIdBoth (>>) (SAMLUserStore.delete uid) (ScimExternalIdStore.delete team)
  new & ST.runValidExternalIdBoth (>>) (`SAMLUserStore.insert` uid) (\email -> ScimExternalIdStore.insert team email uid)

  BrigAccess.setVeid uid new

toScimStoredUser' ::
  HasCallStack =>
  UTCTimeMillis ->
  UTCTimeMillis ->
  URIBS.URI ->
  UserId ->
  Scim.User ST.SparTag ->
  Scim.StoredUser ST.SparTag
toScimStoredUser' createdAt lastChangedAt baseuri uid usr =
  Scim.WithMeta meta $
    Scim.WithId uid $
      usr {Scim.User.schemas = ST.userSchemas}
  where
    mkLocation :: String -> URI
    mkLocation pathSuffix = convURI $ baseuri SAML.=/ cs pathSuffix
      where
        convURI uri = fromMaybe err . parseURI . cs . URIBS.serializeURIRef' $ uri
          where
            err = error $ "internal error: " <> show uri
    meta =
      Scim.Meta
        { Scim.resourceType = Scim.UserResource,
          Scim.created = fromUTCTimeMillis createdAt,
          Scim.lastModified = fromUTCTimeMillis lastChangedAt,
          Scim.version = calculateVersion uid usr,
          -- TODO: it looks like we need to add this to the HTTP header.
          -- https://tools.ietf.org/html/rfc7644#section-3.14
          Scim.location = Scim.URI . mkLocation $ "/Users/" <> cs (idToText uid)
        }

updScimStoredUser ::
  forall r.
  Member Now r =>
  Scim.User ST.SparTag ->
  Scim.StoredUser ST.SparTag ->
  Sem r (Scim.StoredUser ST.SparTag)
updScimStoredUser usr storedusr = do
  now <- toUTCTimeMillis <$> Now.get
  pure $ updScimStoredUser' now usr storedusr

updScimStoredUser' ::
  UTCTimeMillis ->
  Scim.User ST.SparTag ->
  Scim.StoredUser ST.SparTag ->
  Scim.StoredUser ST.SparTag
updScimStoredUser' now usr (Scim.WithMeta meta (Scim.WithId scimuid _)) =
  Scim.WithMeta meta' (Scim.WithId scimuid usr)
  where
    meta' =
      meta
        { Scim.lastModified = fromUTCTimeMillis now,
          Scim.version = calculateVersion scimuid usr
        }

deleteScimUser ::
  Members
    '[ Logger (Msg -> Msg),
       BrigAccess,
       ScimExternalIdStore,
       ScimUserTimesStore,
       SAMLUserStore,
       IdPConfigStore
     ]
    r =>
  ScimTokenInfo ->
  UserId ->
  Scim.ScimHandler (Sem r) ()
deleteScimUser tokeninfo@ScimTokenInfo {stiTeam, stiIdP} uid =
  logScim
    ( logFunction "Spar.Scim.User.deleteScimUser"
        . logTokenInfo tokeninfo
        . logUser uid
    )
    (const id)
    $ do
      mbBrigUser <- lift (Brig.getBrigUser Brig.WithPendingInvitations uid)
      case mbBrigUser of
        Nothing -> do
          -- double-deletion gets you a 404.
          throwError $ Scim.notFound "user" (idToText uid)
        Just brigUser -> do
          -- FUTUREWORK: currently it's impossible to delete the last available team owner via SCIM
          -- (because that owner won't be managed by SCIM in the first place), but if it ever becomes
          -- possible, we should do a check here and prohibit it.
          unless (userTeam brigUser == Just stiTeam) $
            -- users from other teams get you a 404.
            throwError $
              Scim.notFound "user" (idToText uid)

          mIdpConfig <- maybe (pure Nothing) (lift . IdPConfigStore.getConfig) stiIdP

          case Brig.veidFromBrigUser brigUser ((^. SAML.idpMetadata . SAML.edIssuer) <$> mIdpConfig) of
            Left _ -> pure ()
            Right veid ->
              lift $
                ST.runValidExternalIdBoth
                  (>>)
                  (SAMLUserStore.delete uid)
                  (ScimExternalIdStore.delete stiTeam)
                  veid

          lift $ ScimUserTimesStore.delete uid
          lift $ BrigAccess.delete uid
          return ()

----------------------------------------------------------------------------
-- Utilities

-- | Calculate resource version (currently only for 'Scim.User's).
--
-- Spec: <https://tools.ietf.org/html/rfc7644#section-3.14>.
--
-- A version is an /opaque/ string that doesn't need to conform to any format. The only
-- guarantee we have to give is that different resources will have different versions.
--
-- Note: we use weak ETags for versions because we get no guarantees from @aeson@ that its
-- JSON rendering will remain stable between releases, and therefore we can't satisfy the
-- requirements of strong ETags ("same resources have the same version").
calculateVersion ::
  UserId ->
  Scim.User ST.SparTag ->
  Scim.ETag
calculateVersion uid usr = Scim.Weak (Text.pack (show h))
  where
    h :: Digest SHA256
    h = hashlazy (Aeson.encode (Scim.WithId uid usr))

-- |
-- Check that the UserRef is not taken.
--
-- ASSUMPTION: every scim user has a 'SAML.UserRef', and the `SAML.NameID` in it corresponds
-- to a single `externalId`.
assertExternalIdUnused :: Members '[BrigAccess, ScimExternalIdStore, SAMLUserStore] r => TeamId -> ST.ValidExternalId -> Scim.ScimHandler (Sem r) ()
assertExternalIdUnused tid veid = do
  assertExternalIdInAllowedValues
    [Nothing]
    "externalId is already taken"
    tid
    veid

-- |
-- Check that the UserRef is not taken any user other than the passed 'UserId'
-- (it is also acceptable if it is not taken by anybody).
--
-- ASSUMPTION: every scim user has a 'SAML.UserRef', and the `SAML.NameID` in it corresponds
-- to a single `externalId`.
assertExternalIdNotUsedElsewhere :: Members '[BrigAccess, ScimExternalIdStore, SAMLUserStore] r => TeamId -> ST.ValidExternalId -> UserId -> Scim.ScimHandler (Sem r) ()
assertExternalIdNotUsedElsewhere tid veid wireUserId = do
  assertExternalIdInAllowedValues
    [Nothing, Just wireUserId]
    "externalId already in use by another Wire user"
    tid
    veid

assertExternalIdInAllowedValues :: Members '[BrigAccess, ScimExternalIdStore, SAMLUserStore] r => [Maybe UserId] -> Text -> TeamId -> ST.ValidExternalId -> Scim.ScimHandler (Sem r) ()
assertExternalIdInAllowedValues allowedValues errmsg tid veid = do
  isGood <-
    lift $
      ST.runValidExternalIdBoth
        (\ma mb -> (&&) <$> ma <*> mb)
        ( \uref ->
            getUserIdByUref (Just tid) uref <&> \case
              (Spar.App.GetUserFound uid) -> Just uid `elem` allowedValues
              Spar.App.GetUserNotFound -> Nothing `elem` allowedValues
              Spar.App.GetUserNoTeam -> False -- this is never allowed (and also hopefully impossible)
              Spar.App.GetUserWrongTeam -> False -- this can happen, but it's violating all our assertions
        )
        (fmap (`elem` allowedValues) . getUserIdByScimExternalId tid)
        veid
  unless isGood $
    throwError Scim.conflict {Scim.detail = Just errmsg}

assertHandleUnused :: Member BrigAccess r => Handle -> Scim.ScimHandler (Sem r) ()
assertHandleUnused = assertHandleUnused' "userName is already taken"

assertHandleUnused' :: Member BrigAccess r => Text -> Handle -> Scim.ScimHandler (Sem r) ()
assertHandleUnused' msg hndl =
  lift (BrigAccess.checkHandleAvailable hndl) >>= \case
    True -> pure ()
    False -> throwError Scim.conflict {Scim.detail = Just msg}

assertHandleNotUsedElsewhere :: Member BrigAccess r => UserId -> Handle -> Scim.ScimHandler (Sem r) ()
assertHandleNotUsedElsewhere uid hndl = do
  musr <- lift $ Brig.getBrigUser Brig.WithPendingInvitations uid
  unless ((userHandle =<< musr) == Just hndl) $
    assertHandleUnused' "userName already in use by another wire user" hndl

-- | Helper function that translates a given brig user into a 'Scim.StoredUser', with some
-- effects like updating the 'ManagedBy' field in brig and storing creation and update time
-- stamps.
synthesizeStoredUser ::
  forall r.
  Members
    '[ Input Opts,
       Now,
       Logger (Msg -> Msg),
       BrigAccess,
       ScimUserTimesStore
     ]
    r =>
  UserAccount ->
  ST.ValidExternalId ->
  Scim.ScimHandler (Sem r) (Scim.StoredUser ST.SparTag)
synthesizeStoredUser usr veid =
  logScim
    ( logFunction "Spar.Scim.User.synthesizeStoredUser"
        . logUser (userId . accountUser $ usr)
        . maybe id logHandle (userHandle . accountUser $ usr)
        . maybe id logTeam (userTeam . accountUser $ usr)
        . maybe id logEmail (veidEmail veid)
    )
    logScimUserId
    $ do
      let uid = userId (accountUser usr)
          accStatus = accountStatus usr

      let readState :: Sem r (RI.RichInfo, Maybe (UTCTimeMillis, UTCTimeMillis), URIBS.URI)
          readState = do
            richInfo <- BrigAccess.getRichInfo uid
            accessTimes <- ScimUserTimesStore.read uid
            baseuri <- inputs $ derivedOptsScimBaseURI . derivedOpts
            pure (richInfo, accessTimes, baseuri)

      let writeState :: Maybe (UTCTimeMillis, UTCTimeMillis) -> ManagedBy -> RI.RichInfo -> Scim.StoredUser ST.SparTag -> Sem r ()
          writeState oldAccessTimes oldManagedBy oldRichInfo storedUser = do
            when (isNothing oldAccessTimes) $ do
              ScimUserTimesStore.write storedUser
            when (oldManagedBy /= ManagedByScim) $ do
              BrigAccess.setManagedBy uid ManagedByScim
            let newRichInfo = view ST.sueRichInfo . Scim.extra . Scim.value . Scim.thing $ storedUser
            when (oldRichInfo /= newRichInfo) $ do
              BrigAccess.setRichInfo uid newRichInfo

      (richInfo, accessTimes, baseuri) <- lift readState
      now <- toUTCTimeMillis <$> lift Now.get
      let (createdAt, lastUpdatedAt) = fromMaybe (now, now) accessTimes

      handle <- lift $ Brig.giveDefaultHandle (accountUser usr)

      storedUser <-
        synthesizeStoredUser'
          uid
          veid
          (userDisplayName (accountUser usr))
          handle
          richInfo
          accStatus
          createdAt
          lastUpdatedAt
          baseuri
      lift $ writeState accessTimes (userManagedBy (accountUser usr)) richInfo storedUser
      pure storedUser

synthesizeStoredUser' ::
  UserId ->
  ST.ValidExternalId ->
  Name ->
  Handle ->
  RI.RichInfo ->
  AccountStatus ->
  UTCTimeMillis ->
  UTCTimeMillis ->
  URIBS.URI ->
  MonadError Scim.ScimError m => m (Scim.StoredUser ST.SparTag)
synthesizeStoredUser' uid veid dname handle richInfo accStatus createdAt lastUpdatedAt baseuri = do
  let scimUser :: Scim.User ST.SparTag
      scimUser =
        synthesizeScimUser
          ST.ValidScimUser
            { ST._vsuExternalId = veid,
              ST._vsuHandle = handle {- 'Maybe' there is one in @usr@, but we want the type
                                        checker to make sure this exists, so we add it here
                                        redundantly, without the 'Maybe'. -},
              ST._vsuName = dname,
              ST._vsuRichInfo = richInfo,
              ST._vsuActive = ST.scimActiveFlagFromAccountStatus accStatus
            }

  pure $ toScimStoredUser' createdAt lastUpdatedAt baseuri uid (normalizeLikeStored scimUser)

synthesizeScimUser :: ST.ValidScimUser -> Scim.User ST.SparTag
synthesizeScimUser info =
  let Handle userName = info ^. ST.vsuHandle
   in (Scim.empty ST.userSchemas userName (ST.ScimUserExtra (info ^. ST.vsuRichInfo)))
        { Scim.externalId = Brig.renderValidExternalId $ info ^. ST.vsuExternalId,
          Scim.displayName = Just $ fromName (info ^. ST.vsuName),
          Scim.active = Just . Scim.ScimBool $ info ^. ST.vsuActive
        }

getUserById ::
  forall r.
  ( Member BrigAccess r,
    Member (Input Opts) r,
    Member (Logger (Msg -> Msg)) r,
    Member Now r,
    Member SAMLUserStore r,
    Member ScimExternalIdStore r,
    Member ScimUserTimesStore r
  ) =>
  Maybe IdP ->
  TeamId ->
  UserId ->
  MaybeT (Scim.ScimHandler (Sem r)) (Scim.StoredUser ST.SparTag)
getUserById midp stiTeam uid = do
  brigUser <- MaybeT . lift $ BrigAccess.getAccount Brig.WithPendingInvitations uid
  let mbveid =
        Brig.veidFromBrigUser
          (accountUser brigUser)
          ((^. SAML.idpMetadata . SAML.edIssuer) <$> midp)
  case mbveid of
    Right veid | userTeam (accountUser brigUser) == Just stiTeam -> lift $ do
      storedUser :: Scim.StoredUser ST.SparTag <- synthesizeStoredUser brigUser veid
      -- if we get a user from brig that hasn't been touched by scim yet, we call this
      -- function to move it under scim control.
      assertExternalIdNotUsedElsewhere stiTeam veid uid
      createValidScimUserSpar stiTeam uid storedUser veid
      lift $ do
        when (veidChanged (accountUser brigUser) veid) $ do
          BrigAccess.setVeid uid veid
        when (managedByChanged (accountUser brigUser)) $ do
          BrigAccess.setManagedBy uid ManagedByScim
      pure storedUser
    _ -> Applicative.empty
  where
    veidChanged :: BT.User -> ST.ValidExternalId -> Bool
    veidChanged usr veid = case BT.userIdentity usr of
      Nothing -> True
      Just (BT.FullIdentity _ _) -> True
      Just (BT.EmailIdentity _) -> True
      Just (BT.PhoneIdentity _) -> True
      Just (BT.SSOIdentity ssoid _ _) -> Brig.veidToUserSSOId veid /= ssoid

    managedByChanged :: BT.User -> Bool
    managedByChanged usr = userManagedBy usr /= ManagedByScim

scimFindUserByHandle ::
  forall r.
  ( Member BrigAccess r,
    Member (Input Opts) r,
    Member (Logger (Msg -> Msg)) r,
    Member Now r,
    Member SAMLUserStore r,
    Member ScimExternalIdStore r,
    Member ScimUserTimesStore r
  ) =>
  Maybe IdP ->
  TeamId ->
  Text ->
  MaybeT (Scim.ScimHandler (Sem r)) (Scim.StoredUser ST.SparTag)
scimFindUserByHandle mIdpConfig stiTeam hndl = do
  handle <- MaybeT . pure . parseHandle . Text.toLower $ hndl
  brigUser <- MaybeT . lift . BrigAccess.getByHandle $ handle
  getUserById mIdpConfig stiTeam . userId . accountUser $ brigUser

-- | Construct a 'ValidExternalid'.  If it an 'Email', find the non-SAML SCIM user in spar; if
-- that fails, find the user by email in brig.  If it is a 'UserRef', find the SAML user.
-- Return the result as a SCIM user.
--
-- Note the user won't get an entry in `spar.user`.  That will only happen on their first
-- successful authentication with their SAML credentials.
scimFindUserByEmail ::
  forall r.
  ( Member BrigAccess r,
    Member (Input Opts) r,
    Member (Logger (Msg -> Msg)) r,
    Member Now r,
    Member SAMLUserStore r,
    Member ScimExternalIdStore r,
    Member ScimUserTimesStore r
  ) =>
  Maybe IdP ->
  TeamId ->
  Text ->
  MaybeT (Scim.ScimHandler (Sem r)) (Scim.StoredUser ST.SparTag)
scimFindUserByEmail mIdpConfig stiTeam email = do
  -- Azure has been observed to search for externalIds that are not emails, even if the
  -- mapping is set up like it should be.  This is a problem: if there is no SAML IdP, 'mkValidExternalId'
  -- only supports external IDs that are emails.  This is a missing feature / bug in spar tracked in
  -- https://wearezeta.atlassian.net/browse/SQSERVICES-157; once it is fixed, we should go back to
  -- throwing errors returned by 'mkValidExternalId' here, but *not* throw an error if the externalId is
  -- a UUID, or any other text that is valid according to SCIM.
  veid <- MaybeT (either (const Nothing) Just <$> runExceptT (mkValidExternalId mIdpConfig (pure email)))
  uid <- MaybeT . lift $ ST.runValidExternalIdEither withUref withEmailOnly veid
  brigUser <- MaybeT . lift . BrigAccess.getAccount Brig.WithPendingInvitations $ uid
  getUserById mIdpConfig stiTeam . userId . accountUser $ brigUser
  where
    withUref :: SAML.UserRef -> Sem r (Maybe UserId)
    withUref uref = do
      SAMLUserStore.get uref >>= \case
        Nothing -> maybe (pure Nothing) withEmailOnly $ Brig.urefToEmail uref
        Just uid -> pure (Just uid)

    withEmailOnly :: BT.Email -> Sem r (Maybe UserId)
    withEmailOnly eml = maybe inbrig (pure . Just) =<< inspar
      where
        -- FUTUREWORK: we could also always lookup brig, that's simpler and possibly faster,
        -- and it never should be visible in spar, but not in brig.
        inspar, inbrig :: Sem r (Maybe UserId)
        inspar = ScimExternalIdStore.lookup stiTeam eml
        inbrig = userId . accountUser <$$> BrigAccess.getByEmail eml

logFilter :: Filter -> (Msg -> Msg)
logFilter (FilterAttrCompare attr op val) =
  Log.msg $ "filter:" <> rAttrPath attr <> " " <> rCompareOp op <> " " <> rCompValue val
  where
    rCompValue :: Scim.CompValue -> Text
    rCompValue = \case
      Scim.ValNull -> "null"
      Scim.ValBool True -> "true"
      Scim.ValBool False -> "false"
      Scim.ValNumber n -> cs $ Aeson.encodeToLazyText (Aeson.Number n)
      Scim.ValString s ->
        "sha256 "
          <> sha256String s
          <> (if isJust (UUID.fromText s) then " original is a UUID" else "")

{- TODO: might be useful later.
~~~~~~~~~~~~~~~~~~~~~~~~~

-- | Parse a name from a user profile into an SCIM name (Okta wants given
-- name and last name, so we break our names up to satisfy Okta).
--
-- TODO: use the same algorithm as Wire clients use.
toScimName :: Name -> Scim.Name
toScimName (Name name) =
  Scim.Name
    { Scim.formatted = Just name
    , Scim.givenName = Just first
    , Scim.familyName = if Text.null rest then Nothing else Just rest
    , Scim.middleName = Nothing
    , Scim.honorificPrefix = Nothing
    , Scim.honorificSuffix = Nothing
    }
  where
    (first, Text.drop 1 -> rest) = Text.breakOn " " name

-- | Convert from the Wire phone type to the SCIM phone type.
toScimPhone :: Phone -> Scim.Phone
toScimPhone (Phone phone) =
  Scim.Phone
    { Scim.typ = Nothing
    , Scim.value = Just phone
    }

-- | Convert from the Wire email type to the SCIM email type.
toScimEmail :: Email -> Scim.Email
toScimEmail (Email eLocal eDomain) =
  Scim.Email
    { Scim.typ = Nothing
    , Scim.value = Scim.EmailAddress2
        (unsafeEmailAddress (encodeUtf8 eLocal) (encodeUtf8 eDomain))
    , Scim.primary = Just True
    }

-}

-- Note [error handling]
-- ~~~~~~~~~~~~~~~~~
--
-- FUTUREWORK: There are two problems with error handling here:
--
-- 1. We want all errors originating from SCIM handlers to be thrown as SCIM
--    errors, not as Spar errors. Currently errors thrown from things like
--    'getTeamMembers' will look like Spar errors and won't be wrapped into
--    the 'ScimError' type. This might or might not be important, depending
--    on what is expected by apps that use the SCIM interface.
--
-- 2. We want generic error descriptions in response bodies, while still
--    logging nice error messages internally. The current messages might
--    be giving too many internal details away.

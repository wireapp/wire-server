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

-- For @instance UserDB Spar@

-- | Doing operations with users via SCIM.
--
-- Provides a 'Scim.Class.User.UserDB' instance.
module Spar.Scim.User
  ( -- * Internals (for testing)
    validateScimUser',
    synthesizeScimUser,
    toScimStoredUser',
    mkUserRef,
  )
where

import Brig.Types.Intra (AccountStatus)
import Brig.Types.User (ManagedBy (..), Name (..), User (..), ssoIdentity)
import qualified Brig.Types.User as BT
import Control.Exception (assert)
import Control.Lens ((^.))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Crypto.Hash (Digest, SHA256, hashlazy)
import qualified Data.Aeson as Aeson
import Data.Handle (Handle (Handle), parseHandle)
import Data.Id (Id (Id), UserId, idToText)
import Data.Json.Util (UTCTimeMillis, fromUTCTimeMillis, toUTCTimeMillis)
import Data.String.Conversions (cs)
import qualified Data.Text as Text
import qualified Data.UUID.V4 as UUID
import Imports
import Network.URI (URI, parseURI)
import qualified SAML2.WebSSO as SAML
import Spar.App (Spar, getUser, sparCtxOpts, validateEmailIfExists, wrapMonadClient)
import qualified Spar.Data as Data
import qualified Spar.Intra.Brig as Brig
import Spar.Scim.Auth ()
import qualified Spar.Scim.Types as ST
import Spar.Types (IdP, ScimTokenInfo (..), derivedOpts, derivedOptsScimBaseURI, richInfoLimit)
import qualified System.Logger.Class as Log
import qualified URI.ByteString as URIBS
import qualified Web.Scim.Class.User as Scim
import qualified Web.Scim.Filter as Scim
import qualified Web.Scim.Handler as Scim
import qualified Web.Scim.Schema.Common as Scim
import qualified Web.Scim.Schema.Error as Scim
import qualified Web.Scim.Schema.ListResponse as Scim
import qualified Web.Scim.Schema.Meta as Scim
import qualified Web.Scim.Schema.ResourceType as Scim
import qualified Web.Scim.Schema.User as Scim
import qualified Web.Scim.Schema.User as Scim.User (schemas)
import qualified Wire.API.User.RichInfo as RI

----------------------------------------------------------------------------
-- UserDB instance

instance Scim.UserDB ST.SparTag Spar where
  getUsers ::
    ScimTokenInfo ->
    Maybe Scim.Filter ->
    Scim.ScimHandler Spar (Scim.ListResponse (Scim.StoredUser ST.SparTag))
  getUsers _ Nothing = do
    throwError $ Scim.badRequest Scim.TooMany (Just "Please specify a filter when getting users.")
  getUsers ScimTokenInfo {stiTeam, stiIdP} (Just filter') = do
    mIdpConfig <- maybe (pure Nothing) (lift . wrapMonadClient . Data.getIdPConfig) stiIdP
    case filter' of
      Scim.FilterAttrCompare (Scim.AttrPath schema attrName _subAttr) Scim.OpEq (Scim.ValString val)
        | Scim.isUserSchema schema -> do
          x <- runMaybeT $ case attrName of
            "username" -> do
              handle <- MaybeT . pure . parseHandle . Text.toLower $ val
              brigUser <- MaybeT . lift . Brig.getBrigUserByHandle $ handle
              guard $ userTeam brigUser == Just stiTeam
              lift $ synthesizeStoredUser brigUser
            "externalid" -> do
              uref <- mkUserRef mIdpConfig (pure val)
              uid <- do
                -- TODO: without idp, we can't search for externalId now any more...  this is a bit stupid.
                MaybeT $ maybe (pure Nothing) (lift . wrapMonadClient . Data.getSAMLUser) uref
              brigUser <- MaybeT . lift . Brig.getBrigUser $ uid
              guard $ userTeam brigUser == Just stiTeam
              lift $ synthesizeStoredUser brigUser
            _ -> throwError (Scim.badRequest Scim.InvalidFilter (Just "Unsupported attribute"))
          pure $ Scim.fromList (toList x)
        | otherwise -> throwError $ Scim.badRequest Scim.InvalidFilter (Just "Unsupported schema")
      _ -> throwError $ Scim.badRequest Scim.InvalidFilter (Just "Operation not supported")

  getUser ::
    ScimTokenInfo ->
    UserId ->
    Scim.ScimHandler Spar (Scim.StoredUser ST.SparTag)
  getUser ScimTokenInfo {stiTeam} uid = do
    let notfound = Scim.notFound "User" (idToText uid)
    brigUser <- lift (Brig.getBrigUser uid) >>= maybe (throwError notfound) pure
    unless (userTeam brigUser == Just stiTeam) (throwError notfound)
    synthesizeStoredUser brigUser

  postUser ::
    ScimTokenInfo ->
    Scim.User ST.SparTag ->
    Scim.ScimHandler Spar (Scim.StoredUser ST.SparTag)
  postUser tokinfo user = createValidScimUser tokinfo =<< validateScimUser tokinfo user

  putUser ::
    ScimTokenInfo ->
    UserId ->
    Scim.User ST.SparTag ->
    Scim.ScimHandler Spar (Scim.StoredUser ST.SparTag)
  putUser tokinfo uid newScimUser =
    updateValidScimUser tokinfo uid =<< validateScimUser tokinfo newScimUser

  deleteUser :: ScimTokenInfo -> UserId -> Scim.ScimHandler Spar ()
  deleteUser = deleteScimUser

----------------------------------------------------------------------------
-- User creation and validation

-- | Validate a raw SCIM user record and extract data that we care about. See also:
-- 'ValidScimUser''.
validateScimUser ::
  forall m.
  (m ~ Scim.ScimHandler Spar) =>
  -- | Used to decide what IdP to assign the user to
  ScimTokenInfo ->
  Scim.User ST.SparTag ->
  m ST.ValidScimUser
validateScimUser tokinfo user = do
  mIdpConfig <- tokenInfoToIdP tokinfo
  richInfoLimit <- lift $ asks (richInfoLimit . sparCtxOpts)
  validateScimUser' mIdpConfig richInfoLimit user

tokenInfoToIdP :: ScimTokenInfo -> Scim.ScimHandler Spar (Maybe IdP)
tokenInfoToIdP ScimTokenInfo {stiIdP} = do
  maybe (pure Nothing) (lift . wrapMonadClient . Data.getIdPConfig) stiIdP

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
  -- | IdP that the resulting user will be assigned to
  Maybe IdP ->
  -- | Rich info limit
  Int ->
  Scim.User ST.SparTag ->
  m ST.ValidScimUser
validateScimUser' midp richInfoLimit user = do
  mUref :: Maybe SAML.UserRef <- mkUserRef midp (Scim.externalId user)
  handl <- validateHandle . Text.toLower . Scim.userName $ user
  -- FUTUREWORK: 'Scim.userName' should be case insensitive; then the toLower here would
  -- be a little less brittle.
  uname <- do
    let err = throwError . Scim.badRequest Scim.InvalidValue . Just . cs
    either err pure $ Brig.mkUserName (Scim.displayName user) mUref
  richInfo <- validateRichInfo (Scim.extra user ^. ST.sueRichInfo)
  let active = Scim.active user
  pure $ ST.ValidScimUser mUref handl uname richInfo (fromMaybe True active)
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
              )
          )
            { Scim.status = Scim.Status 413
            }
      pure richInfo

-- | Given an 'externalId' and an 'IdP', construct a 'SAML.UserRef'.
--
-- This is needed primarily in 'validateScimUser', but also in 'updateValidScimUser' to
-- recover the 'SAML.UserRef' of the scim user before the update from the database.
mkUserRef ::
  forall m.
  (MonadError Scim.ScimError m) =>
  Maybe IdP ->
  Maybe Text ->
  m (Maybe SAML.UserRef)
mkUserRef Nothing _ = pure Nothing
mkUserRef (Just idp) extid = case extid of
  Just subjectTxt -> do
    let issuer = idp ^. SAML.idpMetadata . SAML.edIssuer
    subject <- validateSubject subjectTxt
    pure . Just $ SAML.UserRef issuer subject
  Nothing ->
    throwError $
      Scim.badRequest
        Scim.InvalidValue
        (Just "externalId is required for SAML users")
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

-- | Creates a SCIM User.
--
-- User is created in Brig first, and then in SCIM and SAML.
--
-- Rationale: If brig user creation fails halfway, we don't have SCIM records that
-- point to inactive users. This stops people from logging in into inactive users.
--
-- We only allow SCIM users that authenticate via SAML. (This is by no means necessary,
-- though. It can be relaxed to allow creating users with password authentication if that is a
-- requirement.)
createValidScimUser ::
  forall m.
  (m ~ Scim.ScimHandler Spar) =>
  ScimTokenInfo ->
  ST.ValidScimUser ->
  m (Scim.StoredUser ST.SparTag)
createValidScimUser ScimTokenInfo {stiTeam} vsu@(ST.ValidScimUser uref handl mbName richInfo active) = do
  -- Generate a UserId will be used both for scim user in spar and for brig.
  buid <- Id <$> liftIO UUID.nextRandom
  -- ensure uniqueness constraints of all affected identifiers.
  -- if we crash now, retry POST will just work
  assertUserRefUnused `mapM_` uref
  assertHandleUnused handl buid
  -- if we crash now, retry POST will just work, or user gets told the handle
  -- is already in use and stops POSTing

  -- FUTUREWORK(arianvp): Get rid of manual lifting. Needs to be SCIM instances for ExceptT
  -- This is the pain and the price you pay for the horribleness called MTL
  storedUser <- lift . toScimStoredUser buid $ synthesizeScimUser vsu
  buid' <- lift $ Brig.createBrigUser uref buid stiTeam mbName ManagedByScim
  assert (buid == buid') $ pure ()
  -- If we crash now, we have an active user that cannot login. And can not
  -- be bound this will be a zombie user that needs to be manually cleaned
  -- up.  We should consider making setUserHandle part of createUser and
  -- making it transactional.  If the user redoes the POST A new standalone
  -- user will be created
  lift $ Brig.setBrigUserHandle buid handl
  -- If we crash now,  a POST retry will fail with 409 user already exists.
  -- Azure at some point will retry with GET /Users?filter=userName eq handle
  -- and then issue a PATCH containing the rich info and the externalId
  lift $ Brig.setBrigUserRichInfo buid richInfo
  -- If we crash now, same as above, but the PATCH will only contain externalId

  -- FUTUREWORK(arianvp): these two actions we probably want to make transactional
  lift . wrapMonadClient $ Data.writeScimUserTimes storedUser
  for_ uref $ (lift . wrapMonadClient . (`Data.insertSAMLUser` buid))

  (lift . validateEmailIfExists buid) `mapM_` uref

  -- TODO(fisx): suspension has yet another race condition: if we don't reach the following
  -- line, the user will be active.
  -- TODO(fisx): what happens with suspended users that have emails?  should emails still be
  -- validated?  will that work on suspended users?  (i think it won't, but i haven't
  -- checked.)
  lift $
    Brig.getStatus buid >>= \old -> do
      let new = ST.scimActiveFlagToAccountStatus old (Just active)
      when (new /= old) $ Brig.setStatus buid new
  pure storedUser

updateValidScimUser ::
  forall m.
  (m ~ Scim.ScimHandler Spar) =>
  ScimTokenInfo ->
  UserId ->
  ST.ValidScimUser ->
  m (Scim.StoredUser ST.SparTag)
updateValidScimUser tokinfo uid newScimUser = do
  -- TODO: how do we get this safe w.r.t. race conditions / crashes?

  -- construct old and new user values with metadata.
  oldScimStoredUser :: Scim.StoredUser ST.SparTag <-
    Scim.getUser tokinfo uid
  oldValidScimUser :: ST.ValidScimUser <-
    validateScimUser tokinfo . Scim.value . Scim.thing $ oldScimStoredUser
  (`assertUserRefNotUsedElsewhere` uid) `mapM_` (newScimUser ^. ST.vsuUserRef)
  assertHandleNotUsedElsewhere (newScimUser ^. ST.vsuHandle) uid
  if oldValidScimUser == newScimUser
    then pure oldScimStoredUser
    else do
      newScimStoredUser :: Scim.StoredUser ST.SparTag <-
        lift $ updScimStoredUser (synthesizeScimUser newScimUser) oldScimStoredUser
      -- update 'SAML.UserRef' on spar (also delete the old 'SAML.UserRef' if it exists and
      -- is different from the new one)
      let mNewUref = newScimUser ^. ST.vsuUserRef
      mOldUref <- do
        let extid :: Maybe Text
            extid = Scim.externalId . Scim.value . Scim.thing $ oldScimStoredUser
        idp <- tokenInfoToIdP tokinfo
        mkUserRef idp extid
      when (mOldUref /= mNewUref) $ do
        for_ mOldUref $ lift . wrapMonadClient . Data.deleteSAMLUser
        forM_ mNewUref $ \newUref -> lift . wrapMonadClient $ Data.insertSAMLUser newUref uid
      -- update 'SAML.UserRef' on brig
      forM_ mNewUref $ \newUref -> do
        bindok <- lift $ Brig.bindBrigUser uid newUref
        unless bindok $ do
          throwError $
            -- this can only happen if user is found in spar.scim_user, but missing on brig.
            -- (internal error?  race condition?)
            Scim.serverError "Failed to update SAML UserRef on brig."

      -- TODO: if the user has been suspended or unsuspended in brig since the last scim
      -- write, we'll find the wrong information here.
      -- [see also](https://github.com/zinfra/backend-issues/issues/1006)
      oldScimUser :: ST.ValidScimUser <-
        validateScimUser tokinfo . Scim.value . Scim.thing $ oldScimStoredUser
      -- the old scim user from our db is already validated, but this also recovers
      -- the extra details not stored in the DB that we need here.

      lift $ do
        when (newScimUser ^. ST.vsuName /= oldScimUser ^. ST.vsuName) $
          Brig.setBrigUserName uid (newScimUser ^. ST.vsuName)
        when (oldScimUser ^. ST.vsuHandle /= newScimUser ^. ST.vsuHandle) $
          Brig.setBrigUserHandle uid $
            newScimUser ^. ST.vsuHandle
        when (oldScimUser ^. ST.vsuRichInfo /= newScimUser ^. ST.vsuRichInfo) $
          Brig.setBrigUserRichInfo uid $
            newScimUser ^. ST.vsuRichInfo

      lift $
        Brig.getStatus uid >>= \old -> do
          let new = ST.scimActiveFlagToAccountStatus old (Just $ newScimUser ^. ST.vsuActive)
          when (new /= old) $ Brig.setStatus uid new

      -- store new user value to scim_user table (spar). (this must happen last, so in case
      -- of crash the client can repeat the operation and it won't be considered a noop.)
      lift . wrapMonadClient $ Data.writeScimUserTimes newScimStoredUser
      pure newScimStoredUser

toScimStoredUser ::
  UserId ->
  Scim.User ST.SparTag ->
  Spar (Scim.StoredUser ST.SparTag)
toScimStoredUser uid usr = do
  SAML.Time (toUTCTimeMillis -> now) <- SAML.getNow
  (createdAt, lastUpdatedAt) <- fromMaybe (now, now) <$> wrapMonadClient (Data.readScimUserTimes uid)
  baseuri <- asks $ derivedOptsScimBaseURI . derivedOpts . sparCtxOpts
  pure $ toScimStoredUser' createdAt lastUpdatedAt baseuri uid usr

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
  forall m.
  (SAML.HasNow m) =>
  Scim.User ST.SparTag ->
  Scim.StoredUser ST.SparTag ->
  m (Scim.StoredUser ST.SparTag)
updScimStoredUser usr storedusr = do
  SAML.Time (toUTCTimeMillis -> now) <- SAML.getNow
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
  ScimTokenInfo -> UserId -> Scim.ScimHandler Spar ()
deleteScimUser ScimTokenInfo {stiTeam} uid = do
  mbBrigUser <- lift (Brig.getBrigUser uid)
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
      ssoId <-
        maybe
          (logThenServerError $ "no userSSOId for user " <> cs (idToText uid))
          pure
          $ BT.userSSOId brigUser
      uref <- either logThenServerError pure $ Brig.fromUserSSOId ssoId
      lift . wrapMonadClient $ Data.deleteSAMLUser uref
      lift . wrapMonadClient $ Data.deleteScimUserTimes uid
      lift $ Brig.deleteBrigUser uid
      return ()
  where
    logThenServerError :: String -> Scim.ScimHandler Spar b
    logThenServerError err = do
      lift $ Log.err (Log.msg err)
      throwError $ Scim.serverError "Server Error"

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
assertUserRefUnused :: SAML.UserRef -> Scim.ScimHandler Spar ()
assertUserRefUnused userRef = do
  mExistingUserId <- lift $ getUser userRef
  unless (isNothing mExistingUserId) $
    throwError Scim.conflict {Scim.detail = Just "externalId is already taken"}

-- |
-- Check that the UserRef is not taken any user other than the passed 'UserId'
-- (it is also acceptable if it is not taken by anybody).
--
-- ASSUMPTION: every scim user has a 'SAML.UserRef', and the `SAML.NameID` in it corresponds
-- to a single `externalId`.
assertUserRefNotUsedElsewhere :: SAML.UserRef -> UserId -> Scim.ScimHandler Spar ()
assertUserRefNotUsedElsewhere userRef wireUserId = do
  mExistingUserId <- lift $ getUser userRef
  unless (mExistingUserId `elem` [Nothing, Just wireUserId]) $ do
    throwError Scim.conflict {Scim.detail = Just "externalId does not match UserId"}

assertHandleUnused :: Handle -> UserId -> Scim.ScimHandler Spar ()
assertHandleUnused = assertHandleUnused' "userName is already taken"

assertHandleUnused' :: Text -> Handle -> UserId -> Scim.ScimHandler Spar ()
assertHandleUnused' msg hndl uid =
  lift (Brig.checkHandleAvailable hndl uid) >>= \case
    True -> pure ()
    False -> throwError Scim.conflict {Scim.detail = Just msg}

assertHandleNotUsedElsewhere :: Handle -> UserId -> Scim.ScimHandler Spar ()
assertHandleNotUsedElsewhere hndl uid = do
  musr <- lift $ Brig.getBrigUser uid
  unless ((userHandle =<< musr) == Just hndl) $
    assertHandleUnused' "userName does not match UserId" hndl uid

-- | Helper function that translates a given brig user into a 'Scim.StoredUser', with some
-- effects like updating the 'ManagedBy' field in brig and storing creation and update time
-- stamps.
synthesizeStoredUser :: BT.User -> Scim.ScimHandler Spar (Scim.StoredUser ST.SparTag)
synthesizeStoredUser usr = do
  let readState :: Spar (RI.RichInfo, AccountStatus, Maybe (UTCTimeMillis, UTCTimeMillis), URIBS.URI)
      readState = do
        richInfo <- Brig.getBrigUserRichInfo (BT.userId usr)
        accStatus <- Brig.getStatus (BT.userId usr)
        accessTimes <- wrapMonadClient (Data.readScimUserTimes (BT.userId usr))
        baseuri <- asks $ derivedOptsScimBaseURI . derivedOpts . sparCtxOpts
        pure (richInfo, accStatus, accessTimes, baseuri)

  let writeState :: UserId -> Maybe (UTCTimeMillis, UTCTimeMillis) -> ManagedBy -> Scim.StoredUser ST.SparTag -> Spar ()
      writeState uid accessTimes managedBy storedUser = do
        when (isNothing accessTimes) $ do
          wrapMonadClient $ Data.writeScimUserTimes storedUser
        when (managedBy /= ManagedByScim) $ do
          Brig.setBrigUserManagedBy uid ManagedByScim

  (richInfo, accStatus, accessTimes, baseuri) <- lift readState
  SAML.Time (toUTCTimeMillis -> now) <- lift SAML.getNow
  let (createdAt, lastUpdatedAt) = fromMaybe (now, now) accessTimes
  handle <- lift $ Brig.giveDefaultHandle usr
  let uref = either (const Nothing) Just . Brig.fromUserSSOId
  storedUser <-
    synthesizeStoredUser'
      (userId usr)
      (userIdentity >=> ssoIdentity >=> uref $ usr)
      (userDisplayName usr)
      handle
      richInfo
      accStatus
      createdAt
      lastUpdatedAt
      baseuri
  lift $ writeState (BT.userId usr) accessTimes (BT.userManagedBy usr) storedUser
  pure storedUser

synthesizeStoredUser' ::
  UserId ->
  Maybe SAML.UserRef ->
  Name ->
  Handle ->
  RI.RichInfo ->
  AccountStatus ->
  UTCTimeMillis ->
  UTCTimeMillis ->
  URIBS.URI ->
  MonadError Scim.ScimError m => m (Scim.StoredUser ST.SparTag)
synthesizeStoredUser' uid ssoid dname handle richInfo accStatus createdAt lastUpdatedAt baseuri = do
  let scimUser :: Scim.User ST.SparTag
      scimUser =
        synthesizeScimUser
          ST.ValidScimUser
            { ST._vsuUserRef = ssoid,
              ST._vsuHandle = handle, -- 'Maybe' there is one in @usr@, but we want to type checker to make sure this exists.
              ST._vsuName = dname,
              ST._vsuRichInfo = richInfo,
              ST._vsuActive = ST.scimActiveFlagFromAccountStatus accStatus
            }

  pure $ toScimStoredUser' createdAt lastUpdatedAt baseuri uid scimUser

synthesizeScimUser :: ST.ValidScimUser -> Scim.User ST.SparTag
synthesizeScimUser info =
  let Handle userName = info ^. ST.vsuHandle
      displayName = fromName (info ^. ST.vsuName)
      toExternalId' :: Maybe SAML.UserRef -> Maybe Text
      toExternalId' = (>>= either (const Nothing) Just . Brig.toExternalId . Brig.toUserSSOId)
   in (Scim.empty ST.userSchemas userName (ST.ScimUserExtra (info ^. ST.vsuRichInfo)))
        { Scim.externalId = toExternalId' $ info ^. ST.vsuUserRef,
          Scim.displayName = Just displayName,
          Scim.active = Just $ info ^. ST.vsuActive
        }

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

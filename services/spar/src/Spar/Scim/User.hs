{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
-- Exported functions are used in tests.
module Spar.Scim.User
  ( validateScimUser',
    synthesizeScimUser,
    toScimStoredUser,
    mkValidScimId,
    scimFindUserByExternalId,
    deleteScimUser,
  )
where

import qualified Control.Applicative as Applicative (empty)
import Control.Lens hiding (op)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Except (mapExceptT)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Crypto.Hash (Digest, SHA256, hashlazy)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import Data.ByteString (toStrict)
import Data.ByteString.Conversion (fromByteString, toByteString, toByteString')
import qualified Data.ByteString.UTF8 as UTF8
import Data.Handle (Handle, fromHandle, parseHandle)
import Data.Id (Id (..), TeamId, UserId, idToText)
import Data.Json.Util (UTCTimeMillis, fromUTCTimeMillis, toUTCTimeMillis)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error
import qualified Data.Text.Lazy as LText
import Data.These
import Data.These.Combinators
import qualified Data.UUID as UUID
import Imports
import Network.URI (URI, parseURI)
import Polysemy
import Polysemy.Error (Error, runError, throw)
import Polysemy.Input
import qualified SAML2.WebSSO as SAML
import Spar.App (getUserByUrefUnsafe, getUserByUrefViaOldIssuerUnsafe, getUserIdByScimExternalId)
import qualified Spar.App
import Spar.Intra.BrigApp as Intra
import qualified Spar.Intra.BrigApp as Brig
import Spar.Scim.Auth ()
import Spar.Scim.Types
import qualified Spar.Scim.Types as ST
import Spar.Sem.BrigAccess (BrigAccess, getAccount)
import qualified Spar.Sem.BrigAccess as BrigAccess
import Spar.Sem.GalleyAccess as GalleyAccess
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
import qualified Web.Scim.Schema.User.Email as Scim.Email
import qualified Wire.API.Team.Member as Member
import Wire.API.Team.Role
import Wire.API.User
import Wire.API.User.IdentityProvider (IdP)
import qualified Wire.API.User.RichInfo as RI
import Wire.API.User.Scim (ScimTokenInfo (..), ValidScimId (..))
import qualified Wire.API.User.Scim as ST
import Wire.Sem.Logger (Logger)
import qualified Wire.Sem.Logger as Logger
import Wire.Sem.Now (Now)
import qualified Wire.Sem.Now as Now
import Wire.Sem.Random (Random)
import qualified Wire.Sem.Random as Random
import Wire.ServerOptions.Spar

----------------------------------------------------------------------------
-- UserDB instance

instance
  ( Member (Logger (Msg -> Msg)) r,
    Member (Logger String) r,
    Member Random r,
    Member (Input Opts) r,
    Member Now r,
    Member GalleyAccess r,
    Member BrigAccess r,
    Member ScimExternalIdStore r,
    Member ScimUserTimesStore r,
    Member IdPConfigStore r,
    Member SAMLUserStore r
  ) =>
  Scim.UserDB ST.SparTag (Sem r)
  where
  getUsers ::
    ScimTokenInfo ->
    Maybe Scim.Filter ->
    Scim.ScimHandler (Sem r) (Scim.ListResponse (Scim.StoredUser ST.SparTag))
  getUsers _ Nothing =
    throwError $ Scim.badRequest Scim.TooMany (Just "Please specify a filter when getting users.")
  getUsers tokeninfo@ScimTokenInfo {stiTeam, stiIdP} (Just filter') =
    logScim
      ( logFunction "Spar.Scim.User.getUsers"
          . logTokenInfo tokeninfo
          . logFilter filter'
      )
      logScimUserIds
      $ do
        mIdpConfig <- mapM (lift . IdPConfigStore.getConfig) stiIdP
        case filter' of
          Scim.FilterAttrCompare (Scim.AttrPath schema attrName _subAttr) Scim.OpEq (Scim.ValString val)
            | Scim.isUserSchema schema -> do
                x <- runMaybeT $ case attrName of
                  "username" -> scimFindUserByHandle mIdpConfig stiTeam val
                  "externalid" -> scimFindUserByExternalId mIdpConfig stiTeam val
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
        mIdpConfig <- mapM (lift . IdPConfigStore.getConfig) stiIdP
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
  forall r.
  ( Member SAMLUserStore r,
    Member BrigAccess r,
    Member (Input Opts) r,
    Member IdPConfigStore r
  ) =>
  Text ->
  -- | Used to decide what IdP to assign the user to
  ScimTokenInfo ->
  Scim.User ST.SparTag ->
  Scim.ScimHandler (Sem r) ST.ValidScimUser
validateScimUser errloc tokinfo user = do
  mIdpConfig <- tokenInfoToIdP tokinfo
  richInfoLimit <- lift $ inputs richInfoLimit
  eitherUser <- lift $ runError $ validateScimUser' errloc mIdpConfig richInfoLimit user
  case eitherUser of
    Left err -> throwError err
    Right validatedUser -> pure validatedUser

tokenInfoToIdP :: (Member IdPConfigStore r) => ScimTokenInfo -> Scim.ScimHandler (Sem r) (Maybe IdP)
tokenInfoToIdP ScimTokenInfo {stiIdP} =
  mapM (lift . IdPConfigStore.getConfig) stiIdP

-- | Validate a handle (@userName@).
validateHandle :: (Member (Error Scim.ScimError) r) => Text -> Sem r Handle
validateHandle txt = case parseHandle txt of
  Just h -> pure h
  Nothing ->
    throw $
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
-- __Email verification:__ we'd like to ensure that only verified emails numbers end up
-- in our database, and implementing verification requires design decisions
-- that we haven't made yet. We store them in our SCIM blobs, but don't syncronize them with
-- Brig. See <https://github.com/wireapp/wire-server/pull/559#discussion_r247466760>.
-- FUTUREWORK(elland): verify with fisx if this still applies.
validateScimUser' ::
  forall r.
  ( Member (Error Scim.ScimError) r,
    Member BrigAccess r,
    Member SAMLUserStore r
  ) =>
  -- | Error location (call site, for debugging)
  Text ->
  -- | IdP that the resulting user will be assigned to
  Maybe IdP ->
  -- | Rich info limit
  Int ->
  Scim.User ST.SparTag ->
  Sem r ST.ValidScimUser
validateScimUser' errloc midp richInfoLimit user = do
  unless (isNothing $ Scim.password user) $ throw $ badRequest "Setting user passwords is not supported for security reasons."
  veid <- mkValidScimId midp (Scim.externalId user) (Scim.Email.scimEmailsToEmailAddress $ Scim.emails user)
  handl <- validateHandle . Text.toLower . Scim.userName $ user
  -- FUTUREWORK: 'Scim.userName' should be case insensitive; then the toLower here would
  -- be a little less brittle.
  uname <- do
    let err msg =
          throw
            . Scim.badRequest Scim.InvalidValue
            . Just
            $ Text.pack msg
              <> " ("
              <> errloc
              <> ")"
    either err pure $ Brig.mkUserName (Scim.displayName user) (ST.validScimIdAuthInfo veid)
  richInfo <- validateRichInfo (Scim.extra user ^. ST.sueRichInfo)
  let active = Scim.active user
  lang <- maybe (throw $ badRequest "Could not parse language. Expected format is ISO 639-1.") pure $ mapM parseLanguage $ Scim.preferredLanguage user
  mRole <- validateRole user

  pure $ ST.ValidScimUser veid handl uname (maybeToList (justHere veid.validScimIdAuthInfo)) richInfo (maybe True Scim.unScimBool active) (flip Locale Nothing <$> lang) mRole
  where
    validRoleNames :: Text
    validRoleNames =
      Text.pack $
        intercalate ", " $
          map
            (UTF8.toString . toByteString')
            [minBound @Role .. maxBound]

    validateRole =
      Scim.roles <&> \case
        [] -> pure Nothing
        [roleName] ->
          maybe
            (throw $ badRequest $ "The role '" <> roleName <> "' is not valid. Valid roles are " <> validRoleNames <> ".")
            (pure . Just)
            (fromByteString $ Text.encodeUtf8 roleName)
        (_ : _ : _) -> throw $ badRequest "A user cannot have more than one role."

    badRequest :: Text -> Scim.ScimError
    badRequest msg =
      Scim.badRequest
        Scim.InvalidValue
        (Just $ msg <> " (" <> errloc <> ")")

    -- Validate rich info (@richInfo@). It must not exceed the rich info limit.
    validateRichInfo :: RI.RichInfo -> Sem r RI.RichInfo
    validateRichInfo richInfo = do
      let sze = RI.richInfoSize richInfo
      when (sze > richInfoLimit) $
        throw $
          ( Scim.badRequest
              Scim.InvalidValue
              ( Just
                  . Text.pack
                  $ show [RI.richInfoMapURN @Text, RI.richInfoAssocListURN @Text]
                    <> " together exceed the size limit: max "
                    <> show richInfoLimit
                    <> " characters, but got "
                    <> show sze
                    <> " ("
                    <> Text.unpack errloc
                    <> ")"
              )
          )
            { Scim.status = Scim.Status 413
            }
      pure richInfo

-- | Given an 'externalId' and an 'IdP', construct a 'ST.ValidScimId'.
--
-- This is needed primarily in 'validateScimUser', but also in 'updateValidScimUser' to
-- recover the 'SAML.UserRef' of the scim user before the update from the database.
mkValidScimId ::
  forall r.
  ( Member BrigAccess r,
    Member SAMLUserStore r,
    Member (Error Scim.ScimError) r
  ) =>
  Maybe IdP ->
  Maybe Text ->
  Maybe EmailAddress ->
  Sem r ST.ValidScimId
mkValidScimId _ Nothing _ =
  throw $
    Scim.badRequest
      Scim.InvalidValue
      (Just "externalId is required")
mkValidScimId Nothing (Just extid) (Just email) = do
  pure $ ST.ValidScimId extid (This email)
mkValidScimId Nothing (Just extid) Nothing = do
  let err =
        Scim.badRequest
          Scim.InvalidValue
          (Just "externalId must be a valid email address or (if there is a SAML IdP) a valid SAML NameID")
  maybe (throw err) (pure . ST.ValidScimId extid . This) $ emailAddressText extid
mkValidScimId (Just idp) (Just extid) mEmail = do
  let issuer = idp ^. SAML.idpMetadata . SAML.edIssuer
  subject <- validateSubject extid
  let uref = SAML.UserRef issuer subject
  -- The index for URef -> user id depends on name of the issuer, which can be
  -- updated by the team admin. This update is not applied immediately to all
  -- users. So, we have to find which URef is actaully pointing to the user.
  indexedUref <-
    getUserByUrefUnsafe uref >>= \case
      Just _ -> pure uref
      Nothing ->
        getUserByUrefViaOldIssuerUnsafe idp uref >>= \case
          Just (olduref, _) -> pure olduref
          Nothing ->
            -- The entry in spar.user_v2 does not exist yet during user
            -- creation. So we just assume that it will exist momentarily.
            pure uref
  pure . ST.ValidScimId extid $ case (mEmail, emailAddressText extid) of
    (Just email, _) -> These email indexedUref
    (Nothing, Just email) -> These email indexedUref
    (Nothing, Nothing) -> That indexedUref
  where
    -- Validate a subject ID (@externalId@).
    validateSubject :: Text -> Sem r SAML.NameID
    validateSubject txt = do
      unameId :: SAML.UnqualifiedNameID <- do
        let eEmail = SAML.mkUNameIDEmail txt
            unspec = SAML.mkUNameIDUnspecified txt
        pure . fromRight unspec $ eEmail
      case SAML.mkNameID unameId Nothing Nothing Nothing of
        Right nameId -> pure nameId
        Left err ->
          throw $
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
                Nothing ->
                  Text.decodeUtf8With lenientDecode . toStrict . Aeson.encode $ e
        Logger.warn $ context . Log.msg errorMsg
        pure (Left e)
      Right x -> do
        Logger.info $ context . postcontext x . Log.msg @Text "call without exception"
        pure (Right x)

logEmail :: EmailAddress -> (Msg -> Msg)
logEmail email =
  Log.field "email_sha256" (sha256String . Text.pack . show $ email)

logVSU :: ST.ValidScimUser -> (Msg -> Msg)
logVSU (ST.ValidScimUser veid handl _name _emails _richInfo _active _lang _role) =
  -- FUTUREWORK(elland): Take SCIM emails field into account.
  maybe id logEmail (justHere $ ST.validScimIdAuthInfo veid)
    . logHandle handl

logTokenInfo :: ScimTokenInfo -> (Msg -> Msg)
logTokenInfo ScimTokenInfo {stiTeam} = logTeam stiTeam

logScimUserId :: Scim.StoredUser ST.SparTag -> (Msg -> Msg)
logScimUserId = logUser . Scim.id . Scim.thing

logScimUserIds :: Scim.ListResponse (Scim.StoredUser ST.SparTag) -> (Msg -> Msg)
logScimUserIds lresp = foldl' (.) id (logScimUserId <$> Scim.resources lresp)

vsUserEmail :: ST.ValidScimUser -> Maybe EmailAddress
vsUserEmail usr = justHere $ ST.validScimIdAuthInfo usr.externalId

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
  ( Member Random r,
    Member Now r,
    Member (Input Opts) r,
    Member (Logger (Msg -> Msg)) r,
    Member (Logger String) r,
    Member GalleyAccess r,
    Member BrigAccess r,
    Member ScimExternalIdStore r,
    Member ScimUserTimesStore r,
    Member SAMLUserStore r,
    Member IdPConfigStore r
  ) =>
  ScimTokenInfo ->
  ST.ValidScimUser ->
  m (Scim.StoredUser ST.SparTag)
createValidScimUser tokeninfo@ScimTokenInfo {stiTeam} vsu@(ST.ValidScimUser {..}) =
  logScim
    ( logFunction "Spar.Scim.User.createValidScimUser"
        . logVSU vsu
        . logTokenInfo tokeninfo
    )
    logScimUserId
    $ do
      lift (ScimExternalIdStore.lookupStatus stiTeam externalId) >>= \case
        Just (buid, ScimUserCreated) ->
          -- If the user has been created, but can't be found in brig anymore,
          -- the invitation has timed out and the user has been deleted on brig's side.
          -- If this is the case we can safely create the user again, AFTER THE
          -- HALF-CREATED ACCOUNT HAS BEEN GARBAGE-COLLECTED.
          -- Otherwise we return a conflict error.
          lift (BrigAccess.getStatusMaybe buid) >>= \case
            Just Active -> throwError (externalIdTakenError ("user with status Active exists: " <> Text.pack (show (externalId, buid))))
            Just Suspended -> throwError (externalIdTakenError ("user with status Suspended exists" <> Text.pack (show (externalId, buid))))
            Just Ephemeral -> throwError (externalIdTakenError ("user with status Ephemeral exists" <> Text.pack (show (externalId, buid))))
            Just PendingInvitation -> throwError (externalIdTakenError ("user with status PendingInvitation exists" <> Text.pack (show (externalId, buid))))
            Just Deleted -> incompleteUserCreationCleanUp buid
            Nothing -> incompleteUserCreationCleanUp buid
        Just (buid, ScimUserCreating) ->
          incompleteUserCreationCleanUp buid
        Nothing -> pure ()

      -- ensure uniqueness constraints of all affected identifiers.
      -- {if we crash now, retry POST will just work}
      assertExternalIdUnused stiTeam externalId
      assertHandleUnused handle
      -- {if we crash now, retry POST will just work, or user gets told the handle
      -- is already in use and stops POSTing}
      buid <- lift $ Id <$> Random.uuid

      lift $ ScimExternalIdStore.insertStatus stiTeam externalId buid ScimUserCreating

      -- Generate a UserId will be used both for scim user in spar and for brig.
      lift $ do
        let doUref uref = do
              -- FUTUREWORK: outsource this and some other fragments from
              -- `createValidScimUser` into a function `createValidScimUserBrig` similar
              -- to `createValidScimUserSpar`?
              void $ BrigAccess.createSAML uref buid stiTeam name ManagedByScim (Just handle) (Just richInfo) locale (fromMaybe defaultRole role)
            doEmail email = do
              void $ BrigAccess.createNoSAML externalId.validScimIdExternal email buid stiTeam name locale (fromMaybe defaultRole role)
              BrigAccess.setHandle buid handle -- FUTUREWORK: possibly do the same one req as we do for saml?
        these doEmail doUref (\_ uref -> doUref uref) (validScimIdAuthInfo externalId)
        Logger.debug ("createValidScimUser: brig says " <> show buid)

        BrigAccess.setRichInfo buid richInfo

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
        synthesizeStoredUser acc externalId
      lift $ Logger.debug ("createValidScimUser: spar says " <> show storedUser)

      -- {(arianvp): these two actions we probably want to make transactional.}
      createValidScimUserSpar stiTeam buid storedUser externalId

      -- If applicable, trigger email validation procedure on brig.
      -- FUTUREWORK: validate fallback emails?
      lift $ Spar.App.validateEmail (Just stiTeam) buid `mapM_` vsUserEmail vsu

      -- TODO: suspension via scim is brittle, and may leave active users behind: if we don't
      -- reach the following line due to a crash, the user will be active.
      lift $ do
        old <- BrigAccess.getStatus buid
        let new = ST.scimActiveFlagToAccountStatus old (Scim.unScimBool <$> active')
            active' = Scim.active . Scim.value . Scim.thing $ storedUser
        when (new /= old) $ BrigAccess.setStatus buid new

      lift $ ScimExternalIdStore.insertStatus stiTeam externalId buid ScimUserCreated
      pure storedUser
  where
    incompleteUserCreationCleanUp :: UserId -> Scim.ScimHandler (Sem r) ()
    incompleteUserCreationCleanUp buid = do
      -- something went wrong while storing the user in brig
      -- we can try clean up now, but if brig is down, we can't do much
      -- and just fail with a 5xx.
      lift $ Logger.warn $ Log.msg @Text "An earlier attempt of creating a user with this external ID has failed and left some inconsistent data. Attempting to clean up."
      deleteScimUser tokeninfo buid
      lift $ Logger.info $ Log.msg @Text "Clean up complete."

    externalIdTakenError :: Text -> Scim.ScimError
    externalIdTakenError msg = Scim.conflict {Scim.detail = Just ("ExternalId is already taken: " <> msg)}

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
  ST.ValidScimId ->
  m ()
createValidScimUserSpar stiTeam uid storedUser veid = lift $ do
  ScimUserTimesStore.write storedUser
  ScimExternalIdStore.insert stiTeam veid.validScimIdExternal uid
  for_ (justThere veid.validScimIdAuthInfo) (`SAMLUserStore.insert` uid)

-- TODO(arianvp): how do we get this safe w.r.t. race conditions / crashes?
updateValidScimUser ::
  forall m r.
  ( Member Random r,
    Member (Input Opts) r,
    Member (Logger (Msg -> Msg)) r,
    Member (Logger String) r,
    Member Now r,
    Member GalleyAccess r,
    Member BrigAccess r,
    Member ScimExternalIdStore r,
    Member ScimUserTimesStore r,
    Member IdPConfigStore r,
    Member SAMLUserStore r
  ) =>
  (m ~ Scim.ScimHandler (Sem r)) =>
  ScimTokenInfo ->
  UserId ->
  ST.ValidScimUser ->
  m (Scim.StoredUser ST.SparTag)
updateValidScimUser tokinfo@ScimTokenInfo {stiTeam} uid nvsu =
  logScim
    ( logFunction "Spar.Scim.User.updateValidScimUser"
        . logVSU nvsu
        . logUser uid
        . logTokenInfo tokinfo
    )
    logScimUserId
    $ do
      oldScimStoredUser :: Scim.StoredUser ST.SparTag <-
        Scim.getUser tokinfo uid
      oldValidScimUser :: ST.ValidScimUser <-
        validateScimUser "recover-old-value" tokinfo . Scim.value . Scim.thing $ oldScimStoredUser

      -- if the locale of the new valid SCIM user is not set,
      -- we set it to default value from brig
      defLocale <- lift BrigAccess.getDefaultUserLocale
      let newValidScimUser = nvsu {ST.locale = ST.locale nvsu <|> Just defLocale}

      -- assertions about new valid scim user that cannot be checked in 'validateScimUser' because
      -- they differ from the ones in 'createValidScimUser'.
      assertExternalIdNotUsedElsewhere stiTeam (newValidScimUser.externalId) uid
      assertHandleNotUsedElsewhere uid (newValidScimUser.handle)

      if oldValidScimUser == newValidScimUser
        then pure oldScimStoredUser
        else do
          lift $ do
            newScimStoredUser :: Scim.StoredUser ST.SparTag <-
              updScimStoredUser (synthesizeScimUser newValidScimUser) oldScimStoredUser

            when (oldValidScimUser.externalId /= newValidScimUser.externalId) $
              updateVsuUref stiTeam uid (oldValidScimUser.externalId) (newValidScimUser.externalId)

            when (newValidScimUser.name /= oldValidScimUser.name) $
              BrigAccess.setName uid (newValidScimUser.name)

            when (oldValidScimUser.handle /= newValidScimUser.handle) $
              BrigAccess.setHandle uid (newValidScimUser.handle)

            when (oldValidScimUser.richInfo /= newValidScimUser.richInfo) $
              BrigAccess.setRichInfo uid (newValidScimUser.richInfo)

            when (oldValidScimUser.locale /= newValidScimUser.locale) $ do
              BrigAccess.setLocale uid (newValidScimUser.locale)

            forM_ (newValidScimUser.role) $ \newRole -> do
              when (oldValidScimUser.role /= Just newRole) $ do
                GalleyAccess.updateTeamMember uid stiTeam newRole

            BrigAccess.getStatusMaybe uid >>= \case
              Nothing -> pure ()
              Just old -> do
                let new = ST.scimActiveFlagToAccountStatus old (Just $ newValidScimUser.active)
                when (new /= old) $ BrigAccess.setStatus uid new

            ScimUserTimesStore.write newScimStoredUser
          Scim.getUser tokinfo uid

updateVsuUref ::
  ( Member GalleyAccess r,
    Member BrigAccess r,
    Member ScimExternalIdStore r,
    Member SAMLUserStore r
  ) =>
  TeamId ->
  UserId ->
  ST.ValidScimId ->
  ST.ValidScimId ->
  Sem r ()
updateVsuUref team uid old new = do
  case (justHere $ ST.validScimIdAuthInfo old, justHere $ ST.validScimIdAuthInfo new) of
    (mo, mn@(Just email)) | mo /= mn -> Spar.App.validateEmail (Just team) uid email
    _ -> pure ()

  ScimExternalIdStore.delete team old.validScimIdExternal
  for_ (justThere old.validScimIdAuthInfo) (SAMLUserStore.delete uid)

  ScimExternalIdStore.insert team new.validScimIdExternal uid
  for_ (justThere new.validScimIdAuthInfo) (`SAMLUserStore.insert` uid)

  BrigAccess.setSSOId uid $ veidToUserSSOId new

toScimStoredUser ::
  (HasCallStack) =>
  UTCTimeMillis ->
  UTCTimeMillis ->
  URIBS.URI ->
  UserId ->
  Scim.User ST.SparTag ->
  Scim.StoredUser ST.SparTag
toScimStoredUser createdAt lastChangedAt baseuri uid usr =
  Scim.WithMeta meta $
    Scim.WithId uid $
      usr {Scim.User.schemas = ST.userSchemas}
  where
    mkLocation :: String -> URI
    mkLocation pathSuffix = convURI $ baseuri SAML.=/ Text.pack pathSuffix
      where
        convURI uri = fromMaybe err . parseURI . UTF8.toString . URIBS.serializeURIRef' $ uri
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
          Scim.location = Scim.URI . mkLocation $ "/Users/" <> Text.unpack (idToText uid)
        }

updScimStoredUser ::
  forall r.
  (Member Now r) =>
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
  ( Member (Logger (Msg -> Msg)) r,
    Member BrigAccess r,
    Member ScimExternalIdStore r,
    Member ScimUserTimesStore r,
    Member SAMLUserStore r,
    Member IdPConfigStore r
  ) =>
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
    do
      -- `getBrigUser` does not include deleted users. This is fine: these
      -- ("tombstones") would not have the needed values (`userIdentity =
      -- Nothing`) to delete a user in spar. I.e. `SAML.UserRef` and `Email`
      -- cannot be figured out when a `User` has status `Deleted`.
      mbAccount <- lift $ BrigAccess.getAccount WithPendingInvitations uid
      case mbAccount of
        Nothing ->
          -- Ensure there's no left-over of this user in brig. This is safe
          -- because the user has either been deleted (tombstone) or does not
          -- exist. Asserting the correct team id here is not needed (and would
          -- be hard as the check relies on the data of `mbBrigUser`): The worst
          -- thing that could happen is that foreign users cleanup partially
          -- deleted users.
          void . lift $ BrigAccess.deleteUser uid
        Just brigUser -> do
          if userTeam brigUser == Just stiTeam
            then do
              -- This deletion needs data from the non-deleted User in brig. So,
              -- execute it first, then delete the user in brig. Unfortunately, this
              -- dependency prevents us from cleaning up the spar fragments of users
              -- that have been deleted in brig.  Deleting scim-managed users in brig
              -- (via the TM app) is blocked, though, so there is no legal way to enter
              -- that situation.
              deleteUserInSpar brigUser
              void . lift $ BrigAccess.deleteUser uid
            else do
              -- if we find the user in another team, we pretend it wasn't even there, to
              -- avoid leaking data to attackers (very unlikely, but hey).
              pure ()
  where
    deleteUserInSpar ::
      ( Member IdPConfigStore r,
        Member SAMLUserStore r,
        Member ScimExternalIdStore r,
        Member ScimUserTimesStore r
      ) =>
      User ->
      Scim.ScimHandler (Sem r) ()
    deleteUserInSpar account = do
      mIdpConfig <- mapM (lift . IdPConfigStore.getConfig) stiIdP

      case Brig.veidFromBrigUser account ((^. SAML.idpMetadata . SAML.edIssuer) <$> mIdpConfig) account.userEmailUnvalidated of
        Left _ -> pure ()
        Right veid -> lift $ do
          for_ (justThere veid.validScimIdAuthInfo) (SAMLUserStore.delete uid)
          ScimExternalIdStore.delete stiTeam veid.validScimIdExternal
      lift $ ScimUserTimesStore.delete uid

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
assertExternalIdUnused ::
  ( Member BrigAccess r,
    Member ScimExternalIdStore r,
    Member SAMLUserStore r
  ) =>
  TeamId ->
  ST.ValidScimId ->
  Scim.ScimHandler (Sem r) ()
assertExternalIdUnused =
  assertExternalIdInAllowedValues
    [Nothing]
    "externalId is already taken"

-- | `UserRef` must map to the given `UserId` or to `Nothing`.
--
-- ASSUMPTION: every scim user has a 'SAML.UserRef', and the `SAML.NameID` in it corresponds
-- to a single `externalId`.
assertExternalIdNotUsedElsewhere ::
  ( Member BrigAccess r,
    Member ScimExternalIdStore r,
    Member SAMLUserStore r
  ) =>
  TeamId ->
  ST.ValidScimId ->
  UserId ->
  Scim.ScimHandler (Sem r) ()
assertExternalIdNotUsedElsewhere tid veid wireUserId =
  assertExternalIdInAllowedValues
    [Nothing, Just wireUserId]
    "externalId already in use by another Wire user"
    tid
    veid

assertExternalIdInAllowedValues ::
  ( Member BrigAccess r,
    Member ScimExternalIdStore r,
    Member SAMLUserStore r
  ) =>
  [Maybe UserId] ->
  Text ->
  TeamId ->
  ST.ValidScimId ->
  Scim.ScimHandler (Sem r) ()
assertExternalIdInAllowedValues allowedValues errmsg tid veid = do
  isGood <-
    lift $ do
      mViaEid <- getUserIdByScimExternalId tid veid.validScimIdExternal
      mViaUref <- join <$> (for (justThere veid.validScimIdAuthInfo) ((userId <$$>) . getUserByUrefUnsafe))
      pure $ all (`elem` allowedValues) [mViaEid, mViaUref]
  unless isGood $
    throwError Scim.conflict {Scim.detail = Just errmsg}

assertHandleUnused :: (Member BrigAccess r) => Handle -> Scim.ScimHandler (Sem r) ()
assertHandleUnused = assertHandleUnused' "userName is already taken"

assertHandleUnused' :: (Member BrigAccess r) => Text -> Handle -> Scim.ScimHandler (Sem r) ()
assertHandleUnused' msg hndl =
  lift (BrigAccess.checkHandleAvailable hndl) >>= \case
    True -> pure ()
    False -> throwError Scim.conflict {Scim.detail = Just msg}

assertHandleNotUsedElsewhere :: (Member BrigAccess r) => UserId -> Handle -> Scim.ScimHandler (Sem r) ()
assertHandleNotUsedElsewhere uid hndl = do
  musr <- lift $ getAccount Brig.WithPendingInvitations uid
  unless ((userHandle =<< musr) == Just hndl) $
    assertHandleUnused' "userName already in use by another wire user" hndl

-- | Helper function that translates a given brig user into a 'Scim.StoredUser', with some
-- effects like updating the 'ManagedBy' field in brig and storing creation and update time
-- stamps.
synthesizeStoredUser ::
  forall r.
  ( Member (Input Opts) r,
    Member Now r,
    Member (Logger (Msg -> Msg)) r,
    Member BrigAccess r,
    Member GalleyAccess r,
    Member ScimUserTimesStore r
  ) =>
  User ->
  ST.ValidScimId ->
  Scim.ScimHandler (Sem r) (Scim.StoredUser ST.SparTag)
synthesizeStoredUser acc veid =
  logScim
    ( logFunction "Spar.Scim.User.synthesizeStoredUser"
        . logUser (userId acc)
        . maybe id logHandle acc.userHandle
        . maybe id logTeam acc.userTeam
        . maybe id logEmail (justHere $ ST.validScimIdAuthInfo veid)
    )
    logScimUserId
    $ do
      let uid = userId acc
          accStatus = acc.userStatus

      let readState :: Sem r (RI.RichInfo, Maybe (UTCTimeMillis, UTCTimeMillis), URIBS.URI, Role)
          readState =
            (,,,)
              <$> BrigAccess.getRichInfo uid
              <*> ScimUserTimesStore.read uid
              <*> inputs (derivedOptsScimBaseURI . derivedOpts)
              <*> getRole

      let writeState :: Maybe (UTCTimeMillis, UTCTimeMillis) -> ManagedBy -> RI.RichInfo -> Scim.StoredUser ST.SparTag -> Sem r ()
          writeState oldAccessTimes oldManagedBy oldRichInfo storedUser = do
            when (isNothing oldAccessTimes) $
              ScimUserTimesStore.write storedUser
            when (oldManagedBy /= ManagedByScim) $
              BrigAccess.setManagedBy uid ManagedByScim
            let newRichInfo = view ST.sueRichInfo . Scim.extra . Scim.value . Scim.thing $ storedUser
            when (oldRichInfo /= newRichInfo) $
              BrigAccess.setRichInfo uid newRichInfo

      (richInfo, accessTimes, baseuri, role) <- lift readState
      now <- toUTCTimeMillis <$> lift Now.get
      let (createdAt, lastUpdatedAt) = fromMaybe (now, now) accessTimes

      handle <- lift $ Brig.giveDefaultHandle acc

      let emails =
            maybeToList $
              acc.userEmailUnvalidated <|> (emailIdentity =<< userIdentity acc) <|> justHere veid.validScimIdAuthInfo

      storedUser <-
        synthesizeStoredUser'
          uid
          veid
          acc.userDisplayName
          emails
          handle
          richInfo
          accStatus
          createdAt
          lastUpdatedAt
          baseuri
          acc.userLocale
          (Just role)
      lift $ writeState accessTimes acc.userManagedBy richInfo storedUser
      pure storedUser
  where
    getRole :: Sem r Role
    getRole = do
      let tmRoleOrDefault m = fromMaybe defaultRole $ m >>= \member -> member ^. Member.permissions . to Member.permissionsRole
      maybe (pure defaultRole) (\tid -> tmRoleOrDefault <$> GalleyAccess.getTeamMember tid (userId acc)) (userTeam acc)

synthesizeStoredUser' ::
  (MonadError Scim.ScimError m) =>
  UserId ->
  ST.ValidScimId ->
  Name ->
  [EmailAddress] ->
  Handle ->
  RI.RichInfo ->
  AccountStatus ->
  UTCTimeMillis ->
  UTCTimeMillis ->
  URIBS.URI ->
  Locale ->
  Maybe Role ->
  m (Scim.StoredUser ST.SparTag)
synthesizeStoredUser' uid veid dname emails handle richInfo accStatus createdAt lastUpdatedAt baseuri locale mbRole = do
  let scimUser :: Scim.User ST.SparTag
      scimUser =
        synthesizeScimUser
          ST.ValidScimUser
            { ST.externalId = veid,
              ST.handle = handle {- 'Maybe' there is one in @usr@, but we want the type
                                    checker to make sure this exists, so we add it here
                                    redundantly, without the 'Maybe'. -},
              ST.emails = emails,
              ST.name = dname,
              ST.richInfo = richInfo,
              ST.active = ST.scimActiveFlagFromAccountStatus accStatus,
              ST.locale = Just locale,
              ST.role = mbRole
            }
  pure $ toScimStoredUser createdAt lastUpdatedAt baseuri uid (normalizeLikeStored scimUser)

synthesizeScimUser :: ST.ValidScimUser -> Scim.User ST.SparTag
synthesizeScimUser info =
  let userName = info.handle.fromHandle
   in (Scim.empty @ST.SparTag ST.userSchemas userName (ST.ScimUserExtra info.richInfo))
        { Scim.externalId = Just $ validScimIdExternal info.externalId,
          Scim.displayName = Just $ fromName info.name,
          Scim.active = Just . Scim.ScimBool $ info.active,
          Scim.preferredLanguage = lan2Text . lLanguage <$> info.locale,
          Scim.roles =
            maybe
              []
              ( (: [])
                  . Text.decodeUtf8With lenientDecode
                  . toStrict
                  . toByteString
              )
              (info.role),
          Scim.emails = (\e -> Scim.Email.Email Nothing (Scim.Email.EmailAddress e) Nothing) <$> info.emails
        }

-- TODO: now write a test, either in /integration or in spar, whichever is easier.  (spar)
getUserById ::
  forall r.
  ( Member BrigAccess r,
    Member GalleyAccess r,
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
          brigUser
          ((^. SAML.idpMetadata . SAML.edIssuer) <$> midp)
          brigUser.userEmailUnvalidated
  case mbveid of
    Right veid | userTeam brigUser == Just stiTeam -> lift $ do
      storedUser :: Scim.StoredUser ST.SparTag <- synthesizeStoredUser brigUser veid
      -- if we get a user from brig that hasn't been touched by scim yet, we call this
      -- function to move it under scim control.
      assertExternalIdNotUsedElsewhere stiTeam veid uid
      createValidScimUserSpar stiTeam uid storedUser veid
      lift $ do
        when (veidChanged brigUser veid) $
          BrigAccess.setSSOId uid (veidToUserSSOId veid)
        when (managedByChanged brigUser) $
          BrigAccess.setManagedBy uid ManagedByScim
      pure storedUser
    _ -> Applicative.empty
  where
    veidChanged :: User -> ST.ValidScimId -> Bool
    veidChanged usr veid = case userIdentity usr of
      Nothing -> True
      Just (EmailIdentity _) -> True
      Just (SSOIdentity ssoid _) -> Brig.veidToUserSSOId veid /= ssoid

    managedByChanged :: User -> Bool
    managedByChanged usr = userManagedBy usr /= ManagedByScim

scimFindUserByHandle ::
  forall r.
  ( Member BrigAccess r,
    Member GalleyAccess r,
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
  getUserById mIdpConfig stiTeam . userId $ brigUser

-- | Construct a 'ValidScimId'.  If it is an 'Email', find the non-SAML SCIM user in spar; if
-- that fails, find the user by email in brig.  If it is a 'UserRef', find the SAML user.
-- Return the result as a SCIM user.
--
-- Note the user won't get an entry in `spar.user`.  That will only happen on their first
-- successful authentication with their SAML credentials.
scimFindUserByExternalId ::
  forall r.
  ( Member BrigAccess r,
    Member GalleyAccess r,
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
scimFindUserByExternalId mIdpConfig stiTeam eid = do
  mViaEid :: Maybe UserId <- MaybeT $ Just <$> lift (ScimExternalIdStore.lookup stiTeam eid)
  uid <- case mViaEid of
    Nothing -> do
      veid <- MaybeT . lift $ either (const Nothing) Just <$> runError @Scim.ScimError (mkValidScimId mIdpConfig (Just eid) (emailAddressText eid))
      MaybeT . lift $ do
        -- there are a few ways to find a user. this should all be redundant, especially the where
        -- we lookup a user from brig by email, throw it away and only keep the uid, and then use
        -- the uid to lookup the account again. but cassandra, and also reasons.
        mViaEmail :: Maybe UserId <- join <$> (for (justHere veid.validScimIdAuthInfo) ((userId <$$>) . BrigAccess.getByEmail))
        mViaUref :: Maybe UserId <- join <$> (for (justThere veid.validScimIdAuthInfo) SAMLUserStore.get)
        pure $ mViaEmail <|> mViaUref
    Just uid -> pure uid
  acc <- MaybeT . lift . BrigAccess.getAccount Brig.WithPendingInvitations $ uid
  getUserById mIdpConfig stiTeam (userId acc)

logFilter :: Filter -> (Msg -> Msg)
logFilter (FilterAttrCompare attr op val) =
  Log.msg $ "filter:" <> rAttrPath attr <> " " <> rCompareOp op <> " " <> rCompValue val
  where
    rCompValue :: Scim.CompValue -> Text
    rCompValue = \case
      Scim.ValNull -> "null"
      Scim.ValBool True -> "true"
      Scim.ValBool False -> "false"
      Scim.ValNumber n -> LText.toStrict $ Aeson.encodeToLazyText (Aeson.Number n)
      Scim.ValString s ->
        "sha256 "
          <> sha256String s
          <> (if isJust (UUID.fromText s) then " original is a UUID" else "")

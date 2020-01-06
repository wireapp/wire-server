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

-- For @instance UserDB Spar@
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Doing operations with users via SCIM.
--
-- Provides a 'Scim.Class.User.UserDB' instance.
module Spar.Scim.User
    ( -- * Internals (for testing)
      validateScimUser'
    , toScimStoredUser'
    , mkUserRef
    , validateHandle
    ) where

import Imports
import Brig.Types.User as BrigTypes
import Galley.Types.Teams    as Galley
import Spar.Intra.Brig as Brig
import Spar.Intra.Galley as Galley
import Control.Lens  ((^.), view)
import Control.Monad.Except
import Control.Error ((??), (!?))
import Control.Exception (assert)
import Crypto.Hash
import Data.Aeson as Aeson
import Data.Id
import Data.Range
import Data.String.Conversions
import Network.URI

import Spar.App (Spar, Env, wrapMonadClient, sparCtxOpts,  wrapMonadClient, getUser)
import Spar.Scim.Types
import Spar.Scim.Auth ()
import Spar.Types

import qualified Data.Text    as Text
import qualified Data.UUID.V4 as UUID
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data    as Data
import qualified Spar.Intra.Brig as Intra.Brig
import qualified URI.ByteString as URIBS
import qualified System.Logger.Class as Log

import qualified Web.Scim.Class.User              as Scim
import qualified Web.Scim.Filter                  as Scim
import qualified Web.Scim.Handler                 as Scim
import qualified Web.Scim.Schema.Common           as Scim
import qualified Web.Scim.Schema.Error            as Scim
import qualified Web.Scim.Schema.ListResponse     as Scim
import qualified Web.Scim.Schema.Meta             as Scim
import qualified Web.Scim.Schema.ResourceType     as Scim
import qualified Web.Scim.Schema.User             as Scim
import qualified Web.Scim.Schema.User             as Scim.User (schemas)

-- | The information needed to synthesize a Scim user.
data NeededInfo = NeededInfo
  { neededHandle :: Handle
  , neededName :: Name
  , neededUserId :: UserId
  , neededExternalId :: Text
  , neededRichInfo :: RichInfo
  }

----------------------------------------------------------------------------
-- UserDB instance

instance Scim.UserDB SparTag Spar where
  -- | Filters users based on some predicate.  We do not support this endpoint
  -- without an explicit predicate due to performance reasons.  The predicate
  -- can currently look for users based on handle or SSO ID. 
  --
  -- This endpoint will look for users in Brig, and then synthesize the
  -- corresponding SCIM record.
  -- look for SSO users in brig; and then return the corresponding SCIM record.
  -- getUsers itself will not do any mutations in the SCIM database; as it
  -- feels weird to me (arian) that a GET causes side-effects like that.
  -- Instead, 'updateUser' will when updating a user see if the user was
  -- already in the SCIM Table; and if not insert it on demand, just like
  -- 'POST'.
  getUsers :: ScimTokenInfo
           -> Maybe Scim.Filter
           -> Scim.ScimHandler Spar (Scim.ListResponse (Scim.StoredUser SparTag))
  getUsers ScimTokenInfo{stiTeam} Nothing = do
    -- TODO(arianvp): DEPRECATED
    -- Getting users without a filter is deprecated an must be
    -- removed in the next release. It is never what we want; and non of the
    -- current SCIM implementations mandate its use. 
    --
    -- The code is rather problematic as it does O(n) HTTP requests to Brig (as
    -- brig does not support getting users in bulk). Given that Azure would
    -- send a /Users request _per user_ this would yield in O(n^2) requests,
    -- which blows up extremely quickly for large teams.
    --
    -- NOTE: unless you supply a filter, it will only list
    -- SCIM users, not all users in a team. That was the original behaviour
    -- and we didn't feel like changing it for this as it's going to be removed
    -- soon anyway. Please scream loudly in the review if you disagree with this,
    -- but it would complicate this code a bit, instead of leaving it as is.
    members <- lift $ Galley.getTeamMembers stiTeam
    brigusers :: [User]
      <- filter (not . userDeleted) <$>
         lift (Intra.Brig.getBrigUsers ((^. Galley.userId) <$> members))
    scimusers :: [Scim.StoredUser SparTag]
      <- lift . wrapMonadClient . Data.getScimUsers $ BrigTypes.userId <$> brigusers
    pure $ Scim.fromList scimusers

  getUsers ScimTokenInfo{stiTeam, stiIdP} (Just filter') = do
    idp <- stiIdP ?? Scim.serverError "No IdP configured for the provisioning token"
    idpConfig <- (wrapMonadClient . Data.getIdPConfig $ idp)  !? Scim.serverError "No IdP configured for the provisioning token"
    neededInfo <-
      case filter' of
        Scim.FilterAttrCompare (Scim.AttrPath schema attrName _subAttr) Scim.OpEq (Scim.ValString val)
          | Scim.isUserSchema schema ->
            -- TODO(arianvp,fisx): Some of this code is already in Spar.App  but
            -- just slightly different.  i.e. there is no byHandle functions in
            -- Spar.App.   Perhaps we should refactor this code a bit and make
            -- common pieces reusable
            case attrName of
              "username" -> do
                handle <- validateHandle val
                brigUser <- Intra.Brig.getBrigUserByHandle handle -- !? Scim.notFound "User" "Couldn't find user by handle"
                unless (userTeam brigUser == Just stiTeam) $ throwError $ Scim.notFound "User" "Not in team"
                let uid = BrigTypes.userId brigUser
                richInfo <- lift $ Intra.Brig.getBrigUserRichInfo uid

                -- NOTE: presence of ssoIdentity is enough to decide that the
                -- user is indeed an SSO user TODO(arianvp): This requirement
                -- might change in the future!
                externalId' <- lift . toExternalId =<< (userIdentity brigUser >>= ssoIdentity) ?? Scim.notFound "User" "Not an SSO User"
                pure $ NeededInfo
                  { neededHandle = handle
                  , neededName = userName brigUser
                  , neededUserId = uid
                  , neededExternalId = externalId'
                  , neededRichInfo = richInfo
                  }
              "externalid" -> do
                uref <- mkUserRef idpConfig (pure val)
                uid <- (wrapMonadClient . Data.getSAMLUser $ uref) !? Scim.notFound "User" "Not a known SAML User"
                brigUser <- Intra.Brig.getBrigUser uid !? Scim.notFound "User" "Not a known brig user"
                unless (userTeam brigUser == Just stiTeam) $ throwError $ Scim.notFound "User" "Not in team"
                richInfo <- lift $ Intra.Brig.getBrigUserRichInfo uid
                -- NOTE: The SSO login flow without scim _forces_ the user to
                -- choose a handle. So this isn't a problem in practise
                handle <- userHandle brigUser ?? Scim.serverError "Found an SSO user without a handle. This should not happen!"
                pure $ NeededInfo
                  { neededHandle = handle
                  , neededName = userName brigUser
                  , neededUserId = uid
                  , neededExternalId = val
                  , neededRichInfo = richInfo
                  }
              _ -> throwError $ Scim.badRequest Scim.InvalidFilter (Just "Attribute not supported")
                
          | otherwise -> throwError $ Scim.badRequest Scim.InvalidFilter (Just "Not a supported schema")
        _ -> throwError $ Scim.badRequest Scim.InvalidFilter (Just "Operation not supported")

    let buid = neededUserId neededInfo
    -- NOTE: We set ManagedBy to SCIM as quickly as possible; as to reduce
    -- the amount of time a race condition can occur.  
    mScimUser <- lift . wrapMonadClient . Data.getScimUser $ buid
    case mScimUser of
      Just scimUser -> pure $ Scim.fromList [scimUser]
      Nothing -> do
        -- NOTE: This case only happens when you search users by "externalId". By construction doesn't happen when you search by handle
        let user = synthesizeScimUser neededInfo 
        storedUser <- lift $ toScimStoredUser buid user
        lift . wrapMonadClient $ Data.insertScimUser buid storedUser
        pure $ Scim.fromList [storedUser]

  -- | Get a single user by its ID.
  getUser :: ScimTokenInfo
          -> UserId
          -> Scim.ScimHandler Spar (Scim.StoredUser SparTag)
  getUser ScimTokenInfo{stiTeam} uid = do
    mbBrigUser <- lift (Intra.Brig.getBrigUser uid)
    mbScimUser <- if isJust mbBrigUser && (userTeam =<< mbBrigUser) == Just stiTeam
      then lift . wrapMonadClient . Data.getScimUser $ uid
      else pure Nothing
    maybe (throwError . Scim.notFound "User" $ idToText uid) pure mbScimUser

  -- | Create a new user.
  postUser :: ScimTokenInfo
           -> Scim.User SparTag
           -> Scim.ScimHandler Spar (Scim.StoredUser SparTag)
  postUser tokinfo user = createValidScimUser =<< validateScimUser tokinfo user

  putUser :: ScimTokenInfo
          -> UserId
          -> Scim.User SparTag
          -> Scim.ScimHandler Spar (Scim.StoredUser SparTag)
  putUser tokinfo uid newScimUser =
    updateValidScimUser tokinfo uid =<< validateScimUser tokinfo newScimUser

  deleteUser :: ScimTokenInfo -> UserId -> Scim.ScimHandler Spar ()
  deleteUser = deleteScimUser


----------------------------------------------------------------------------
-- User creation and validation

-- | Validate a raw SCIM user record and extract data that we care about. See also:
-- 'ValidScimUser''.
validateScimUser
  :: forall m. (m ~ Scim.ScimHandler Spar)
  => ScimTokenInfo    -- ^ Used to decide what IdP to assign the user to
  -> Scim.User SparTag
  -> m ValidScimUser
validateScimUser ScimTokenInfo{stiIdP} user = do
    idp <- stiIdP ?? Scim.serverError "No IdP configured for the provisioning token"
    idpConfig <- (wrapMonadClient . Data.getIdPConfig $ idp) !? 
      Scim.serverError "No IdP configured for the provisioning token"
    richInfoLimit <- lift $ asks (richInfoLimit . sparCtxOpts)
    validateScimUser' idpConfig richInfoLimit user

-- | Validate a handle (@userName@).
-- We should lowercase the wire handle here first, as SCIM says it's case insensitive.
-- TODO(arianvp): We should do this at the hscim level. but for now I do it here
validateHandle :: MonadError Scim.ScimError m => Text -> m Handle
validateHandle txt = case parseHandle (Text.toLower txt) of
    Just h -> pure h
    Nothing -> throwError $ Scim.badRequest Scim.InvalidValue
        (Just "userName must be a valid Wire handle")

-- | Map the SCIM data on the spar and brig schemata, and throw errors if the SCIM data does
-- not comply with the standard / our constraints. See also: 'ValidScimUser'.
--
-- Checks like "is this handle claimed already?" are not performed. Only schema checks.
--
-- __Mapped fields:__
--
--   * @userName@ is mapped to our 'userHandle'.
--
--   * @displayName@ is mapped to our 'userName'. We don't use the @name@ field, as it
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
validateScimUser'
  :: forall m. (MonadError Scim.ScimError m)
  => IdP        -- ^ IdP that the resulting user will be assigned to
  -> Int        -- ^ Rich info limit
  -> Scim.User SparTag
  -> m ValidScimUser
validateScimUser' idp richInfoLimit user = do
    uref :: SAML.UserRef <- mkUserRef idp (Scim.externalId user)
    handl <- validateHandle (Scim.userName user)
    mbName <- mapM validateName (Scim.displayName user)
    richInfo <- validateRichInfo (Scim.extra user ^. sueRichInfo)
    pure $ ValidScimUser user uref handl mbName richInfo
  where

    -- Validate a name (@displayName@). It has to conform to standard Wire rules.
    validateName :: Text -> m Name
    validateName txt = case checkedEitherMsg @_ @1 @128 "displayName" txt of
        Right rtxt -> pure $ Name (fromRange rtxt)
        Left err -> throwError $ Scim.badRequest Scim.InvalidValue
            (Just ("displayName must be a valid Wire name, but: " <> Text.pack err))

    -- Validate rich info (@richInfo@). It must not exceed the rich info limit.
    validateRichInfo :: RichInfo -> m RichInfo
    validateRichInfo richInfo = do
        let size = richInfoSize richInfo
        when (size > richInfoLimit) $ throwError $
            (Scim.badRequest Scim.InvalidValue
                 (Just . cs $
                      "richInfo exceeds the limit: max " <> show richInfoLimit <>
                      " characters, but got " <> show size))
            { Scim.status = Scim.Status 413 }
        pure richInfo

-- | Given an 'externalId' and an 'IdP', construct a 'SAML.UserRef'.
--
-- This is needed primarily in 'validateScimUser', but also in 'updateValidScimUser' to
-- recover the 'SAML.UserRef' of the scim user before the update from the database.
mkUserRef
  :: forall m. (MonadError Scim.ScimError m)
  => IdP
  -> Maybe Text
  -> m SAML.UserRef
mkUserRef idp extid = case extid of
    Just subjectTxt -> do
        let issuer = idp ^. SAML.idpMetadata . SAML.edIssuer
        subject <- validateSubject subjectTxt
        pure $ SAML.UserRef issuer subject
    Nothing -> throwError $ Scim.badRequest Scim.InvalidValue
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
            Left err -> throwError $ Scim.badRequest Scim.InvalidValue
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
createValidScimUser
  :: forall m. (m ~ Scim.ScimHandler Spar)
  => ValidScimUser -> m (Scim.StoredUser SparTag)
createValidScimUser (ValidScimUser user uref handl mbName richInfo) = do
    -- Generate a UserId will be used both for scim user in spar and for brig.
    buid <- Id <$> liftIO UUID.nextRandom
    -- ensure uniqueness constraints of all affected identifiers.
    -- if we crash now, retry POST will just work
    assertUserRefUnused uref
    assertHandleUnused handl buid
    -- if we crash now, retry POST will just work, or user gets told the handle
    -- is already in use and stops POSTing
   
    -- TODO(arianvp): Get rid of manual lifting. Needs to be SCIM instances for ExceptT
    -- This is the pain and the price you pay for the horribleness called MTL
    storedUser <- lift $ toScimStoredUser buid user
    idpConfig <-  lift $ SAML.getIdPConfigByIssuer (uref ^. SAML.uidTenant)
    let teamid = view SAML.idpExtraInfo idpConfig
    buid' <- lift $ Intra.Brig.createBrigUser uref buid teamid mbName ManagedByScim
    assert (buid == buid') $ pure ()
    -- If we crash now, we have an active user that cannot login. And can not
    -- be bound this will be a zombie user that needs to be manually cleaned
    -- up.  We should consider making setUserHandle part of createUser and
    -- making it transactional.  If the user redoes the POST A new standalone
    -- user will be created
    lift $ Intra.Brig.setBrigUserHandle buid handl
    -- If we crash now,  a POST retry will fail with 409 user already exists. 
    -- Azure at some point will retry with GET /Users?filter=userName eq handle
    -- and then issue a PATCH containing the rich info and the externalId
    -- TODO(arianvp): Implement PATCH (Part of Phase 1)
    lift $ Intra.Brig.setBrigUserRichInfo buid richInfo
    -- If we crash now, same as above, but the PATCH will only contain externalId
    
    -- TODO(arianvp): these two actions we probably want to make transactional
    lift . wrapMonadClient $ Data.insertScimUser buid storedUser
    lift . wrapMonadClient $ Data.insertSAMLUser uref buid
    pure storedUser

updateValidScimUser
  :: forall m. (m ~ Scim.ScimHandler Spar)
  => ScimTokenInfo -> UserId -> ValidScimUser -> m (Scim.StoredUser SparTag)
updateValidScimUser tokinfo@ScimTokenInfo{stiIdP} uid newScimUser = do
    -- TODO: currently the types in @hscim@ are constructed in such a way that
    -- 'Scim.User.User' doesn't contain an ID, only 'Scim.StoredUser'
    -- does. @fisx believes that this situation could be improved (see
    -- <https://github.com/wireapp/wire-server/pull/559#discussion_r247392882>).
    --
    -- If 'Scim.User.User' and 'ValidScimUser' did contain the user ID, we wouldn't need
    -- 'uidText' in this function -- or we could at least check in hscim that the ID in the
    -- user object matches the ID in the path.

    -- TODO: how do we get this safe w.r.t. race conditions / crashes?

    -- construct old and new user values with metadata.
    oldScimStoredUser :: Scim.StoredUser SparTag
      <- Scim.getUser tokinfo uid

    assertUserRefNotUsedElsewhere (newScimUser ^. vsuSAMLUserRef) uid
    assertHandleNotUsedElsewhere (newScimUser ^. vsuHandle) uid

    if Scim.value (Scim.thing oldScimStoredUser) == (newScimUser ^. vsuUser)
      then pure oldScimStoredUser
      else do
        newScimStoredUser :: Scim.StoredUser SparTag
          <- lift $ updScimStoredUser (newScimUser ^. vsuUser) oldScimStoredUser

        -- update 'SAML.UserRef' on spar (also delete the old 'SAML.UserRef' if it exists and
        -- is different from the new one)
        let newuref = newScimUser ^. vsuSAMLUserRef
        molduref <- do
          let eid = Scim.externalId . Scim.value . Scim.thing $ oldScimStoredUser
          (lift . wrapMonadClient . Data.getIdPConfig) `mapM` stiIdP >>= \case
            Just (Just idp) -> Just <$> mkUserRef idp eid
            _               -> pure Nothing
        case molduref of
          Just olduref -> when (olduref /= newuref) $ do
            lift . wrapMonadClient $ Data.deleteSAMLUser olduref
            lift . wrapMonadClient $ Data.insertSAMLUser newuref uid
          Nothing -> do
            -- if there was no uref before.  (can't currently happen because we require saml
            -- for scim to work, but this would be the right way to handle the case.)
            lift . wrapMonadClient $ Data.insertSAMLUser newuref uid

        -- update 'SAML.UserRef' on brig
        bindok <- lift $ Intra.Brig.bindBrigUser uid newuref
        unless bindok . throwError $
            Scim.serverError "Failed to update SAML UserRef on brig."
            -- this can only happen if user is found in spar.scim_user, but missing on brig.
            -- (internal error?  race condition?)

        -- TODO: rich info and/or user handle may not have changed.  in that case don't write
        -- it.
        maybe (pure ()) (lift . Intra.Brig.setBrigUserName uid) $ newScimUser ^. vsuName
        lift . Intra.Brig.setBrigUserHandle uid $ newScimUser ^. vsuHandle
        lift . Intra.Brig.setBrigUserRichInfo uid $ newScimUser ^. vsuRichInfo

        -- store new user value to scim_user table (spar). (this must happen last, so in case
        -- of crash the client can repeat the operation and it won't be considered a noop.)
        lift . wrapMonadClient $ Data.insertScimUser uid newScimStoredUser

        pure newScimStoredUser


toScimStoredUser
  :: forall m. (SAML.HasNow m, MonadReader Env m)
  => UserId -> Scim.User SparTag -> m (Scim.StoredUser SparTag)
toScimStoredUser uid usr = do
  now <- SAML.getNow
  baseuri <- asks $ derivedOptsScimBaseURI . derivedOpts . sparCtxOpts
  pure $ toScimStoredUser' now baseuri uid usr

toScimStoredUser'
  :: HasCallStack
  => SAML.Time
  -> URIBS.URI
  -> UserId
  -> Scim.User SparTag
  -> Scim.StoredUser SparTag
toScimStoredUser' (SAML.Time now) baseuri uid usr =
    Scim.WithMeta meta $
    Scim.WithId uid $
    usr { Scim.User.schemas = userSchemas }
  where
    mkLocation :: String -> URI
    mkLocation pathSuffix = convURI $ baseuri SAML.=/ cs pathSuffix
      where
        convURI uri = fromMaybe err . parseURI . cs . URIBS.serializeURIRef' $ uri
          where err = error $ "internal error: " <> show uri

    meta = Scim.Meta
      { Scim.resourceType = Scim.UserResource
      , Scim.created = now
      , Scim.lastModified = now
      , Scim.version = calculateVersion uid usr
        -- TODO: it looks like we need to add this to the HTTP header.
        -- https://tools.ietf.org/html/rfc7644#section-3.14
      , Scim.location = Scim.URI . mkLocation $ "/Users/" <> cs (idToText uid)
      }

updScimStoredUser
  :: forall m. (SAML.HasNow m)
  => Scim.User SparTag
  -> Scim.StoredUser SparTag
  -> m (Scim.StoredUser SparTag)
updScimStoredUser usr storedusr = do
  now <- SAML.getNow
  pure $ updScimStoredUser' now usr storedusr

updScimStoredUser'
  :: SAML.Time
  -> Scim.User SparTag
  -> Scim.StoredUser SparTag
  -> Scim.StoredUser SparTag
updScimStoredUser' (SAML.Time moddate) usr (Scim.WithMeta meta (Scim.WithId scimuid _)) =
    Scim.WithMeta meta' (Scim.WithId scimuid usr)
  where
    meta' = meta
      { Scim.lastModified = moddate
      , Scim.version = calculateVersion scimuid usr
      }


deleteScimUser
  :: ScimTokenInfo -> UserId -> Scim.ScimHandler Spar ()
deleteScimUser ScimTokenInfo{stiTeam} uid = do
    mbBrigUser <- lift (Intra.Brig.getBrigUser uid)
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
          throwError $ Scim.notFound "user" (idToText uid)
        ssoId <- maybe (logThenServerError $ "no userSSOId for user " <> cs (idToText uid))
                       pure
                       $ BrigTypes.userSSOId brigUser
        uref <- either logThenServerError pure $ Intra.Brig.fromUserSSOId ssoId
        lift . wrapMonadClient $ Data.deleteSAMLUser uref
        lift . wrapMonadClient $ Data.deleteScimUser uid
        lift $ Intra.Brig.deleteBrigUser uid
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
calculateVersion
  :: UserId
  -> Scim.User SparTag
  -> Scim.ETag
calculateVersion uid usr = Scim.Weak (Text.pack (show h))
  where
    h :: Digest SHA256
    h = hashlazy (Aeson.encode (Scim.WithId uid usr))

{-|
Check that the UserRef is not taken.

ASSUMPTION: every scim user has a 'SAML.UserRef', and the `SAML.NameID` in it corresponds
to a single `externalId`.
-}
assertUserRefUnused :: SAML.UserRef -> Scim.ScimHandler Spar ()
assertUserRefUnused userRef = do
  mExistingUserId <- lift $ getUser userRef
  unless (isNothing mExistingUserId) $
    throwError Scim.conflict {Scim.detail = Just "externalId is already taken"}

{-|
Check that the UserRef is not taken any user other than the passed 'UserId'
(it is also acceptable if it is not taken by anybody).

ASSUMPTION: every scim user has a 'SAML.UserRef', and the `SAML.NameID` in it corresponds
to a single `externalId`.
-}
assertUserRefNotUsedElsewhere :: SAML.UserRef -> UserId -> Scim.ScimHandler Spar ()
assertUserRefNotUsedElsewhere userRef wireUserId = do
  mExistingUserId <- lift $ getUser userRef
  unless (mExistingUserId `elem` [Nothing, Just wireUserId]) $ do
    throwError Scim.conflict {Scim.detail = Just "externalId does not match UserId"}

assertHandleUnused :: Handle -> UserId -> Scim.ScimHandler Spar ()
assertHandleUnused = assertHandleUnused' "userName is already taken"

assertHandleUnused' :: Text -> Handle -> UserId -> Scim.ScimHandler Spar ()
assertHandleUnused' msg hndl uid = lift (Brig.checkHandleAvailable hndl uid) >>= \case
  True  -> pure ()
  False -> throwError Scim.conflict {Scim.detail = Just msg}

assertHandleNotUsedElsewhere :: Handle -> UserId -> Scim.ScimHandler Spar ()
assertHandleNotUsedElsewhere hndl uid = do
  musr <- lift $ Brig.getBrigUser uid
  unless ((userHandle =<< musr) == Just hndl) $
    assertHandleUnused' "userName does not match UserId" hndl uid

-- | Fails if the handle isn't set
synthesizeScimUser :: NeededInfo -> Scim.User SparTag
synthesizeScimUser info =
  let 
    Handle userName = neededHandle info
    Name displayName = neededName info
  in
    (Scim.empty userSchemas userName (ScimUserExtra (neededRichInfo info)))
      { Scim.externalId = Just $ neededExternalId info
      , Scim.displayName = Just displayName
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

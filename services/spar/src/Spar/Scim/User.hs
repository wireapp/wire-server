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
    ) where

import Imports
import Brig.Types.User       as Brig
import Control.Lens hiding ((.=), Strict)
import Control.Monad.Except
import Control.Monad.Extra (whenM)
import Crypto.Hash
import Data.Aeson as Aeson
import Data.Id
import Data.Range
import Data.String.Conversions
import Galley.Types.Teams    as Galley
import Network.URI
import Spar.App (Spar, Env, wrapMonadClient, sparCtxOpts, createUser_, wrapMonadClient)
import Spar.Intra.Galley
import Spar.Scim.Types
import Spar.Scim.Auth ()
import Spar.Types

import qualified Data.Text    as Text
import qualified Data.UUID.V4 as UUID
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data    as Data
import qualified Spar.Intra.Brig as Intra.Brig
import qualified URI.ByteString as URIBS

import qualified Web.Scim.Class.User              as Scim
import qualified Web.Scim.Filter                  as Scim
import qualified Web.Scim.Handler                 as Scim
import qualified Web.Scim.Schema.Common           as Scim
import qualified Web.Scim.Schema.Error            as Scim
import qualified Web.Scim.Schema.ListResponse     as Scim
import qualified Web.Scim.Schema.Meta             as Scim
import qualified Web.Scim.Schema.ResourceType     as Scim
import qualified Web.Scim.Schema.User             as Scim


----------------------------------------------------------------------------
-- UserDB instance

instance Scim.UserDB Spar where
  -- | List all users, possibly filtered by some predicate.
  list :: ScimTokenInfo
       -> Maybe Scim.Filter
       -> Scim.ScimHandler Spar (Scim.ListResponse Scim.StoredUser)
  list ScimTokenInfo{stiTeam} mbFilter = do
    members <- lift $ getTeamMembers stiTeam
    brigusers :: [User]
      <- filter (not . userDeleted) <$>
         lift (Intra.Brig.getUsers ((^. Galley.userId) <$> members))
    scimusers :: [Scim.StoredUser]
      <- lift . wrapMonadClient . Data.getScimUsers $ Brig.userId <$> brigusers
    let check user = case mbFilter of
          Nothing -> pure True
          Just filter_ ->
            let user' = Scim.value (Scim.thing user)
            in case Scim.filterUser filter_ user' of
                 Right res -> pure res
                 Left err  -> throwError $
                   Scim.badRequest Scim.InvalidFilter (Just err)
    -- FUTUREWORK: once bigger teams arrive, we should have pagination here.
    Scim.fromList <$> filterM check scimusers

  -- | Get a single user by its ID.
  get :: ScimTokenInfo
      -> Text
      -> Scim.ScimHandler Spar (Maybe Scim.StoredUser)
  get ScimTokenInfo{stiTeam} uidText = do
    uid <- parseUid uidText
    mbBrigUser <- lift (Intra.Brig.getUser uid)
    if isJust mbBrigUser && (userTeam =<< mbBrigUser) == Just stiTeam
      then lift . wrapMonadClient . Data.getScimUser $ uid
      else pure Nothing

  -- | Create a new user.
  create :: ScimTokenInfo
         -> Scim.User
         -> Scim.ScimHandler Spar Scim.StoredUser
  create tokinfo user =
    createValidScimUser =<< validateScimUser tokinfo user

  update :: ScimTokenInfo
         -> Text
         -> Scim.User
         -> Scim.ScimHandler Spar Scim.StoredUser
  update tokinfo uidText newScimUser =
    updateValidScimUser tokinfo uidText =<< validateScimUser tokinfo newScimUser

  delete :: ScimTokenInfo -> Text -> Scim.ScimHandler Spar Bool
  delete _ _ =
      throwError $ Scim.serverError "User delete is not implemented yet"  -- TODO

  getMeta :: ScimTokenInfo -> Scim.ScimHandler Spar Scim.Meta
  getMeta _ =
      throwError $ Scim.serverError "User getMeta is not implemented yet"  -- TODO

----------------------------------------------------------------------------
-- User creation and validation

-- | Validate a raw SCIM user record and extract data that we care about.
validateScimUser
  :: forall m. (m ~ Scim.ScimHandler Spar)
  => ScimTokenInfo    -- ^ Used to decide what IdP to assign the user to
  -> Scim.User
  -> m ValidScimUser
validateScimUser ScimTokenInfo{stiIdP} user = do
    idp <- case stiIdP of
        Nothing -> throwError $
          Scim.serverError "No IdP configured for the provisioning token"
        Just idp -> lift (wrapMonadClient (Data.getIdPConfig idp)) >>= \case
            Nothing -> throwError $
              Scim.serverError "The IdP corresponding to the provisioning token \
                               \was not found"
            Just idpConfig -> pure idpConfig
    validateScimUser' (Just idp) user

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
-- @externalId@ by different users.
--
-- __Emails and phone numbers:__ we'd like to ensure that only verified emails and phone
-- numbers end up in our database, and implementing verification requires design decisions
-- that we haven't made yet. We store them in our SCIM blobs, but don't syncronize them with
-- Brig. See <https://github.com/wireapp/wire-server/pull/559#discussion_r247466760>.
validateScimUser'
  :: forall m. (MonadError Scim.ScimError m)
  => Maybe IdP        -- ^ IdP that the resulting user will be assigned to
  -> Scim.User
  -> m ValidScimUser
validateScimUser' Nothing _ =
    throwError $ Scim.serverError "SCIM users without SAML SSO are not supported"
validateScimUser' (Just idp) user = do
    uref :: SAML.UserRef <- case Scim.externalId user of
        Just subjectTxt -> do
            let issuer = idp ^. SAML.idpMetadata . SAML.edIssuer
            subject <- validateSubject subjectTxt
            pure $ SAML.UserRef issuer subject
        Nothing -> throwError $ Scim.badRequest Scim.InvalidValue
            (Just "externalId is required for SAML users")
    handl <- validateHandle (Scim.userName user)
    mbName <- mapM validateName (Scim.displayName user)
    pure $ ValidScimUser user uref handl mbName

  where
    -- Validate a subject ID (@externalId@).
    validateSubject :: Text -> m SAML.NameID
    validateSubject txt = do
        let unameId = case parseEmail txt of
                Just _  -> SAML.UNameIDEmail txt
                Nothing -> SAML.UNameIDUnspecified txt
        case SAML.mkNameID unameId Nothing Nothing Nothing of
            Right nameId -> pure nameId
            Left err -> throwError $ Scim.serverError
                ("Can't construct a subject ID from externalId: " <> Text.pack err)

    -- Validate a handle (@userName@).
    validateHandle :: Text -> m Handle
    validateHandle txt = case parseHandle txt of
        Just h -> pure h
        Nothing -> throwError $ Scim.badRequest Scim.InvalidValue
            (Just "userName must be a valid Wire handle")

    -- Validate a name (@displayName@). It has to conform to standard Wire rules.
    validateName :: Text -> m Name
    validateName txt = case checkedEitherMsg @_ @1 @128 "displayName" txt of
        Right rtxt -> pure $ Name (fromRange rtxt)
        Left err -> throwError $ Scim.badRequest Scim.InvalidValue
            (Just ("displayName must be a valid Wire name, but: " <> Text.pack err))

-- | We only allow SCIM users that authenticate via SAML. (This is by no means necessary,
-- though. It can be relaxed to allow creating users with password authentication if that is a
-- requirement.)
createValidScimUser
  :: forall m. (m ~ Scim.ScimHandler Spar)
  => ValidScimUser -> m Scim.StoredUser
createValidScimUser (ValidScimUser user uref handl mbName) = do
    -- FUTUREWORK: The @hscim@ library checks that the handle is not taken before 'create' is
    -- even called. However, it does that in an inefficient manner. We should remove the check
    -- from @hscim@ and do it here instead.

    -- Check that the UserRef is not taken.
    whenM (isJust <$> lift (wrapMonadClient (Data.getUser uref))) $
        throwError Scim.conflict {Scim.detail = Just "externalId is already taken"}
    -- Generate a UserId will be used both for scim user in spar and for brig.
    buid <- Id <$> liftIO UUID.nextRandom
    -- Create SCIM user here in spar.
    storedUser <- lift $ toScimStoredUser buid user
    lift . wrapMonadClient $ Data.insertScimUser buid storedUser
    -- Create SAML user here in spar, which in turn creates a brig user.
    lift $ createUser_ buid uref mbName ManagedByScim
    -- Set user handle on brig (which can't be done during user creation yet).
    -- TODO: handle errors better here?
    lift $ Intra.Brig.setHandle buid handl

    pure storedUser

    -- FUTUREWORK: think about potential failure points in this function (SCIM can succeed but
    -- SAML can fail, Brig user creation can succeed but handle-setting can fail).

updateValidScimUser
  :: forall m. (m ~ Scim.ScimHandler Spar)
  => ScimTokenInfo -> Text -> ValidScimUser -> m Scim.StoredUser
updateValidScimUser tokinfo uidText newScimUser = do

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
    uid :: UserId <- parseUid uidText
    oldScimStoredUser :: Scim.StoredUser
      <- let err = throwError $ Scim.notFound "user" uidText
         in maybe err pure =<< Scim.get tokinfo uidText

    if Scim.value (Scim.thing oldScimStoredUser) == (newScimUser ^. vsuUser)
      then pure oldScimStoredUser
      else do
        newScimStoredUser :: Scim.StoredUser
          <- lift $ updScimStoredUser (newScimUser ^. vsuUser) oldScimStoredUser

        -- update 'SAML.UserRef'
        let uref = newScimUser ^. vsuSAMLUserRef
        lift . wrapMonadClient $ Data.insertUser uref uid  -- on spar
        bindok <- lift $ Intra.Brig.bindUser uid uref  -- on brig
        unless bindok . throwError $
          Scim.serverError "Failed to update SAML UserRef (no such UserId? duplicate Handle?)"

        maybe (pure ()) (lift . Intra.Brig.setName uid) $ newScimUser ^. vsuName
        lift . Intra.Brig.setHandle uid $ newScimUser ^. vsuHandle

        -- store new user value to scim_user table (spar). (this must happen last, so in case
        -- of crash the client can repeat the operation and it won't be considered a noop.)
        lift . wrapMonadClient $ Data.insertScimUser uid newScimStoredUser

        pure newScimStoredUser


toScimStoredUser
  :: forall m. (SAML.HasNow m, MonadReader Env m)
  => UserId -> Scim.User -> m Scim.StoredUser
toScimStoredUser uid usr = do
  now <- SAML.getNow
  baseuri <- asks $ derivedOptsScimBaseURI . derivedOpts . sparCtxOpts
  pure $ toScimStoredUser' now baseuri uid usr

toScimStoredUser'
  :: HasCallStack
  => SAML.Time -> URIBS.URI -> UserId -> Scim.User -> Scim.StoredUser
toScimStoredUser' (SAML.Time now) baseuri (idToText -> uid) usr =
    Scim.WithMeta meta (Scim.WithId uid usr)
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
      , Scim.location = Scim.URI . mkLocation $ "/Users/" <> cs uid
      }

updScimStoredUser
  :: forall m. (SAML.HasNow m)
  => Scim.User -> Scim.StoredUser -> m Scim.StoredUser
updScimStoredUser usr storedusr = do
  now <- SAML.getNow
  pure $ updScimStoredUser' now usr storedusr

updScimStoredUser'
  :: SAML.Time
  -> Scim.User
  -> Scim.StoredUser
  -> Scim.StoredUser
updScimStoredUser' (SAML.Time moddate) usr (Scim.WithMeta meta (Scim.WithId scimuid _)) =
    Scim.WithMeta meta' (Scim.WithId scimuid usr)
  where
    meta' = meta
      { Scim.lastModified = moddate
      , Scim.version = calculateVersion scimuid usr
      }

----------------------------------------------------------------------------
-- Utilities

parseUid
  :: forall m m'. (m ~ Scim.ScimHandler m', Monad m')
  => Text -> m UserId
parseUid uidText = maybe err pure $ readMaybe (Text.unpack uidText)
  where err = throwError $ Scim.notFound "user" uidText

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
  :: Text               -- ^ User ID
  -> Scim.User
  -> Scim.ETag
calculateVersion uidText usr = Scim.Weak (Text.pack (show h))
  where
    h :: Digest SHA256
    h = hashlazy (Aeson.encode (Scim.WithId uidText usr))

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
-- There are two problems with error handling here:
--
-- 1. We want all errors originating from SCIM handlers to be thrown as SCIM
--    errors, not as Spar errors. Currently errors thrown from things like
--    'getTeamMembers' will look like Spar errors and won't be wrapped into
--    the 'ScimError' type. This might or might not be important, depending
--    on what is expected by apps that use the SCIM interface.
--
-- 2. We want generic error descriptions in response bodies, while still
--    logging nice error messages internally.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- TODO remove (orphans can be avoided by only implementing functions here, and gathering them
-- in the instance near the Spar type.)
{-# OPTIONS_GHC
    -Wno-missing-methods
    -Wno-unused-imports
    -Wno-orphans
    -Wunused-top-binds
  #-}

-- | An implementation of the SCIM API for doing bulk operations with users.
--
-- See <https://en.wikipedia.org/wiki/System_for_Cross-domain_Identity_Management>
--
-- = SCIM user creation flow
--
-- When a user is created via SCIM, a SAML user identity has to be created with it. Currently
-- we don't allow SCIM users without SAML user identities.
--
-- Creating these two user identities (SCIM and SAML) together requires constructing a
-- 'UserRef' from the SCIM request, which is then stored by 'Spar.Data.insertUser'.
--
-- The 'UserRef' consists of:
--
--   * tenant (the url-shaped ID the IdP assigns to itself);
--
--   * subject (usually an email, or an unstructured nickname, or a few more obscure
--     alternatives).
--
-- /Tenant:/ if there is only one IdP for the current team, the tenant can be found by calling
-- 'getIdPConfigsByTeam' and looking up @^. idpMetadata . edIssuer@ on the result. If there is
-- more than one IdP, we need a way to associate user creation requests with specific IdPs.
-- Currently we disallow teams with more than one IdP.
--
-- /Subject:/ there are different reasonable ways to pick a subject for a user; this should be
-- configurable in the team settings page (e.g. a choice of one field from the SCIM user
-- schema, optionally transformed with one of a few hard-coded functions). A simple default
-- could be "take the email address, and type it as an email address", or in saml2-web-sso
-- pseudo-code: @\email -> entityNameID (parseURI ("email:" <> renderEmail email))@.

module Spar.Scim
  (
  -- * The API
    APIScim
  , apiScim
  -- ** Request and response types
  , CreateScimToken(..)
  , CreateScimTokenResponse(..)
  , ScimTokenList(..)

  -- * testing
  , validateScimUser'
  , toScimStoredUser'
  ) where

import Imports
import Brig.Types.User       as Brig
import Control.Exception
import Control.Lens hiding ((.=), Strict)
import Control.Monad.Catch
import Control.Monad.Except
import Crypto.Hash
import Data.Aeson as Aeson
import Data.Id
import Data.Misc ((<$$>))
import Data.Range
import Data.String.Conversions
import Data.Text.Encoding
import Data.Time
import Data.UUID as UUID
import Galley.Types.Teams    as Galley
import Network.URI
import OpenSSL.Random (randBytes)
import SAML2.WebSSO (IdPId)
import Servant
import Servant.API.Generic
import Spar.API.Util
import Spar.App (Spar, Env, wrapMonadClient, sparCtxOpts, createUser_, wrapMonadClient)
import Spar.Error
import Spar.Intra.Galley
import Spar.Scim.Types
import Spar.Types
import Text.Email.Validate

import qualified Data.ByteString.Base64 as ES
import qualified Data.Text    as Text
import qualified Data.UUID.V4 as UUID
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data    as Data
import qualified Spar.Intra.Brig as Intra.Brig
import qualified URI.ByteString as URIBS

-- FUTUREWORK: these imports are not very handy.  split up Spar.Scim into
-- Spar.Scim.{Core,User,Group} to avoid at least some of the hscim name clashes?
import qualified Web.Scim.Class.Auth              as Scim.Class.Auth
import qualified Web.Scim.Class.Group             as Scim.Class.Group
import qualified Web.Scim.Class.User              as Scim.Class.User
import qualified Web.Scim.Filter                  as Scim
import qualified Web.Scim.Handler                 as Scim
import qualified Web.Scim.Schema.Common           as Scim
import qualified Web.Scim.Schema.Error            as Scim
import qualified Web.Scim.Schema.ListResponse     as Scim
import qualified Web.Scim.Schema.Meta             as Scim
import qualified Web.Scim.Schema.ResourceType     as Scim
import qualified Web.Scim.Server                  as Scim

import qualified Web.Scim.Schema.User             as Scim.User
import qualified Web.Scim.Schema.User.Email       as Scim.User
import qualified Web.Scim.Schema.User.Name        as Scim.User
import qualified Web.Scim.Schema.User.Phone       as Scim.User

import qualified Web.Scim.Capabilities.MetaSchema as Scim.Meta

import qualified Web.Scim.Schema.Common           as Scim.Common

-- | SCIM config for our server.
--
-- TODO: the 'Scim.Meta.empty' configuration claims that we don't support filters, but we
-- actually do; it's a bug in hscim
configuration :: Scim.Meta.Configuration
configuration = Scim.Meta.empty

apiScim :: ServerT APIScim Spar
apiScim = hoistScim (toServant (Scim.siteServer configuration))
     :<|> apiScimToken
  where
    hoistScim = hoistServer (Proxy @(Scim.SiteAPI ScimToken))
                            (Scim.fromScimHandler fromError)
    fromError = throwError . SAML.CustomServant . Scim.scimToServantErr

----------------------------------------------------------------------------
-- UserDB

-- | Retrieve 'IdP' from 'ScimTokenInfo' and call 'validateScimUser''.
validateScimUser
  :: forall m. (m ~ Scim.ScimHandler Spar)
  => ScimTokenInfo -> Scim.User.User -> m ValidScimUser
validateScimUser ScimTokenInfo{stiIdP} user = do
    idp <- case stiIdP of
        Nothing -> Scim.throwScim $
          Scim.serverError "No IdP configured for the provisioning token"
        Just idp -> lift (wrapMonadClient (Data.getIdPConfig idp)) >>= \case
            Nothing -> Scim.throwScim $
              Scim.serverError "The IdP corresponding to the provisioning token \
                               \was not found"
            Just idpConfig -> pure idpConfig
    validateScimUser' (Just idp) user

-- | Map the SCIM data on the spar and brig schemata, and throw errors if the SCIM data does
-- not comply with the standard / our constraints. See also: 'ValidScimUser'.
--
-- __Mapped fields:__
--
--   * @userName@ is mapped to our 'userHandle'. If there is no handle, we use 'userId',
--     because having some unique @userName@ is a SCIM requirement.
--
--   * @name@ is left empty and is never stored, even when it's sent to us via SCIM.
--
--   * @displayName@ is mapped to our 'userName'.
--
--   * A mandatory @SAML.UserRef@ is derived from 'Scim.User.externalId' and the 'idpId'
--     (retrieved via SCIM token).
--
-- FUTUREWORK: We may need to make the SAML NameID type derived from the available SCIM data
-- configurable on a per-team basis in the future, to accomodate different legal uses of
-- externalId by different users.
--
-- __Emails and phone numbers:__ we'd like to ensure that only verified emails and phone
-- numbers end up in our database, and implementing verification requires design decisions
-- that we haven't made yet. We store them in our SCIM blobs, but don't syncronize them with
-- Brig.
--
-- See <https://github.com/wireapp/wire-server/pull/559#discussion_r247466760>
--
-- __Names:__ some systems like Okta require given name and family name to be present, but
-- it's a poor model for names, and in practice many other apps also ignore this model.
-- Leaving @name@ empty will prevent the confusion that might appear when somebody tries to
-- set @name@ to some value and the @displayName@ won't be affected by that change.
validateScimUser'
  :: forall m. (MonadError Scim.ScimError m)
  => Maybe IdP -> Scim.User.User -> m ValidScimUser
validateScimUser' Nothing _ =
    throwError $ Scim.serverError "SCIM users without SAML SSO are not supported"
validateScimUser' (Just idp) user = do
    let validateNameOrExtId :: Maybe Text -> m (Maybe Text)
        validateNameOrExtId mtxt = forM mtxt $ \txt ->
          case checkedEitherMsg @_ @1 @128 "displayName" txt of
            Right rtxt -> pure $ fromRange rtxt
            Left err -> throwError $ Scim.badRequest Scim.InvalidValue
              (Just ("displayName is not compliant: " <> Text.pack err))

    uref :: SAML.UserRef <- do
      msubjid <- SAML.opaqueNameID <$$>
        validateNameOrExtId (Scim.User.externalId user)

      case msubjid of
        Just subj -> do
            pure $ SAML.UserRef (idp ^. SAML.idpMetadata . SAML.edIssuer) subj
        Nothing -> throwError $
            Scim.badRequest Scim.InvalidValue (Just "externalId is required for SAML users")

    handl <- case parseHandle (Scim.User.userName user) of
      Just x -> pure x
      Nothing -> throwError $
        Scim.badRequest Scim.InvalidValue (Just "userName is not compliant")

    -- We check the name for validity, but only if it's present
    mbName <- Name <$$> validateNameOrExtId (Scim.User.displayName user)

    -- NB: We assume that checking that the user does _not_ exist has
    -- already been done before -- the hscim library check does a 'get'
    -- before a 'create'

    pure $ ValidScimUser user uref handl mbName

-- | We only allow SCIM users that authenticate via SAML. (This is by no means necessary,
-- though. It can be relaxed to allow creating users with password authentication if that is a
-- requirement.)
createValidScimUser
  :: forall m. (m ~ Scim.ScimHandler Spar)
  => ValidScimUser -> m Scim.Class.User.StoredUser
createValidScimUser (ValidScimUser user uref handl mbName) = do
    -- This UserId will be used both for scim user in spar and for brig.
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
  => ScimTokenInfo -> Text -> ValidScimUser -> m Scim.Class.User.StoredUser
updateValidScimUser tokinfo uidText newScimUser = do

    -- TODO: currently the types in @hscim@ are constructed in such a way that
    -- 'Scim.User.User' doesn't contain an ID, only 'Scim.Class.User.StoredUser'
    -- does. @fisx believes that this situation could be improved (see
    -- <https://github.com/wireapp/wire-server/pull/559#discussion_r247392882>).
    --
    -- If 'Scim.User.User' and 'ValidScimUser' did contain the user ID, we wouldn't need
    -- 'uidText' in this function -- or we could at least check in hscim that the ID in the
    -- user object matches the ID in the path.

    -- TODO: how do we get this safe w.r.t. race conditions / crashes?

    -- construct old and new user values with metadata.
    uid :: UserId <- parseUid uidText
    oldScimStoredUser :: Scim.Class.User.StoredUser
      <- let err = Scim.throwScim $ Scim.notFound "user" uidText
         in maybe err pure =<< Scim.Class.User.get tokinfo uidText

    if Scim.value (Scim.thing oldScimStoredUser) == (newScimUser ^. vsuUser)
      then pure oldScimStoredUser
      else do
        newScimStoredUser :: Scim.Class.User.StoredUser
          <- lift $ updScimStoredUser (newScimUser ^. vsuUser) oldScimStoredUser

        -- update 'SAML.UserRef'
        let uref = newScimUser ^. vsuSAMLUserRef
        lift . wrapMonadClient $ Data.insertUser uref uid  -- on spar
        bindok <- lift $ Intra.Brig.bindUser uid uref  -- on brig
        unless bindok . Scim.throwScim $
          Scim.serverError "Failed to update SAML UserRef (no such UserId? duplicate Handle?)"

        maybe (pure ()) (lift . Intra.Brig.setName uid) $ newScimUser ^. vsuName
        lift . Intra.Brig.setHandle uid $ newScimUser ^. vsuHandle

        -- store new user value to scim_user table (spar). (this must happen last, so in case
        -- of crash the client can repeat the operation and it won't be considered a noop.)
        lift . wrapMonadClient $ Data.insertScimUser uid newScimStoredUser

        pure newScimStoredUser


toScimStoredUser
  :: forall m. (SAML.HasNow m, MonadReader Env m)
  => UserId -> Scim.User.User -> m Scim.Class.User.StoredUser
toScimStoredUser uid usr = do
  now <- SAML.getNow
  baseuri <- asks $ derivedOptsScimBaseURI . derivedOpts . sparCtxOpts
  pure $ toScimStoredUser' now baseuri uid usr

toScimStoredUser'
  :: HasCallStack
  => SAML.Time -> URIBS.URI -> UserId -> Scim.User.User -> Scim.Class.User.StoredUser
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
  => Scim.User.User -> Scim.Class.User.StoredUser -> m Scim.Class.User.StoredUser
updScimStoredUser usr storedusr = do
  now <- SAML.getNow
  pure $ updScimStoredUser' now usr storedusr

updScimStoredUser'
  :: SAML.Time
  -> Scim.User.User
  -> Scim.Class.User.StoredUser
  -> Scim.Class.User.StoredUser
updScimStoredUser' (SAML.Time moddate) usr (Scim.WithMeta meta (Scim.WithId scimuid _)) =
    Scim.WithMeta meta' (Scim.WithId scimuid usr)
  where
    meta' = meta
      { Scim.lastModified = moddate
      , Scim.version = calculateVersion scimuid usr
      }

parseUid
  :: forall m m'. (m ~ Scim.ScimHandler m', Monad m')
  => Text -> m UserId
parseUid uidText = maybe err pure $ readMaybe (Text.unpack uidText)
  where err = Scim.throwScim $ Scim.notFound "user" uidText

-- | Calculate resource version (currently only for 'Scim.User.User's).
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
  -> Scim.User.User
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
toScimName :: Name -> Scim.User.Name
toScimName (Name name) =
  Scim.User.Name
    { Scim.User.formatted = Just name
    , Scim.User.givenName = Just first
    , Scim.User.familyName = if Text.null rest then Nothing else Just rest
    , Scim.User.middleName = Nothing
    , Scim.User.honorificPrefix = Nothing
    , Scim.User.honorificSuffix = Nothing
    }
  where
    (first, Text.drop 1 -> rest) = Text.breakOn " " name

-- | Convert from the Wire phone type to the SCIM phone type.
toScimPhone :: Phone -> Scim.User.Phone
toScimPhone (Phone phone) =
  Scim.User.Phone
    { Scim.User.typ = Nothing
    , Scim.User.value = Just phone
    }

-- | Convert from the Wire email type to the SCIM email type.
toScimEmail :: Email -> Scim.User.Email
toScimEmail (Email eLocal eDomain) =
  Scim.User.Email
    { Scim.User.typ = Nothing
    , Scim.User.value = Scim.User.EmailAddress2
        (unsafeEmailAddress (encodeUtf8 eLocal) (encodeUtf8 eDomain))
    , Scim.User.primary = Just True
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

instance Scim.Class.User.UserDB Spar where
  -- | List all users, possibly filtered by some predicate.
  list :: ScimTokenInfo
       -> Maybe Scim.Filter
       -> Scim.ScimHandler Spar (Scim.ListResponse Scim.Class.User.StoredUser)
  list ScimTokenInfo{stiTeam} mbFilter = do
    members <- lift $ getTeamMembers stiTeam
    brigusers :: [User]
      <- filter (not . userDeleted) <$>
         lift (Intra.Brig.getUsers ((^. Galley.userId) <$> members))
    scimusers :: [Scim.Class.User.StoredUser]
      <- lift . wrapMonadClient . Data.getScimUsers $ Brig.userId <$> brigusers
    let check user = case mbFilter of
          Nothing -> pure True
          Just filter_ ->
            let user' = Scim.Common.value (Scim.thing user)
            in case Scim.filterUser filter_ user' of
                 Right res -> pure res
                 Left err  -> Scim.throwScim $
                   Scim.badRequest Scim.InvalidFilter (Just err)
    -- FUTUREWORK: once bigger teams arrive, we should have pagination here.
    Scim.fromList <$> filterM check scimusers

  -- | Get a single user by its ID.
  get :: ScimTokenInfo
      -> Text
      -> Scim.ScimHandler Spar (Maybe Scim.Class.User.StoredUser)
  get ScimTokenInfo{stiTeam} uidText = do
    uid <- parseUid uidText
    mbBrigUser <- lift (Intra.Brig.getUser uid)
    if isJust mbBrigUser && (userTeam =<< mbBrigUser) == Just stiTeam
      then lift . wrapMonadClient . Data.getScimUser $ uid
      else pure Nothing

  -- | Create a new user.
  create :: ScimTokenInfo
         -> Scim.User.User
         -> Scim.ScimHandler Spar Scim.Class.User.StoredUser
  create tokinfo user =
    createValidScimUser =<< validateScimUser tokinfo user

  update :: ScimTokenInfo
         -> Text
         -> Scim.User.User
         -> Scim.ScimHandler Spar Scim.Class.User.StoredUser
  update tokinfo uidText newScimUser =
    updateValidScimUser tokinfo uidText =<< validateScimUser tokinfo newScimUser

  delete :: ScimTokenInfo -> Text -> Scim.ScimHandler Spar Bool
  delete _ _ =
      Scim.throwScim $ Scim.serverError "User delete is not implemented yet"  -- TODO

  getMeta :: ScimTokenInfo -> Scim.ScimHandler Spar Scim.Meta
  getMeta _ =
      Scim.throwScim $ Scim.serverError "User getMeta is not implemented yet"  -- TODO

----------------------------------------------------------------------------
-- GroupDB

instance Scim.Class.Group.GroupDB Spar where
  -- TODO

----------------------------------------------------------------------------
-- AuthDB

instance Scim.Class.Auth.AuthDB Spar where
  type AuthData Spar = ScimToken
  type AuthInfo Spar = ScimTokenInfo

  authCheck Nothing =
      Scim.throwScim (Scim.unauthorized "Token not provided")
  authCheck (Just token) =
      maybe (Scim.throwScim (Scim.unauthorized "Invalid token")) pure =<<
      lift (wrapMonadClient (Data.lookupScimToken token))

-- TODO: don't forget to delete the tokens when the team is deleted

----------------------------------------------------------------------------
-- API for manipulating authentication tokens

apiScimToken :: ServerT APIScimToken Spar
apiScimToken
     = createScimToken
  :<|> deleteScimToken
  :<|> listScimTokens

----------------------------------------------------------------------------
-- Handlers

createScimToken :: Maybe UserId -> CreateScimToken -> Spar CreateScimTokenResponse
createScimToken zusr CreateScimToken{..} = do
    let descr = createScimTokenDescr
    -- Don't enable this endpoint until SCIM is ready.
    _ <- error "Creating SCIM tokens is not supported yet."
    teamid <- Intra.Brig.getZUsrOwnedTeam zusr
    tokenNumber <- fmap length $ wrapMonadClient $ Data.getScimTokens teamid
    maxTokens <- asks (maxScimTokens . sparCtxOpts)
    unless (tokenNumber < maxTokens) $
        throwSpar SparProvisioningTokenLimitReached
    idps <- wrapMonadClient $ Data.getIdPConfigsByTeam teamid
    case idps of
        [idp] -> do
            -- TODO: sign tokens. Also, we might want to use zauth, if we can / if
            -- it makes sense semantically
            token <- ScimToken . cs . ES.encode <$> liftIO (randBytes 32)
            tokenid <- randomId
            now <- liftIO getCurrentTime
            let idpid = idp ^. SAML.idpId
                info = ScimTokenInfo
                    { stiId        = tokenid
                    , stiTeam      = teamid
                    , stiCreatedAt = now
                    , stiIdP       = Just idpid
                    , stiDescr     = descr
                    }
            wrapMonadClient $ Data.insertScimToken token info
            pure $ CreateScimTokenResponse token info
        [] -> throwSpar $ SparProvisioningNoSingleIdP
                "SCIM tokens can only be created for a team with an IdP, \
                \but none are found"
        _  -> throwSpar $ SparProvisioningNoSingleIdP
                "SCIM tokens can only be created for a team with exactly one IdP, \
                \but more are found"

deleteScimToken :: Maybe UserId -> ScimTokenId -> Spar NoContent
deleteScimToken zusr tokenid = do
    teamid <- Intra.Brig.getZUsrOwnedTeam zusr
    wrapMonadClient $ Data.deleteScimToken teamid tokenid
    pure NoContent

listScimTokens :: Maybe UserId -> Spar ScimTokenList
listScimTokens zusr = do
    teamid <- Intra.Brig.getZUsrOwnedTeam zusr
    ScimTokenList <$> wrapMonadClient (Data.getScimTokens teamid)

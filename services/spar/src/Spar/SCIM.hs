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

-- TODO remove (orphans can be avoided by only implementing functions here, and gathering them in
-- the instance near the Spar type.)
{-# OPTIONS_GHC
    -Wno-missing-methods
    -Wno-unused-imports
    -Wno-orphans
    -Wunused-top-binds
  #-}

-- | An implementation of the SCIM API for doing bulk operations with users.
--
-- See <https://en.wikipedia.org/wiki/System_for_Cross-domain_Identity_Management>
module Spar.SCIM
  (
  -- * The API
    APIScim
  , apiScim
  -- ** Request and response types
  , CreateScimToken(..)
  , CreateScimTokenResponse(..)
  , ScimTokenList(..)

  -- * The mapping between Wire and SCIM users
  , mapScimToBrig
  , mapBrigToScim

  -- * testing
  , toSCIMStoredUser'
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
import Spar.App (Spar, Env, wrapMonadClient, sparCtxOpts, createUser', wrapMonadClient)
import Spar.Error
import Spar.Intra.Galley
import Spar.SCIM.Types
import Spar.Types
import Text.Email.Validate

import qualified Data.ByteString.Base64 as ES
import qualified Data.Text    as Text
import qualified Data.UUID.V4 as UUID
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data    as Data
import qualified Spar.Intra.Brig as Intra.Brig
import qualified URI.ByteString as URIBS

import qualified Web.SCIM.Class.Auth              as SCIM.Class.Auth
import qualified Web.SCIM.Class.Group             as SCIM.Class.Group
import qualified Web.SCIM.Class.User              as SCIM.Class.User
import qualified Web.SCIM.Filter                  as SCIM
import qualified Web.SCIM.Handler                 as SCIM
import qualified Web.SCIM.Schema.Common           as SCIM
import qualified Web.SCIM.Schema.Error            as SCIM
import qualified Web.SCIM.Schema.ListResponse     as SCIM
import qualified Web.SCIM.Schema.Meta             as SCIM
import qualified Web.SCIM.Schema.ResourceType     as SCIM
import qualified Web.SCIM.Server                  as SCIM

import qualified Web.SCIM.Schema.User             as SCIM.User
import qualified Web.SCIM.Schema.User.Email       as SCIM.User
import qualified Web.SCIM.Schema.User.Name        as SCIM.User
import qualified Web.SCIM.Schema.User.Phone       as SCIM.User

import qualified Web.SCIM.Capabilities.MetaSchema as SCIM.Meta

import qualified Web.SCIM.Schema.Common           as SCIM.Common

-- | SCIM config for our server.
--
-- TODO: the 'SCIM.Meta.empty' configuration claims that we don't support
-- filters, but we actually do; it's a bug in hscim
configuration :: SCIM.Meta.Configuration
configuration = SCIM.Meta.empty

apiScim :: ServerT APIScim Spar
apiScim = hoistSCIM (toServant (SCIM.siteServer configuration))
     :<|> apiScimToken
  where
    hoistSCIM = hoistServer (Proxy @(SCIM.SiteAPI ScimToken))
                            (SCIM.fromSCIMHandler fromError)
    fromError = throwError . SAML.CustomServant . SCIM.scimToServantErr

----------------------------------------------------------------------------
-- UserDB

-- | Mapping from SCIM user to brig user.  This is ok to use in production, but
-- it doesn't fit the brig rest api.  So we duplicate the behavior of this
-- function in 'createValidSCIMUser' below and use this function (a) as
-- executable documentation and (b) in the tests to make sure it's valid.
-- The function takes an existing brig user that is updated in *some* fields,
-- and left intact in others.
--
--   * @userName@ is mapped to our 'userHandle'. If there is no handle, we
--     use 'userId', because having some unique @userName@ is a SCIM
--     requirement.
--
--   * @name@ is left empty and is never stored, even when it's sent to us
--     via SCIM.
--
--   * @displayName@ is mapped to our 'userName'.
--
-- We don't handle emails and phone numbers for now, because we'd like to
-- ensure that only verified emails and phone numbers end up in our
-- database, and implementing verification requires design decisions that we
-- haven't made yet.
--
-- Regarding names: some systems like Okta require given name and family
-- name to be present, but it's a poor model for names, and in practice many
-- other apps also ignore this model. Leaving @name@ empty will prevent the
-- confusion that might appear when somebody tries to set @name@ to some
-- value and the @displayName@ won't be affected by that change.
mapScimToBrig :: SCIM.Class.User.StoredUser -> Brig.User -> Brig.User
mapScimToBrig = undefined

mapBrigToScim :: Brig.User -> SCIM.Class.User.StoredUser -> SCIM.Class.User.StoredUser
mapBrigToScim = undefined

-- | Map the SCIM data on the spar and brig schemata, and throw errors if the SCIM data does not
-- comply with the standard / our constraints.
validateSCIMUser
  :: forall m. (MonadError SCIM.SCIMError m)
  => SCIM.User.User -> m ValidSCIMUser
validateSCIMUser user = do
    let validateNameOrExtId :: Maybe Text -> m (Maybe Text)
        validateNameOrExtId mtxt = forM mtxt $ \txt ->
          case checkedEitherMsg @_ @1 @128 "displayName" txt of
            Right rtxt -> pure $ fromRange rtxt
            Left err -> throwError $ SCIM.badRequest SCIM.InvalidValue
              (Just ("displayName is not compliant: " <> Text.pack err))

    -- TODO: Assume that externalID is the subjectID, let's figure out how
    -- to extract that later.  We may need to make this configurable on a
    -- per-team basis to correctly create subject IDs for either email or
    -- persistent names, or possibly others.
    samlSubjectId <- SAML.opaqueNameID <$$>
      validateNameOrExtId (SCIM.User.externalId user)

    handl <- case parseHandle (SCIM.User.userName user) of
      Just x -> pure x
      Nothing -> throwError $
        SCIM.badRequest SCIM.InvalidValue (Just "userName is not compliant")

    -- We check the name for validity, but only if it's present
    mbName <- Name <$$> validateNameOrExtId (SCIM.User.displayName user)

    -- NB: We assume that checking that the user does _not_ exist has
    -- already been done before -- the hscim library check does a 'get'
    -- before a 'create'

    pure $ ValidSCIMUser user samlSubjectId handl mbName

-- | We only allow SCIM users that authenticate via SAML.  (This is by no means necessary, though.
-- It can be relaxed to allow creating users with password authentication if that is a requirement.)
createValidSCIMUser
  :: forall m. (m ~ SCIM.SCIMHandler Spar)
  => ScimTokenInfo -> ValidSCIMUser -> m SCIM.Class.User.StoredUser
createValidSCIMUser ScimTokenInfo{stiIdP} (ValidSCIMUser user samlSubjectId handl mbName) = do
    uref <- case (stiIdP, samlSubjectId) of
        (Nothing, _) -> SCIM.throwSCIM $
          SCIM.serverError "No IdP configured for the provisioning token"
        (_, Nothing) -> SCIM.throwSCIM $
          SCIM.badRequest SCIM.InvalidValue (Just "externalId is required for SAML users")
        (Just idp, Just subj) -> lift (wrapMonadClient (Data.getIdPConfig idp)) >>= \case
            Nothing -> SCIM.throwSCIM $
              SCIM.serverError "The IdP corresponding to the provisioning token \
                               \was not found"
            Just idpConfig -> pure $
              SAML.UserRef (idpConfig ^. SAML.idpMetadata . SAML.edIssuer) subj

    -- This UserId will be used both for scim user in spar and for brig.
    buid <- Id <$> liftIO UUID.nextRandom
    -- Create SCIM user here in spar.
    storedUser <- lift $ toSCIMStoredUser buid user
    lift . wrapMonadClient $ Data.insertScimUser buid storedUser
    -- Create SAML user here in spar, which in turn creates a brig user.
    lift $ createUser' buid uref mbName
    -- Set user handle on brig (which can't be done during user creation).
    lift $ Intra.Brig.setHandle buid handl

    pure storedUser

toSCIMStoredUser
  :: forall m. (SAML.HasNow m, MonadReader Env m)
  => UserId -> SCIM.User.User -> m SCIM.Class.User.StoredUser
toSCIMStoredUser uid usr = do
  now <- SAML.getNow
  baseuri <- asks $ derivedOptsScimBaseURI . derivedOpts . sparCtxOpts
  pure $ toSCIMStoredUser' now baseuri uid usr

toSCIMStoredUser'
  :: SAML.Time -> URIBS.URI -> UserId -> SCIM.User.User -> SCIM.Class.User.StoredUser
toSCIMStoredUser' (SAML.Time now) baseuri (idToText -> uid) usr = SCIM.WithMeta meta thing
  where
    mkLocation :: String -> URI
    mkLocation pathSuffix = convURI $ baseuri SAML.=/ cs pathSuffix
      where
        convURI uri = fromMaybe err . parseURI . cs . URIBS.serializeURIRef' $ uri
          where err = error $ "internal error: " <> show uri

    thing = SCIM.WithId uid usr
    thingHash = hashlazy (Aeson.encode thing) :: Digest SHA256

    meta = SCIM.Meta
      { SCIM.resourceType = SCIM.UserResource
      , SCIM.created = now
      , SCIM.lastModified = now
      , SCIM.version = SCIM.Strong (Text.pack (show thingHash))
        -- TODO: it looks like (a) we need to add this to the HTTP header, and (b) this should be a
        -- version (which is increasing over time), not a hash (which is not).
        -- https://tools.ietf.org/html/rfc7644#section-3.14
      , SCIM.location = SCIM.URI . mkLocation $ "/Users/" <> cs uid
      }

parseUid
  :: forall m m'. (m ~ SCIM.SCIMHandler m', Monad m')
  => Text -> m UserId
parseUid uidText = maybe err pure $ readMaybe (Text.unpack uidText)
  where err = SCIM.throwSCIM $ SCIM.notFound "user" uidText


{- TODO: might be useful later.
~~~~~~~~~~~~~~~~~~~~~~~~~

-- | Parse a name from a user profile into an SCIM name (Okta wants given
-- name and last name, so we break our names up to satisfy Okta).
--
-- TODO: use the same algorithm as Wire clients use.
toSCIMName :: Name -> SCIM.User.Name
toSCIMName (Name name) =
  SCIM.User.Name
    { SCIM.User.formatted = Just name
    , SCIM.User.givenName = Just first
    , SCIM.User.familyName = if Text.null rest then Nothing else Just rest
    , SCIM.User.middleName = Nothing
    , SCIM.User.honorificPrefix = Nothing
    , SCIM.User.honorificSuffix = Nothing
    }
  where
    (first, Text.drop 1 -> rest) = Text.breakOn " " name

-- | Convert from the Wire phone type to the SCIM phone type.
toSCIMPhone :: Phone -> SCIM.User.Phone
toSCIMPhone (Phone phone) =
  SCIM.User.Phone
    { SCIM.User.typ = Nothing
    , SCIM.User.value = Just phone
    }

-- | Convert from the Wire email type to the SCIM email type.
toSCIMEmail :: Email -> SCIM.User.Email
toSCIMEmail (Email eLocal eDomain) =
  SCIM.User.Email
    { SCIM.User.typ = Nothing
    , SCIM.User.value = SCIM.User.EmailAddress2
        (unsafeEmailAddress (encodeUtf8 eLocal) (encodeUtf8 eDomain))
    , SCIM.User.primary = Just True
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
--    the 'SCIMError' type. This might or might not be important, depending
--    on what is expected by apps that use the SCIM interface.
--
-- 2. We want generic error descriptions in response bodies, while still
--    logging nice error messages internally.

instance SCIM.Class.User.UserDB Spar where
  -- | List all users, possibly filtered by some predicate.
  list :: ScimTokenInfo
       -> Maybe SCIM.Filter
       -> SCIM.SCIMHandler Spar (SCIM.ListResponse SCIM.Class.User.StoredUser)
  list ScimTokenInfo{stiTeam} mbFilter = do
    members <- lift $ getTeamMembers stiTeam
    brigusers :: [User]
      <- filter (not . userDeleted) <$>
         lift (Intra.Brig.getUsers ((^. Galley.userId) <$> members))
    scimusers :: [SCIM.Class.User.StoredUser]
      <- lift . wrapMonadClient . Data.getScimUsers $ Brig.userId <$> brigusers
    let check user = case mbFilter of
          Nothing -> pure True
          Just filter_ ->
            let user' = SCIM.Common.value (SCIM.thing user)
            in case SCIM.filterUser filter_ user' of
                 Right res -> pure res
                 Left err  -> SCIM.throwSCIM $
                   SCIM.badRequest SCIM.InvalidFilter (Just err)
    -- FUTUREWORK: once bigger teams arrive, we should have pagination here.
    SCIM.fromList <$> filterM check scimusers

  -- | Get a single user by its ID.
  get :: ScimTokenInfo
      -> Text
      -> SCIM.SCIMHandler Spar (Maybe SCIM.Class.User.StoredUser)
  get ScimTokenInfo{stiTeam} uidText = do
    uid <- parseUid uidText
    briguser <- lift $ Intra.Brig.getUser uid
    scimuser <- maybe (pure Nothing)
                      (lift . wrapMonadClient . Data.getScimUser . Brig.userId)
                      briguser
    if ( userTeam (fromJust briguser) /= Just stiTeam ||
         userDeleted (fromJust briguser)
       )
      then pure Nothing
      else pure scimuser

  -- | Create a new user.
  create :: ScimTokenInfo
         -> SCIM.User.User
         -> SCIM.SCIMHandler Spar SCIM.Class.User.StoredUser
  create tokinfo user = createValidSCIMUser tokinfo =<< validateSCIMUser user

  update :: ScimTokenInfo
         -> Text
         -> SCIM.User.User
         -> SCIM.SCIMHandler Spar SCIM.Class.User.StoredUser
  update _ _ _ =
      SCIM.throwSCIM $ SCIM.serverError "User update is not implemented yet"  -- TODO

  delete :: ScimTokenInfo -> Text -> SCIM.SCIMHandler Spar Bool
  delete _ _ =
      SCIM.throwSCIM $ SCIM.serverError "User delete is not implemented yet"  -- TODO

  getMeta :: ScimTokenInfo -> SCIM.SCIMHandler Spar SCIM.Meta
  getMeta _ =
      SCIM.throwSCIM $ SCIM.serverError "User getMeta is not implemented yet"  -- TODO

----------------------------------------------------------------------------
-- GroupDB

instance SCIM.Class.Group.GroupDB Spar where
  -- TODO

----------------------------------------------------------------------------
-- AuthDB

instance SCIM.Class.Auth.AuthDB Spar where
  type AuthData Spar = ScimToken
  type AuthInfo Spar = ScimTokenInfo

  authCheck Nothing =
      SCIM.throwSCIM (SCIM.unauthorized "Token not provided")
  authCheck (Just token) =
      maybe (SCIM.throwSCIM (SCIM.unauthorized "Invalid token")) pure =<<
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

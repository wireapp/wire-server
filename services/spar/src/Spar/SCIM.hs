{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- TODO remove
{-# OPTIONS_GHC
    -Wno-missing-methods
    -Wno-unused-imports
    -Wno-orphans
    -Wno-unused-top-binds
  #-}

-- | An implementation of the SCIM API for doing bulk operations with users.
--
-- See <https://en.wikipedia.org/wiki/System_for_Cross-domain_Identity_Management>
module Spar.SCIM
  ( SCIM.SiteAPI
  , scimApi

  -- * The mapping between Wire and SCIM users
  -- $mapping
  ) where

import Imports
import Brig.Types.User       as Brig
import Galley.Types.Teams    as Galley
import Control.Monad.Except
import Control.Monad.Catch
import Control.Exception
import Control.Lens
import Data.Id
import Data.Range
import Servant
import Spar.App (Spar, wrapMonadClient)
import Spar.Error
import Spar.Types
import Spar.Intra.Brig
import Spar.Intra.Galley
import Data.UUID as UUID
import Crypto.Hash
import Data.Time
import Data.Text.Encoding
import Data.Aeson as Aeson
import Text.Email.Validate
import Servant.Generic

import qualified Data.Text    as Text
import qualified Data.UUID.V4 as UUID
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data    as Data

import qualified Web.SCIM.Class.User              as SCIM
import qualified Web.SCIM.Class.Group             as SCIM
import qualified Web.SCIM.Class.Auth              as SCIM
import qualified Web.SCIM.Server                  as SCIM
import qualified Web.SCIM.Handler                 as SCIM
import qualified Web.SCIM.Filter                  as SCIM
import qualified Web.SCIM.Schema.Common           as SCIM
import qualified Web.SCIM.Schema.Meta             as SCIM
import qualified Web.SCIM.Schema.ResourceType     as SCIM
import qualified Web.SCIM.Schema.ListResponse     as SCIM
import qualified Web.SCIM.Schema.Error            as SCIM

import qualified Web.SCIM.Schema.User             as SCIM.User
import qualified Web.SCIM.Schema.User.Email       as SCIM.User
import qualified Web.SCIM.Schema.User.Phone       as SCIM.User
import qualified Web.SCIM.Schema.User.Name        as SCIM.User

import qualified Web.SCIM.Capabilities.MetaSchema as SCIM.Meta

import qualified Web.SCIM.Schema.Common           as SCIM.Common

-- | SCIM config for our server.
--
-- TODO: the 'SCIM.Meta.empty' configuration claims that we don't support
-- filters, but we actually do; it's a bug in hscim
configuration :: SCIM.Meta.Configuration
configuration = SCIM.Meta.empty

type ScimApi = SCIM.SiteAPI ScimToken

scimApi :: ServerT ScimApi Spar
scimApi = hoistSCIM (toServant (SCIM.siteServer configuration))
  where
    hoistSCIM = hoistServer (Proxy @ScimApi) (SCIM.fromSCIMHandler fromError)
    fromError = throwError . SAML.CustomServant . SCIM.scimToServantErr

----------------------------------------------------------------------------
-- UserDB

-- $mapping
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

-- | Expose a Wire user as an SCIM user.
toSCIMUser :: User -> SCIM.StoredUser
toSCIMUser user = SCIM.WithMeta meta thing
  where
    -- User ID in text format
    idText = idToText (Brig.userId user)
    -- The representation of the user, without the meta information
    thing = SCIM.WithId idText $ SCIM.User.empty
      { SCIM.User.userName = maybe idText fromHandle (userHandle user)
      , SCIM.User.name = Just emptySCIMName
      , SCIM.User.displayName = Just (fromName (userName user))
      }
    -- The hash of the user representation (used as a version, i.e. ETag)
    thingHash = hashlazy (Aeson.encode thing) :: Digest SHA256
    -- Meta-info about the user
    meta = SCIM.Meta
      { SCIM.resourceType = SCIM.UserResource
      , SCIM.created = testDate
      , SCIM.lastModified = testDate
      , SCIM.version = SCIM.Strong (Text.pack (show thingHash))
      -- TODO: The location should be /Users/<uid>. It might also have to
      -- include the baseurl of our server -- this has to be checked.
      , SCIM.location = SCIM.URI $ URI "https://TODO" Nothing "" "" ""
      }

-- 2018-01-01 00:00
--
-- TODO: real dates!
testDate :: UTCTime
testDate = UTCTime
  { utctDay = ModifiedJulianDay 58119
  , utctDayTime = 0
  }

emptySCIMName :: SCIM.User.Name
emptySCIMName =
  SCIM.User.Name
    { SCIM.User.formatted = Nothing
    , SCIM.User.givenName = Just ""
    , SCIM.User.familyName = Just ""
    , SCIM.User.middleName = Nothing
    , SCIM.User.honorificPrefix = Nothing
    , SCIM.User.honorificSuffix = Nothing
    }

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

instance SCIM.UserDB Spar where
  -- | List all users, possibly filtered by some predicate.
  list (tid, _) mbFilter = do
    members <- lift $ getTeamMembers tid
    users <- forM members $ \member ->
      lift (getUser (member ^. Galley.userId)) >>= \case
        Just user -> pure (toSCIMUser user)
        Nothing -> SCIM.throwSCIM $
          SCIM.serverError "Database is inconsistent"
    let check user = case mbFilter of
          Nothing -> pure True
          Just filter_ ->
            let user' = SCIM.Common.value (SCIM.thing user)
            in case SCIM.filterUser filter_ user' of
                 Right res -> pure res
                 Left err  -> SCIM.throwSCIM $
                   SCIM.badRequest SCIM.InvalidFilter (Just err)
    -- TODO: once bigger teams arrive, we should have pagination here.
    SCIM.fromList <$> filterM check users

  -- | Get a single user by its ID.
  get (tid, _) uidText = do
    uid <- case readMaybe (Text.unpack uidText) of
      Just u -> pure u
      Nothing -> SCIM.throwSCIM $
        SCIM.notFound "user" uidText
    lift (getUser uid) >>= traverse (\user -> do
      when (userTeam user /= Just tid) $ SCIM.throwSCIM $
        SCIM.notFound "user" (idToText uid)
      pure (toSCIMUser user))

  -- | Create a new user.
  create (tid, mbIdp) user = do
    extId <- case SCIM.User.externalId user of
      Just x -> pure x
      Nothing -> SCIM.throwSCIM $
        SCIM.badRequest SCIM.InvalidValue (Just "externalId is required")
    handl <- case parseHandle (SCIM.User.userName user) of
      Just x -> pure x
      Nothing -> SCIM.throwSCIM $
        SCIM.badRequest SCIM.InvalidValue (Just "userName is not compliant")

    -- We check the name for validity, but only if it's present
    mbName <- forM (SCIM.User.displayName user) $ \n ->
      case checkedEitherMsg @_ @1 @128 "displayName" n of
        Right x -> pure $ Name (fromRange x)
        Left err -> SCIM.throwSCIM $
          SCIM.badRequest
            SCIM.InvalidValue
            (Just ("displayName is not compliant: " <> Text.pack err))
    -- NB: We assume that checking that the user does _not_ exist has
    -- already been done before -- the hscim library check does a 'get'
    -- before a 'create'

    buid <- Id <$> liftIO UUID.nextRandom
    -- TODO: Assume that externalID is the subjectID, let's figure out how
    -- to extract that later
    -- TODO: when the issuer is deleted, the token still remains, so we can
    -- fail here
    issuer <- case mbIdp of
        Nothing -> error "No IdP configured for the provisioning token"
        Just idp -> lift (wrapMonadClient (Data.getIdPConfig idp)) >>= \case
            Nothing -> error "IdP not found"
            Just idpConfig -> pure (idpConfig ^. SAML.idpMetadata . SAML.edIssuer)
    let uref = SAML.UserRef issuer (SAML.opaqueNameID extId)

    -- TODO: Adding a handle should be done _DURING_ the creation
    lift $ do
      _ <- createUser uref buid tid mbName
      setHandle buid handl

    maybe (error "How can there be no user?") (pure . toSCIMUser) =<<
      lift (getUser buid)

  -- update   :: TeamId -> UserId -> User -> m StoredUser
  -- patch    :: TeamId -> UserId -> m StoredUser
  -- delete   :: TeamId -> UserId -> m Bool  -- ^ Return 'False' if the group didn't exist
  -- getMeta  :: TeamId -> m Meta

----------------------------------------------------------------------------
-- GroupDB

instance SCIM.GroupDB Spar where
  -- TODO

----------------------------------------------------------------------------
-- AuthDB

instance SCIM.AuthDB Spar where
  type AuthData Spar = ScimToken
  type AuthInfo Spar = (TeamId, Maybe SAML.IdPId)

  authCheck Nothing =
      SCIM.throwSCIM (SCIM.unauthorized "Token not provided")
  authCheck (Just token) =
      maybe (SCIM.throwSCIM (SCIM.unauthorized "Invalid token")) pure =<<
      lift (wrapMonadClient (Data.lookupScimToken token))

-- TODO: don't forget to delete the tokens when the team is deleted

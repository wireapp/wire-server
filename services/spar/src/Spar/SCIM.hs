{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

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
  ( API
  , api
  ) where

import Brig.Types.User       as Brig
import Galley.Types.Teams    as Galley
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Exception
import Data.Id
import Lens.Micro
import Servant
import Spar.App (Spar)
import Spar.Error
import Spar.Intra.Brig
import Spar.Intra.Galley
import Data.UUID as UUID
import Crypto.Hash
import Data.Time
import Data.Maybe
import Data.Text.Encoding
import Data.Aeson as Aeson
import Text.Email.Validate
import Servant.Generic

import qualified Data.Text    as Text
import qualified SAML2.WebSSO as SAML

import qualified Web.SCIM.Class.User              as SCIM
import qualified Web.SCIM.Class.Group             as SCIM
import qualified Web.SCIM.Class.Auth              as SCIM
import qualified Web.SCIM.Server                  as SCIM
import qualified Web.SCIM.Handler                 as SCIM
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

-- | SCIM config for our server.
--
-- TODO: the 'SCIM.Meta.empty' configuration claims that we don't support
-- filters, but we actually do; it's a bug in hscim
configuration :: SCIM.Meta.Configuration
configuration = SCIM.Meta.empty

type API = Capture "team" TeamId :> SCIM.SiteAPI

api :: ServerT API Spar
api tid = hoistSCIM (toServant (SCIM.siteServer configuration))
  where
    hoistSCIM =
      enter @(ServerT SCIM.SiteAPI (SCIM.SCIMHandler (ReaderT TeamId Spar)))
            @(SCIM.SCIMHandler (ReaderT TeamId Spar))
            @Spar
            @(ServerT SCIM.SiteAPI Spar)
        (NT (flip runReaderT tid . SCIM.fromSCIMHandler fromError))
    fromError = throwError . SAML.CustomServant . SCIM.scimToServantErr

----------------------------------------------------------------------------
-- UserDB

-- | Export a Wire user as an SCIM user.
toSCIMUser :: User -> SCIM.StoredUser
toSCIMUser user = SCIM.WithMeta meta thing
  where
    -- The representation of the user, without the meta information
    thing = SCIM.WithId (idToText (Brig.userId user)) $ SCIM.User.empty
      {
      -- TODO what should we do if the user doesn't have a handle? also,
      -- those handles are supposed to be unique and case-insensitive -- I'm
      -- not sure our handles satisfy those requirements
        SCIM.User.userName = maybe "" fromHandle (userHandle user)
      , SCIM.User.name = Just (toSCIMName (userName user))
      , SCIM.User.displayName = Just (fromName (userName user))
      , SCIM.User.emails = (:[]) . toSCIMEmail <$>
          (emailIdentity =<< userIdentity user)
      , SCIM.User.phoneNumbers = (:[]) . toSCIMPhone <$>
          (phoneIdentity =<< userIdentity user)
      -- , photos = <TODO profile pic here>
      }
    -- The hash of the user representation (used as an ETag)
    thingHash = hashlazy (Aeson.encode thing) :: Digest SHA256
    -- meta-info about the user
    meta = SCIM.Meta
      { SCIM.resourceType = SCIM.UserResource
      , SCIM.created = testDate
      , SCIM.lastModified = testDate
      , SCIM.version = SCIM.Strong (Text.pack (show thingHash))
      , SCIM.location = SCIM.URI $ URI "TODO" Nothing "" "" ""
      }

-- 2018-01-01 00:00
--
-- TODO: real dates!
testDate :: UTCTime
testDate = UTCTime
  { utctDay = ModifiedJulianDay 58119
  , utctDayTime = 0
  }

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

instance SCIM.UserDB (ReaderT TeamId Spar) where
  list mbFilter = case mbFilter of
    Just _ ->
      error "filters are not implemented"
    Nothing -> do
      tid <- ask
      members <- lift $ getTeamMembers tid
      fmap SCIM.fromList $ forM members $ \member ->
        lift (getUser (member ^. Galley.userId)) >>= \case
          Nothing -> SCIM.throwSCIM (SCIM.serverError "Database is inconsistent")
          Just user -> pure (toSCIMUser user)

{-
  get uid = do
    tid <- ask
    assert that the user is a team member
    fetch the user
    convert to storeduser
-}
  -- create   :: User -> m StoredUser
  -- update   :: UserId -> User -> m StoredUser
  -- patch    :: UserId -> m StoredUser
  -- delete   :: UserId -> m Bool  -- ^ Return 'False' if the group didn't exist
  -- getMeta  :: m Meta

----------------------------------------------------------------------------
-- GroupDB

instance SCIM.GroupDB (ReaderT TeamId Spar) where
  -- TODO

----------------------------------------------------------------------------
-- AuthDB

instance SCIM.AuthDB (ReaderT TeamId Spar) where
  -- TODO
  authCheck Nothing =
    pure Unauthorized
  authCheck (Just (SCIM.SCIMAuthData uuid _pass)) =
    pure (Authorized (SCIM.Admin uuid))

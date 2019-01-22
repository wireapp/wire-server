{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE InstanceSigs #-}

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
module Spar.Scim
  (
  -- * The API
    APIScim
  , apiScim
  -- ** Request and response types
  , CreateScimToken(..)
  , CreateScimTokenResponse(..)
  , ScimTokenList(..)

  -- * The mapping between Wire and SCIM users
  -- $mapping
  ) where

import Imports
import Brig.Types.User       as Brig
import Galley.Types.Teams    as Galley
import Control.Monad.Except
import Control.Monad.Catch
import Control.Exception
import Control.Lens hiding ((.=), Strict)
import Data.Id
import Data.Range
import Servant
import Spar.App (Spar, wrapMonadClient, sparCtxOpts, createUser)
import Spar.API.Util
import Spar.Error
import Spar.Types
import Spar.Intra.Galley
import Data.UUID as UUID
import Crypto.Hash
import Data.Time
import Data.Text.Encoding
import Data.Aeson as Aeson
import Text.Email.Validate
import Servant.API.Generic
import OpenSSL.Random (randBytes)
import Data.String.Conversions
import SAML2.WebSSO (IdPId)

import qualified Data.Text    as Text
import qualified Data.UUID.V4 as UUID
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data    as Data
import qualified Data.ByteString.Base64 as ES
import qualified Spar.Intra.Brig as Intra.Brig

import qualified Web.Scim.Class.User              as Scim
import qualified Web.Scim.Class.Group             as Scim
import qualified Web.Scim.Class.Auth              as Scim
import qualified Web.Scim.Server                  as Scim
import qualified Web.Scim.Handler                 as Scim
import qualified Web.Scim.Filter                  as Scim
import qualified Web.Scim.Schema.Common           as Scim
import qualified Web.Scim.Schema.Meta             as Scim
import qualified Web.Scim.Schema.ResourceType     as Scim
import qualified Web.Scim.Schema.ListResponse     as Scim
import qualified Web.Scim.Schema.Error            as Scim

import qualified Web.Scim.Schema.User             as Scim.User
import qualified Web.Scim.Schema.User.Email       as Scim.User
import qualified Web.Scim.Schema.User.Phone       as Scim.User
import qualified Web.Scim.Schema.User.Name        as Scim.User

import qualified Web.Scim.Capabilities.MetaSchema as Scim.Meta

import qualified Web.Scim.Schema.Common           as Scim.Common

-- | SCIM config for our server.
--
-- TODO: the 'Scim.Meta.empty' configuration claims that we don't support
-- filters, but we actually do; it's a bug in hscim
configuration :: Scim.Meta.Configuration
configuration = Scim.Meta.empty

type APIScim
     = OmitDocs :> "v2" :> Scim.SiteAPI ScimToken
  :<|> "auth-tokens" :> APIScimToken

apiScim :: ServerT APIScim Spar
apiScim = hoistScim (toServant (Scim.siteServer configuration))
     :<|> apiScimToken
  where
    hoistScim = hoistServer (Proxy @(Scim.SiteAPI ScimToken))
                            (Scim.fromScimHandler fromError)
    fromError = throwError . SAML.CustomServant . Scim.scimToServantErr

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
toScimUser :: User -> Scim.StoredUser
toScimUser user = Scim.WithMeta meta thing
  where
    -- User ID in text format
    idText = idToText (Brig.userId user)
    -- The representation of the user, without the meta information
    thing = Scim.WithId idText $ Scim.User.empty
      { Scim.User.userName = maybe idText fromHandle (userHandle user)
      , Scim.User.name = Just emptyScimName
      , Scim.User.displayName = Just (fromName (userName user))
      }
    -- The hash of the user representation (used as a version, i.e. ETag)
    thingHash = hashlazy (Aeson.encode thing) :: Digest SHA256
    -- Meta-info about the user
    meta = Scim.Meta
      { Scim.resourceType = Scim.UserResource
      , Scim.created = testDate
      , Scim.lastModified = testDate
      , Scim.version = Scim.Strong (Text.pack (show thingHash))
      -- TODO: The location should be /Users/<uid>. It might also have to
      -- include the baseurl of our server -- this has to be checked.
      , Scim.location = Scim.URI $ URI "https://TODO" Nothing "" "" ""
      }

-- 2018-01-01 00:00
--
-- TODO: real dates!
testDate :: UTCTime
testDate = UTCTime
  { utctDay = ModifiedJulianDay 58119
  , utctDayTime = 0
  }

emptyScimName :: Scim.User.Name
emptyScimName =
  Scim.User.Name
    { Scim.User.formatted = Nothing
    , Scim.User.givenName = Just ""
    , Scim.User.familyName = Just ""
    , Scim.User.middleName = Nothing
    , Scim.User.honorificPrefix = Nothing
    , Scim.User.honorificSuffix = Nothing
    }

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

instance Scim.UserDB Spar where
  -- | List all users, possibly filtered by some predicate.
  list :: ScimTokenInfo
       -> Maybe Scim.Filter
       -> Scim.ScimHandler Spar (Scim.ListResponse Scim.StoredUser)
  list ScimTokenInfo{stiTeam} mbFilter = do
    members <- lift $ getTeamMembers stiTeam
    users <- fmap catMaybes $ forM members $ \member ->
      lift (Intra.Brig.getUser (member ^. Galley.userId)) >>= \case
        Just user
          | userDeleted user -> pure Nothing
          | otherwise        -> pure (Just (toScimUser user))
        Nothing -> Scim.throwScim $
          Scim.serverError "Scim.UserDB.list: couldn't fetch team member"
    let check user = case mbFilter of
          Nothing -> pure True
          Just filter_ ->
            let user' = Scim.Common.value (Scim.thing user)
            in case Scim.filterUser filter_ user' of
                 Right res -> pure res
                 Left err  -> Scim.throwScim $
                   Scim.badRequest Scim.InvalidFilter (Just err)
    -- TODO: once bigger teams arrive, we should have pagination here.
    Scim.fromList <$> filterM check users

  -- | Get a single user by its ID.
  get :: ScimTokenInfo
      -> Text
      -> Scim.ScimHandler Spar (Maybe Scim.StoredUser)
  get ScimTokenInfo{stiTeam} uidText = do
    uid <- case readMaybe (Text.unpack uidText) of
      Just u -> pure u
      Nothing -> Scim.throwScim $
        Scim.notFound "user" uidText
    lift (Intra.Brig.getUser uid) >>= traverse (\user -> do
      when (userTeam user /= Just stiTeam || userDeleted user) $
        Scim.throwScim $ Scim.notFound "user" (idToText uid)
      pure (toScimUser user))

  -- | Create a new user.
  create :: ScimTokenInfo
         -> Scim.User.User
         -> Scim.ScimHandler Spar Scim.StoredUser
  create ScimTokenInfo{stiIdP} user = do
    extId <- case Scim.User.externalId user of
      Just x -> pure x
      Nothing -> Scim.throwScim $
        Scim.badRequest Scim.InvalidValue (Just "externalId is required")
    handl <- case parseHandle (Scim.User.userName user) of
      Just x -> pure x
      Nothing -> Scim.throwScim $
        Scim.badRequest Scim.InvalidValue (Just "userName is not compliant")

    -- We check the name for validity, but only if it's present
    mbName <- forM (Scim.User.displayName user) $ \n ->
      case checkedEitherMsg @_ @1 @128 "displayName" n of
        Right x -> pure $ Name (fromRange x)
        Left err -> Scim.throwScim $
          Scim.badRequest
            Scim.InvalidValue
            (Just ("displayName is not compliant: " <> Text.pack err))
    -- NB: We assume that checking that the user does _not_ exist has
    -- already been done before -- the hscim library check does a 'get'
    -- before a 'create'

    -- TODO: Assume that externalID is the subjectID, let's figure out how
    -- to extract that later
    issuer <- case stiIdP of
        Nothing -> Scim.throwScim $
          Scim.serverError "No IdP configured for the provisioning token"
        Just idp -> lift (wrapMonadClient (Data.getIdPConfig idp)) >>= \case
            Nothing -> Scim.throwScim $
              Scim.serverError "The IdP corresponding to the provisioning token \
                               \was not found"
            Just idpConfig -> pure (idpConfig ^. SAML.idpMetadata . SAML.edIssuer)
    let uref = SAML.UserRef issuer (SAML.opaqueNameID extId)

    -- TODO: Adding a handle should be done _DURING_ the creation
    buid <- lift $ createUser uref mbName
    lift $ Intra.Brig.setHandle buid handl

    maybe (Scim.throwScim (Scim.serverError "Scim.UserDB.create: user disappeared"))
          (pure . toScimUser) =<<
      lift (Intra.Brig.getUser buid)

  update :: ScimTokenInfo
         -> Text
         -> Scim.User.User
         -> Scim.ScimHandler Spar Scim.StoredUser
  update _ _ _ =
      Scim.throwScim $ Scim.serverError "User update is not implemented yet"

  delete :: ScimTokenInfo -> Text -> Scim.ScimHandler Spar Bool
  delete _ _ =
      Scim.throwScim $ Scim.serverError "User delete is not implemented yet"

  getMeta :: ScimTokenInfo -> Scim.ScimHandler Spar Scim.Meta
  getMeta _ =
      Scim.throwScim $ Scim.serverError "User getMeta is not implemented yet"

----------------------------------------------------------------------------
-- GroupDB

instance Scim.GroupDB Spar where
  -- TODO

----------------------------------------------------------------------------
-- AuthDB

instance Scim.AuthDB Spar where
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

type APIScimToken
     = Header "Z-User" UserId :> APIScimTokenCreate
  :<|> Header "Z-User" UserId :> APIScimTokenDelete
  :<|> Header "Z-User" UserId :> APIScimTokenList

type APIScimTokenCreate
     = ReqBody '[JSON] CreateScimToken
    :> Post '[JSON] CreateScimTokenResponse

type APIScimTokenDelete
     = QueryParam' '[Required, Strict] "id" ScimTokenId
    :> DeleteNoContent '[JSON] NoContent

type APIScimTokenList
     = Get '[JSON] ScimTokenList

apiScimToken :: ServerT APIScimToken Spar
apiScimToken
     = createScimToken
  :<|> deleteScimToken
  :<|> listScimTokens

----------------------------------------------------------------------------
-- Request and response types

-- | Type used for request parameters to 'APIScimTokenCreate'.
data CreateScimToken = CreateScimToken
  { createScimTokenDescr :: Text
  } deriving (Eq, Show)

instance FromJSON CreateScimToken where
  parseJSON = withObject "CreateScimToken" $ \o -> do
    createScimTokenDescr <- o .: "description"
    pure CreateScimToken{..}

instance ToJSON CreateScimToken where
  toJSON CreateScimToken{..} = object
    [ "description" .= createScimTokenDescr
    ]

-- | Type used for the response of 'APIScimTokenCreate'.
data CreateScimTokenResponse = CreateScimTokenResponse
  { createScimTokenResponseToken :: ScimToken
  , createScimTokenResponseInfo  :: ScimTokenInfo
  } deriving (Eq, Show)

instance FromJSON CreateScimTokenResponse where
  parseJSON = withObject "CreateScimTokenResponse" $ \o -> do
    createScimTokenResponseToken <- o .: "token"
    createScimTokenResponseInfo  <- o .: "info"
    pure CreateScimTokenResponse{..}

instance ToJSON CreateScimTokenResponse where
  toJSON CreateScimTokenResponse{..} = object
    [ "token" .= createScimTokenResponseToken
    , "info"  .= createScimTokenResponseInfo
    ]

-- | Type used for responses of endpoints that return a list of SCIM tokens.
-- Wrapped into an object to allow extensibility later on.
--
-- We don't show tokens once they have been created – only their metadata.
data ScimTokenList = ScimTokenList
  { scimTokenListTokens :: [ScimTokenInfo]
  }
  deriving (Eq, Show)

instance FromJSON ScimTokenList where
  parseJSON = withObject "ScimTokenList" $ \o -> do
    scimTokenListTokens <- o .: "tokens"
    pure ScimTokenList{..}

instance ToJSON ScimTokenList where
  toJSON ScimTokenList{..} = object
    [ "tokens" .= scimTokenListTokens
    ]

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

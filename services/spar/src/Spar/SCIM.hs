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

  -- * testing
  , toSCIMUser'
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
import Spar.App (Spar, Env, wrapMonadClient, sparCtxOpts, createUser)
import Spar.Error
import Spar.Intra.Galley
import Spar.Types
import Text.Email.Validate

import qualified Data.ByteString.Base64 as ES
import qualified Data.Text    as Text
import qualified Data.UUID.V4 as UUID
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data    as Data
import qualified Spar.Intra.Brig as Intra.Brig
import qualified URI.ByteString as URIBS

import qualified Web.SCIM.Class.Auth              as SCIM
import qualified Web.SCIM.Class.Group             as SCIM
import qualified Web.SCIM.Class.User              as SCIM
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

type APIScim
     = OmitDocs :> "v2" :> SCIM.SiteAPI ScimToken
  :<|> "auth-tokens" :> APIScimToken

apiScim :: ServerT APIScim Spar
apiScim = hoistSCIM (toServant (SCIM.siteServer configuration))
     :<|> apiScimToken
  where
    hoistSCIM = hoistServer (Proxy @(SCIM.SiteAPI ScimToken))
                            (SCIM.fromSCIMHandler fromError)
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

data ValidSCIMUser = ValidSCIMUser
  { _vsuUser          :: SCIM.User.User
  , _vsuSAMLSubjectId :: Maybe SAML.NameID
    -- ^ if the user should authenticate via SAML, use this subjId.
  , _vsuHandle        :: Handle
  , _vsuName          :: Maybe Name
  }

validateSCIMUser
  :: forall m m'. (m ~ SCIM.SCIMHandler m', Monad m')
  => SCIM.User.User -> m ValidSCIMUser
validateSCIMUser user = do
    let validateNameOrExtId :: Maybe Text -> m (Maybe Text)
        validateNameOrExtId mtxt = forM mtxt $ \txt ->
          case checkedEitherMsg @_ @1 @128 "displayName" txt of
            Right rtxt -> pure $ fromRange rtxt
            Left err -> SCIM.throwSCIM $ SCIM.badRequest SCIM.InvalidValue
              (Just ("displayName is not compliant: " <> Text.pack err))

    -- TODO: Assume that externalID is the subjectID, let's figure out how
    -- to extract that later
    samlSubjectId <- SAML.opaqueNameID <$$>
      validateNameOrExtId (SCIM.User.externalId user)

    handl <- case parseHandle (SCIM.User.userName user) of
      Just x -> pure x
      Nothing -> SCIM.throwSCIM $
        SCIM.badRequest SCIM.InvalidValue (Just "userName is not compliant")

    -- We check the name for validity, but only if it's present
    mbName <- Name <$$> validateNameOrExtId (SCIM.User.displayName user)

    -- NB: We assume that checking that the user does _not_ exist has
    -- already been done before -- the hscim library check does a 'get'
    -- before a 'create'

    pure $ ValidSCIMUser user samlSubjectId handl mbName

createValidSCIMUser :: ScimTokenInfo -> ValidSCIMUser -> ExceptT SCIM.SCIMError Spar SCIM.StoredUser
createValidSCIMUser ScimTokenInfo{stiIdP} (ValidSCIMUser _user samlSubjectId handl mbName) = do
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

    -- TODO: Adding a handle should be done _DURING_ the creation
    buid <- lift $ createUser uref mbName
    lift $ Intra.Brig.setHandle buid handl

    maybe (SCIM.throwSCIM (SCIM.serverError "SCIM.UserDB.create: user disappeared"))
          (lift . toSCIMUser) =<<
      lift (Intra.Brig.getUser buid)


-- | Expose a Wire user as an SCIM user.
--
-- TODO: this function is conceptually broken: brig users contain strictly less information than
-- SCIM users, and that's a good thing.  instead, we need to store SCIM user records in spar, and
-- sync them with brig on a need-to-know basis.
toSCIMUser :: (SAML.HasNow m, MonadReader Env m) => User -> m SCIM.StoredUser
toSCIMUser user = do
  SAML.Time now <- SAML.getNow
  baseuri <- asks $ derivedOptsScimBaseURI . derivedOpts . sparCtxOpts
  pure $ toSCIMUser' now baseuri user

-- | Pure part of 'toSCIMUser'.
toSCIMUser' :: UTCTime -> URIBS.URI -> User -> SCIM.StoredUser
toSCIMUser' now baseuri user = SCIM.WithMeta meta thing
  where
    mkLocation :: String -> URI
    mkLocation pathSuffix = convURI $ baseuri SAML.=/ cs pathSuffix
      where
        convURI uri = fromMaybe err . parseURI . cs . URIBS.serializeURIRef' $ uri
          where err = error $ "internal error: " <> show uri

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
      , SCIM.created = now
      , SCIM.lastModified = now
      , SCIM.version = SCIM.Strong (Text.pack (show thingHash))
        -- TODO: it looks like (a) we need to add this to the HTTP header, and (b) this should be a
        -- version (which is increasing over time), not a hash (which is not).
        -- https://tools.ietf.org/html/rfc7644#section-3.14
      , SCIM.location = SCIM.URI . mkLocation $ "/Users/" <> show (Brig.userId user)
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
  list :: ScimTokenInfo
       -> Maybe SCIM.Filter
       -> SCIM.SCIMHandler Spar (SCIM.ListResponse SCIM.StoredUser)
  list ScimTokenInfo{stiTeam} mbFilter = do
    members <- lift $ getTeamMembers stiTeam
    users <- fmap catMaybes $ forM members $ \member ->
      lift (Intra.Brig.getUser (member ^. Galley.userId)) >>= \case
        Just user
          | userDeleted user -> pure Nothing
          | otherwise        -> Just <$> lift (toSCIMUser user)
        Nothing -> SCIM.throwSCIM $
          SCIM.serverError "SCIM.UserDB.list: couldn't fetch team member"
    let check user = case mbFilter of
          Nothing -> pure True
          Just filter_ ->
            let user' = SCIM.Common.value (SCIM.thing user)
            in case SCIM.filterUser filter_ user' of
                 Right res -> pure res
                 Left err  -> SCIM.throwSCIM $
                   SCIM.badRequest SCIM.InvalidFilter (Just err)
    -- FUTUREWORK: once bigger teams arrive, we should have pagination here.
    SCIM.fromList <$> filterM check users

  -- | Get a single user by its ID.
  get :: ScimTokenInfo
      -> Text
      -> SCIM.SCIMHandler Spar (Maybe SCIM.StoredUser)
  get ScimTokenInfo{stiTeam} uidText = do
    uid <- case readMaybe (Text.unpack uidText) of
      Just u -> pure u
      Nothing -> SCIM.throwSCIM $
        SCIM.notFound "user" uidText
    lift (Intra.Brig.getUser uid) >>= traverse (\user -> do
      when (userTeam user /= Just stiTeam || userDeleted user) $
        SCIM.throwSCIM $ SCIM.notFound "user" (idToText uid)
      lift (toSCIMUser user))

  -- | Create a new user.
  create :: ScimTokenInfo
         -> SCIM.User.User
         -> SCIM.SCIMHandler Spar SCIM.StoredUser
  create tokinfo user = createValidSCIMUser tokinfo =<< validateSCIMUser user

  update :: ScimTokenInfo
         -> Text
         -> SCIM.User.User
         -> SCIM.SCIMHandler Spar SCIM.StoredUser
  update _ _ _ =
      SCIM.throwSCIM $ SCIM.serverError "User update is not implemented yet"

  delete :: ScimTokenInfo -> Text -> SCIM.SCIMHandler Spar Bool
  delete _ _ =
      SCIM.throwSCIM $ SCIM.serverError "User delete is not implemented yet"

  getMeta :: ScimTokenInfo -> SCIM.SCIMHandler Spar SCIM.Meta
  getMeta _ =
      SCIM.throwSCIM $ SCIM.serverError "User getMeta is not implemented yet"

----------------------------------------------------------------------------
-- GroupDB

instance SCIM.GroupDB Spar where
  -- TODO

----------------------------------------------------------------------------
-- AuthDB

instance SCIM.AuthDB Spar where
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

{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns        #-}

module Util.Scim where

import Imports
import Bilge
import Bilge.Assert
import Brig.Types.User
import Cassandra
import Control.Lens
import Control.Monad.Random
import Data.ByteString.Conversion
import Data.Id
import Data.String.Conversions (cs)
import Data.Time
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import SAML2.WebSSO.Types (IdPId, idpId)
import Spar.Data as Data
import Spar.Scim (CreateScimToken(..), CreateScimTokenResponse(..), ScimTokenList(..))
import Spar.Scim.Types
import Spar.Types (ScimToken(..), ScimTokenInfo(..), IdP)
import Util.Core
import Util.Types
import Web.HttpApiData (toHeader)

import qualified Data.Aeson                       as Aeson
import qualified SAML2.WebSSO                     as SAML
import qualified Spar.Intra.Brig                  as Intra
import qualified Text.Email.Parser                as Email
import qualified Web.Scim.Class.User              as Scim
import qualified Web.Scim.Filter                  as Scim
import qualified Web.Scim.Schema.Common           as Scim
import qualified Web.Scim.Schema.ListResponse     as Scim
import qualified Web.Scim.Schema.Meta             as Scim
import qualified Web.Scim.Schema.User             as Scim.User
import qualified Web.Scim.Schema.User.Email       as Email
import qualified Web.Scim.Schema.User.Phone       as Phone


-- | Call 'registerTestIdP', then 'registerScimToken'.  The user returned is the owner of the team;
-- the IdP is registered with the team; the SCIM token can be used to manipulate the team.
registerIdPAndScimToken :: HasCallStack => TestSpar (ScimToken, (UserId, TeamId, IdP))
registerIdPAndScimToken = do
  team@(_owner, teamid, idp) <- registerTestIdP
  (, team) <$> registerScimToken teamid (Just (idp ^. idpId))

-- | Create a fresh SCIM token and register it for the team.
registerScimToken :: HasCallStack => TeamId -> Maybe IdPId -> TestSpar ScimToken
registerScimToken teamid midpid = do
  env <- ask
  tok <- ScimToken <$> do
    code <- liftIO UUID.nextRandom
    pure $ "scim-test-token/" <> "team=" <> idToText teamid <> "/code=" <> UUID.toText code
  scimTokenId <- randomId
  now <- liftIO getCurrentTime
  runClient (env ^. teCql) $ Data.insertScimToken
      tok
      ScimTokenInfo
          { stiTeam      = teamid
          , stiId        = scimTokenId
          , stiCreatedAt = now
          , stiIdP       = midpid
          , stiDescr     = "test token"
          }
  pure tok

-- | Generate a SCIM user with a random name and handle.  At the very least, everything considered
-- in @instance IsUser Scim.User.User@ and @validateScimUser@ must be random here.
--
-- FUTUREWORK: make this more exhaustive.  change everything that can be changed!  move this to the
-- hspec package when done.
randomScimUser :: MonadRandom m => m Scim.User.User
randomScimUser = do
    suffix <- cs <$> replicateM 5 (getRandomR ('0', '9'))
    emails <- getRandomR (0, 3) >>= \n -> replicateM n randomScimEmail
    phones <- getRandomR (0, 3) >>= \n -> replicateM n randomScimPhone
    pure $ Scim.User.empty
        { Scim.User.userName     = "scimuser_" <> suffix
        , Scim.User.displayName  = Just ("Scim User #" <> suffix)
        , Scim.User.externalId   = Just ("scimuser_extid_" <> suffix <> "@wire.com")
        , Scim.User.emails       = emails
        , Scim.User.phoneNumbers = phones
        }

randomScimEmail :: MonadRandom m => m Email.Email
randomScimEmail = do
    let typ     :: Maybe Text = Nothing
        primary :: Maybe Bool = Nothing  -- TODO: where should we catch users with more than one
                                         -- primary email?
    value :: Email.EmailAddress2 <- do
      localpart  <- cs <$> replicateM 15 (getRandomR ('a', 'z'))
      domainpart <- (<> ".com") . cs <$> replicateM 15 (getRandomR ('a', 'z'))
      pure . Email.EmailAddress2 $ Email.unsafeEmailAddress localpart domainpart
    pure Email.Email{..}

randomScimPhone :: MonadRandom m => m Phone.Phone
randomScimPhone = do
    let typ :: Maybe Text = Nothing
    value :: Maybe Text <- do
      let mkdigits n = replicateM n (getRandomR ('0', '9'))
      mini <- mkdigits 8
      maxi <- mkdigits =<< getRandomR (0, 7)
      pure $ Just (cs ('+' : mini <> maxi))
    pure Phone.Phone{..}

----------------------------------------------------------------------------
-- API wrappers

-- | Create a user.
createUser
    :: HasCallStack
    => ScimToken
    -> Scim.User.User
    -> TestSpar Scim.StoredUser
createUser tok user = do
    env <- ask
    r <- createUser_
             (Just tok)
             user
             (env ^. teSpar)
         <!! const 201 === statusCode
    pure (decodeBody' r)

-- | Update a user.
updateUser
    :: HasCallStack
    => ScimToken
    -> UserId
    -> Scim.User.User
    -> TestSpar Scim.StoredUser
updateUser tok userid user = do
    env <- ask
    r <- updateUser_
             (Just tok)
             (Just userid)
             user
             (env ^. teSpar)
         <!! const 200 === statusCode
    pure (decodeBody' r)

-- | List all users.
listUsers
    :: HasCallStack
    => ScimToken
    -> Maybe Scim.Filter
    -> TestSpar [Scim.StoredUser]
listUsers tok mbFilter = do
    env <- ask
    r <- listUsers_
             (Just tok)
             mbFilter
             (env ^. teSpar)
         <!! const 200 === statusCode
    let r' = decodeBody' r
    when (Scim.totalResults r' /= length (Scim.resources r')) $
        error "listUsers: got a paginated result, but pagination \
              \is not supported yet"
    pure (Scim.resources r')

-- | Get a user.
getUser
    :: HasCallStack
    => ScimToken
    -> UserId
    -> TestSpar Scim.StoredUser
getUser tok userid = do
    env <- ask
    r <- getUser_
             (Just tok)
             userid
             (env ^. teSpar)
         <!! const 200 === statusCode
    pure (decodeBody' r)

-- | Create a SCIM token.
createToken
    :: HasCallStack
    => UserId
    -> CreateScimToken
    -> TestSpar CreateScimTokenResponse
createToken zusr payload = do
    env <- ask
    r <- createToken_
             zusr
             payload
             (env ^. teSpar)
         <!! const 200 === statusCode
    pure (decodeBody' r)

-- | Delete a SCIM token.
deleteToken
    :: HasCallStack
    => UserId
    -> ScimTokenId                -- ^ Token to delete
    -> TestSpar ()
deleteToken zusr tokenid = do
    env <- ask
    deleteToken_
        zusr
        tokenid
        (env ^. teSpar)
        !!! const 204 === statusCode

-- | List SCIM tokens.
listTokens
    :: HasCallStack
    => UserId
    -> TestSpar ScimTokenList
listTokens zusr = do
    env <- ask
    r <- listTokens_
             zusr
             (env ^. teSpar)
         <!! const 200 === statusCode
    pure (decodeBody' r)

----------------------------------------------------------------------------
-- "Raw" API requests

-- | Create a user.
createUser_
    :: Maybe ScimToken          -- ^ Authentication
    -> Scim.User.User           -- ^ User data
    -> SparReq                  -- ^ Spar endpoint
    -> TestSpar ResponseLBS
createUser_ auth user spar_ = do
    -- NB: we don't use 'mkEmailRandomLocalSuffix' here, because emails
    -- shouldn't be submitted via SCIM anyway.
    -- TODO: what's the consequence of this?  why not update emails via
    -- SCIM?  how else should they be submitted?  i think this there is
    -- still some confusion here about the distinction between *validated*
    -- emails and *scim-provided* emails, which are two entirely
    -- different things.
    call . post $
        ( spar_
        . paths ["scim", "v2", "Users"]
        . scimAuth auth
        . contentScim
        . body (RequestBodyLBS . Aeson.encode $ user)
        . acceptScim
        )

-- | Update a user.
updateUser_
    :: Maybe ScimToken          -- ^ Authentication
    -> Maybe UserId             -- ^ User to update; when not provided, the request will return 4xx
    -> Scim.User.User           -- ^ User data
    -> SparReq                  -- ^ Spar endpoint
    -> TestSpar ResponseLBS
updateUser_ auth muid user spar_ = do
    call . put $
        ( spar_
        . paths (["scim", "v2", "Users"] <> maybeToList (toByteString' <$> muid))
        . scimAuth auth
        . contentScim
        . body (RequestBodyLBS . Aeson.encode $ user)
        . acceptScim
        )

-- | List all users.
listUsers_
    :: Maybe ScimToken          -- ^ Authentication
    -> Maybe Scim.Filter        -- ^ Predicate to filter the results
    -> SparReq                  -- ^ Spar endpoint
    -> TestSpar ResponseLBS
listUsers_ auth mbFilter spar_ = do
    call . get $
        ( spar_
        . paths ["scim", "v2", "Users"]
        . queryItem' "filter" (toByteString' . Scim.renderFilter <$> mbFilter)
        . scimAuth auth
        . acceptScim
        )

-- | Get one user.
getUser_
    :: Maybe ScimToken          -- ^ Authentication
    -> UserId                   -- ^ User
    -> SparReq                  -- ^ Spar endpoint
    -> TestSpar ResponseLBS
getUser_ auth userid spar_ = do
    call . get $
        ( spar_
        . paths ["scim", "v2", "Users", toByteString' userid]
        . scimAuth auth
        . acceptScim
        )

-- | Create a SCIM token.
createToken_
    :: UserId                   -- ^ User
    -> CreateScimToken
    -> SparReq                  -- ^ Spar endpoint
    -> TestSpar ResponseLBS
createToken_ userid payload spar_ = do
    call . post $
        ( spar_
        . paths ["scim", "auth-tokens"]
        . zUser userid
        . contentJson
        . body (RequestBodyLBS . Aeson.encode $ payload)
        . acceptJson
        )

-- | Delete a SCIM token.
deleteToken_
    :: UserId                   -- ^ User
    -> ScimTokenId              -- ^ Token to delete
    -> SparReq                  -- ^ Spar endpoint
    -> TestSpar ResponseLBS
deleteToken_ userid tokenid spar_ = do
    call . delete $
        ( spar_
        . paths ["scim", "auth-tokens"]
        . queryItem "id" (toByteString' tokenid)
        . zUser userid
        )

-- | List SCIM tokens.
listTokens_
    :: UserId                   -- ^ User
    -> SparReq                  -- ^ Spar endpoint
    -> TestSpar ResponseLBS
listTokens_ userid spar_ = do
    call . get $
        ( spar_
        . paths ["scim", "auth-tokens"]
        . zUser userid
        )

----------------------------------------------------------------------------
-- Utilities

-- | Add SCIM authentication to a request.
scimAuth :: Maybe ScimToken -> Request -> Request
scimAuth Nothing = id
scimAuth (Just auth) = header "Authorization" (toHeader auth)

-- | Signal that the body is an SCIM payload.
contentScim :: Request -> Request
contentScim = content "application/scim+json"

-- | Signal that the response type is expected to be an SCIM payload.
acceptScim :: Request -> Request
acceptScim = accept "application/scim+json"

-- | Get ID of a user returned from SCIM.
scimUserId :: Scim.StoredUser -> UserId
scimUserId storedUser = either err id (readEither id_)
  where
    id_ = cs (Scim.id (Scim.thing storedUser))
    err e = error $ "scimUserId: couldn't parse ID " ++ id_ ++ ": " ++ e

-- | There are a number of user types that all partially map on each other.  This class provides a
-- uniform interface to data stored in those types.
class IsUser u where
  maybeUserId :: u -> Maybe UserId
  maybeHandle :: u -> Maybe (Maybe Handle)
  maybeName :: u -> Maybe (Maybe Name)
  maybeTenant :: u -> Maybe (Maybe SAML.Issuer)
  maybeSubject :: u -> Maybe (Maybe SAML.NameID)

-- | 'ValidScimUser' is tested in ScimSpec.hs exhaustively with literal inputs, so here we assume it
-- is correct and don't aim to verify that name, handle, etc correspond to ones in 'vsuUser'.
instance IsUser ValidScimUser where
    maybeUserId = const Nothing
    maybeHandle = Just . Just . view vsuHandle
    maybeName = Just . view vsuName
    maybeTenant = Just . Just . view (vsuSAMLUserRef . SAML.uidTenant)
    maybeSubject = Just . Just . view (vsuSAMLUserRef . SAML.uidSubject)

instance IsUser Scim.StoredUser where
    maybeUserId = Just . scimUserId
    maybeHandle = maybeHandle . Scim.value . Scim.thing
    maybeName = maybeName . Scim.value . Scim.thing
    maybeTenant = maybeTenant . Scim.value . Scim.thing
    maybeSubject = maybeSubject . Scim.value . Scim.thing

instance IsUser Scim.User.User where
    maybeUserId = const Nothing
    maybeHandle = Just . Just . Handle . Scim.User.userName
    maybeName = Just . fmap Name . Scim.User.displayName
    maybeTenant = const Nothing
    maybeSubject =
        let emailNameID s = SAML.NameID (SAML.UNameIDEmail s) Nothing Nothing Nothing
        in Just . fmap emailNameID . Scim.User.externalId

instance IsUser User where
    maybeUserId = Just . userId
    maybeHandle = Just . userHandle
    maybeName = Just . Just . userName
    maybeTenant = Just . fmap (view SAML.uidTenant) . urefFromBrig
    maybeSubject = Just . fmap (view SAML.uidSubject) . urefFromBrig

-- | For all properties that are present in both @u1@ and @u2@, check that they match.
userShouldMatch
    :: (HasCallStack, MonadIO m, IsUser u1, IsUser u2)
    => u1 -> u2 -> m ()
userShouldMatch u1 u2 = liftIO $ do
    check "userId" (maybeUserId u1) (maybeUserId u2)
    check "handle" (maybeHandle u1) (maybeHandle u2)
    check "name" (maybeName u1) (maybeName u2)
    check "tenant" (maybeTenant u1) (maybeTenant u2)
    check "subject" (maybeSubject u1) (maybeSubject u2)
  where
    check field (Just a) (Just b) = (field :: String, a) `shouldBe` (field, b)
    check _ _ _ = pure ()

urefFromBrig :: User -> Maybe SAML.UserRef
urefFromBrig brigUser = case userIdentity brigUser of
    Just (SSOIdentity ssoid _ _) -> case Intra.fromUserSSOId ssoid of
        Right uref -> Just uref
        Left e     -> error $
            "urefFromBrig: bad SSO id: " <>
            "UserSSOId = " <> show ssoid <> ", error = " <> e
    _ -> Nothing

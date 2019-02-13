
module Util.Scim where

import Imports
import Bilge
import Bilge.Assert
import Brig.Types.User
import Cassandra
import Control.Lens
import Data.ByteString.Conversion
import Data.Id
import Data.Text (pack, unpack)
import Data.Time
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import SAML2.WebSSO.Types (IdPId, idpId)
import Spar.Data as Data
import Spar.Scim (CreateScimToken(..), CreateScimTokenResponse(..), ScimTokenList(..))
import Spar.Types (ScimToken(..), ScimTokenInfo(..), IdP)
import System.Random
import Util.Core
import Util.Types
import Web.HttpApiData (toHeader)

import qualified Data.Aeson                       as Aeson
import qualified Web.Scim.Class.User              as Scim
import qualified Web.Scim.Filter                  as Scim
import qualified Web.Scim.Schema.Common           as Scim
import qualified Web.Scim.Schema.ListResponse     as Scim
import qualified Web.Scim.Schema.Meta             as Scim
import qualified Web.Scim.Schema.User             as Scim.User


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

-- | Generate a SCIM user with a random name and handle.
randomScimUser :: TestSpar Scim.User.User
randomScimUser = do
    suffix <- pack <$> replicateM 5 (liftIO (randomRIO ('0', '9')))
    pure $ Scim.User.empty
        { Scim.User.userName    = "scimuser_" <> suffix
        , Scim.User.displayName = Just ("Scim User #" <> suffix)
        , Scim.User.externalId  = Just ("scimuser_extid_" <> suffix)
        }

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
    call . post $
        ( spar_
        . paths ["scim", "v2", "Users"]
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
    id_ = unpack (Scim.id (Scim.thing storedUser))
    err e = error $ "scimUserId: couldn't parse ID " ++ id_ ++ ": " ++ e

-- | Check that some properties match between an SCIM user and a Brig user.
userShouldMatch
    :: (HasCallStack, MonadIO m)
    => Scim.StoredUser -> User -> m ()
userShouldMatch scimStoredUser brigUser = liftIO $ do
    let scimUser = Scim.value (Scim.thing scimStoredUser)
    scimUserId scimStoredUser `shouldBe`
        userId brigUser
    Just (Handle (Scim.User.userName scimUser)) `shouldBe`
        userHandle brigUser
    fmap Name (Scim.User.displayName scimUser) `shouldBe`
        Just (userName brigUser)

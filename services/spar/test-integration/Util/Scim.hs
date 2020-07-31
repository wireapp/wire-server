{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Util.Scim where

import Bilge
import Bilge.Assert
import Brig.Types.User
import Cassandra
import Control.Lens
import Control.Monad.Random
import qualified Data.Aeson as Aeson
import Data.ByteString.Conversion
import Data.Handle (Handle (Handle))
import Data.Id
import Data.String.Conversions (cs)
import Data.Time
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import Imports
import qualified SAML2.WebSSO as SAML
import SAML2.WebSSO.Types (IdPId, idpId)
import Spar.Data as Data
import qualified Spar.Intra.Brig as Intra
import Spar.Scim (CreateScimToken (..), CreateScimTokenResponse (..), ScimTokenList (..))
import Spar.Scim.Types
import Spar.Scim.User (synthesizeScimUser, validateScimUser')
import Spar.Types (IdP, IdPMetadataInfo (..), ScimToken (..), ScimTokenInfo (..))
import Test.QuickCheck (arbitrary, generate)
import qualified Text.Email.Parser as Email
import qualified Text.XML.DSig as SAML
import Util.Core
import Util.Types
import Web.HttpApiData (toHeader)
import qualified Web.Scim.Class.User as Scim
import qualified Web.Scim.Filter as Filter
import qualified Web.Scim.Filter as Scim
import qualified Web.Scim.Schema.Common as Scim
import qualified Web.Scim.Schema.ListResponse as Scim
import qualified Web.Scim.Schema.Meta as Scim
import qualified Web.Scim.Schema.PatchOp as Scim.PatchOp
import qualified Web.Scim.Schema.User as Scim.User
import qualified Web.Scim.Schema.User.Email as Email
import qualified Web.Scim.Schema.User.Phone as Phone
import Wire.API.User.RichInfo

-- | Call 'registerTestIdP', then 'registerScimToken'.  The user returned is the owner of the team;
-- the IdP is registered with the team; the SCIM token can be used to manipulate the team.
registerIdPAndScimToken :: HasCallStack => TestSpar (ScimToken, (UserId, TeamId, IdP))
registerIdPAndScimToken = do
  team@(_owner, teamid, idp) <- registerTestIdP
  (,team) <$> registerScimToken teamid (Just (idp ^. idpId))

-- | Call 'registerTestIdPWithMeta', then 'registerScimToken'.  The user returned is the owner of the team;
-- the IdP is registered with the team; the SCIM token can be used to manipulate the team.
registerIdPAndScimTokenWithMeta :: HasCallStack => TestSpar (ScimToken, (UserId, TeamId, IdP, (IdPMetadataInfo, SAML.SignPrivCreds)))
registerIdPAndScimTokenWithMeta = do
  team@(_owner, teamid, idp, _) <- registerTestIdPWithMeta
  (,team) <$> registerScimToken teamid (Just (idp ^. idpId))

-- | Create a fresh SCIM token and register it for the team.
registerScimToken :: HasCallStack => TeamId -> Maybe IdPId -> TestSpar ScimToken
registerScimToken teamid midpid = do
  env <- ask
  tok <-
    ScimToken <$> do
      code <- liftIO UUID.nextRandom
      pure $ "scim-test-token/" <> "team=" <> idToText teamid <> "/code=" <> UUID.toText code
  scimTokenId <- randomId
  now <- liftIO getCurrentTime
  runClient (env ^. teCql) $
    Data.insertScimToken
      tok
      ScimTokenInfo
        { stiTeam = teamid,
          stiId = scimTokenId,
          stiCreatedAt = now,
          stiIdP = midpid,
          stiDescr = "test token"
        }
  pure tok

-- | Generate a SCIM user with a random name and handle.  At the very least, everything considered
-- in @instance IsUser Scim.User.User@ and @validateScimUser@ must be random here.
--
-- FUTUREWORK: make this more exhaustive.  change everything that can be changed!  move this to the
-- hspec package when done.
randomScimUser :: (HasCallStack, MonadRandom m, MonadIO m) => m (Scim.User.User SparTag)
randomScimUser = fst <$> randomScimUserWithSubject

-- | Like 'randomScimUser', but also returns the intended subject ID that the user should
-- have. It's already available as 'Scim.User.externalId' but it's not structured.
randomScimUserWithSubject ::
  (HasCallStack, MonadRandom m, MonadIO m) =>
  m (Scim.User.User SparTag, SAML.UnqualifiedNameID)
randomScimUserWithSubject = do
  randomScimUserWithSubjectAndRichInfo =<< liftIO (generate arbitrary)

-- | See 'randomScimUser', 'randomScimUserWithSubject'.
randomScimUserWithSubjectAndRichInfo ::
  (HasCallStack, MonadRandom m, MonadIO m) =>
  RichInfo ->
  m (Scim.User.User SparTag, SAML.UnqualifiedNameID)
randomScimUserWithSubjectAndRichInfo richInfo = do
  suffix <- cs <$> replicateM 7 (getRandomR ('0', '9'))
  emails <- getRandomR (0, 3) >>= \n -> replicateM n randomScimEmail
  phones <- getRandomR (0, 3) >>= \n -> replicateM n randomScimPhone
  -- Related, but non-trivial to re-use here: 'nextSubject'
  (externalId, subj) <-
    getRandomR (0, 1 :: Int) <&> \case
      0 ->
        ( "scimuser_extid_" <> suffix <> "@example.com",
          either (error . show) id $
            SAML.mkUNameIDEmail ("scimuser_extid_" <> suffix <> "@example.com")
        )
      1 ->
        ( "scimuser_extid_" <> suffix,
          SAML.mkUNameIDUnspecified ("scimuser_extid_" <> suffix)
        )
      _ -> error "randomScimUserWithSubject: impossible"
  pure
    ( (Scim.User.empty userSchemas ("scimuser_" <> suffix) (ScimUserExtra richInfo))
        { Scim.User.displayName = Just ("Scim User #" <> suffix),
          Scim.User.externalId = Just externalId,
          Scim.User.emails = emails,
          Scim.User.phoneNumbers = phones
        },
      subj
    )

randomScimUserWithEmail :: MonadRandom m => m (Scim.User.User SparTag, Email)
randomScimUserWithEmail = do
  suffix <- cs <$> replicateM 7 (getRandomR ('0', '9'))
  let email = Email ("email" <> suffix) "example.com"
      externalId = fromEmail email
  pure
    ( (Scim.User.empty userSchemas ("scimuser_" <> suffix) (ScimUserExtra mempty))
        { Scim.User.displayName = Just ("Scim User #" <> suffix),
          Scim.User.externalId = Just externalId
        },
      email
    )

randomScimEmail :: MonadRandom m => m Email.Email
randomScimEmail = do
  let typ :: Maybe Text = Nothing
      primary :: Maybe Bool = Nothing -- TODO: where should we catch users with more than one
      -- primary email?
  value :: Email.EmailAddress2 <- do
    localpart <- cs <$> replicateM 15 (getRandomR ('a', 'z'))
    domainpart <- (<> ".com") . cs <$> replicateM 15 (getRandomR ('a', 'z'))
    pure . Email.EmailAddress2 $ Email.unsafeEmailAddress localpart domainpart
  pure Email.Email {..}

randomScimPhone :: MonadRandom m => m Phone.Phone
randomScimPhone = do
  let typ :: Maybe Text = Nothing
  value :: Maybe Text <- do
    let mkdigits n = replicateM n (getRandomR ('0', '9'))
    mini <- mkdigits 8
    maxi <- mkdigits =<< getRandomR (0, 7)
    pure $ Just (cs ('+' : mini <> maxi))
  pure Phone.Phone {..}

----------------------------------------------------------------------------
-- API wrappers

-- | Create a user.
createUser ::
  HasCallStack =>
  ScimToken ->
  Scim.User.User SparTag ->
  TestSpar (Scim.StoredUser SparTag)
createUser tok user = do
  env <- ask
  r <-
    createUser_
      (Just tok)
      user
      (env ^. teSpar)
      <!! const 201 === statusCode
  pure (responseJsonUnsafe r)

-- | Update a user.
updateUser ::
  HasCallStack =>
  ScimToken ->
  UserId ->
  Scim.User.User SparTag ->
  TestSpar (Scim.StoredUser SparTag)
updateUser tok userid user = do
  env <- ask
  r <-
    updateUser_
      (Just tok)
      (Just userid)
      user
      (env ^. teSpar)
      <!! const 200 === statusCode
  pure (responseJsonUnsafe r)

-- | Patch a user
patchUser ::
  HasCallStack =>
  ScimToken ->
  UserId ->
  Scim.PatchOp.PatchOp SparTag ->
  TestSpar (Scim.StoredUser SparTag)
patchUser tok uid patchOp = do
  env <- ask
  r <-
    patchUser_ (Just tok) (Just uid) patchOp (env ^. teSpar)
      <!! const 200 === statusCode
  pure (responseJsonUnsafe r)

-- | Delete a user.
deleteUser ::
  HasCallStack =>
  ScimToken ->
  UserId ->
  TestSpar (Scim.StoredUser SparTag)
deleteUser tok userid = do
  env <- ask
  r <-
    deleteUser_
      (Just tok)
      (Just userid)
      (env ^. teSpar)
      <!! const 200 === statusCode -- status code maybe some other 2xx?
  pure (responseJsonUnsafe r)

-- | List all users.
listUsers ::
  HasCallStack =>
  ScimToken ->
  Maybe Scim.Filter ->
  TestSpar [(Scim.StoredUser SparTag)]
listUsers tok mbFilter = do
  env <- ask
  r <-
    listUsers_
      (Just tok)
      mbFilter
      (env ^. teSpar)
      <!! const 200 === statusCode
  let r' = responseJsonUnsafe r
  when (Scim.totalResults r' /= length (Scim.resources r')) $
    error
      "listUsers: got a paginated result, but pagination \
      \is not supported yet"
  pure (Scim.resources r')

-- | Get a user.
getUser ::
  HasCallStack =>
  ScimToken ->
  UserId ->
  TestSpar (Scim.StoredUser SparTag)
getUser tok userid = do
  env <- ask
  r <-
    getUser_
      (Just tok)
      userid
      (env ^. teSpar)
      <!! const 200 === statusCode
  pure (responseJsonUnsafe r)

-- | Create a SCIM token.
createToken ::
  HasCallStack =>
  UserId ->
  CreateScimToken ->
  TestSpar CreateScimTokenResponse
createToken zusr payload = do
  env <- ask
  r <-
    createToken_
      zusr
      payload
      (env ^. teSpar)
      <!! const 200 === statusCode
  pure (responseJsonUnsafe r)

-- | Delete a SCIM token.
deleteToken ::
  HasCallStack =>
  UserId ->
  -- | Token to delete
  ScimTokenId ->
  TestSpar ()
deleteToken zusr tokenid = do
  env <- ask
  deleteToken_
    zusr
    tokenid
    (env ^. teSpar)
    !!! const 204 === statusCode

-- | List SCIM tokens.
listTokens ::
  HasCallStack =>
  UserId ->
  TestSpar ScimTokenList
listTokens zusr = do
  env <- ask
  r <-
    listTokens_
      zusr
      (env ^. teSpar)
      <!! const 200 === statusCode
  pure (responseJsonUnsafe r)

----------------------------------------------------------------------------
-- "Raw" API requests

-- | Create a user.
createUser_ ::
  -- | Authentication
  Maybe ScimToken ->
  -- | User data
  Scim.User.User SparTag ->
  -- | Spar endpoint
  SparReq ->
  TestSpar ResponseLBS
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
updateUser_ ::
  -- | Authentication
  Maybe ScimToken ->
  -- | User to update; when not provided, the request will
  --   return 4xx
  Maybe UserId ->
  -- | User data
  Scim.User.User SparTag ->
  -- | Spar endpoint
  SparReq ->
  TestSpar ResponseLBS
updateUser_ auth muid user spar_ = do
  call . put $
    ( spar_
        . paths (["scim", "v2", "Users"] <> maybeToList (toByteString' <$> muid))
        . scimAuth auth
        . contentScim
        . body (RequestBodyLBS . Aeson.encode $ user)
        . acceptScim
    )

-- | Patch a user
patchUser_ :: Maybe ScimToken -> Maybe UserId -> Scim.PatchOp.PatchOp SparTag -> SparReq -> TestSpar ResponseLBS
patchUser_ auth muid patchop spar_ =
  call . patch $
    ( spar_
        . paths (["scim", "v2", "Users"] <> maybeToList (toByteString' <$> muid))
        . scimAuth auth
        . contentScim
        . body (RequestBodyLBS . Aeson.encode $ patchop)
        . acceptScim
    )

-- | Update a user.
deleteUser_ ::
  -- | Authentication
  Maybe ScimToken ->
  -- | User to update; when not provided, the request will return 4xx
  Maybe UserId ->
  -- | Spar endpoint
  SparReq ->
  TestSpar ResponseLBS
deleteUser_ auth uid spar_ = do
  call . delete $
    ( spar_
        . paths (["scim", "v2", "Users"] <> (toByteString' <$> maybeToList uid))
        . scimAuth auth
        . contentScim
        . acceptScim
    )

-- | List all users.
listUsers_ ::
  -- | Authentication
  Maybe ScimToken ->
  -- | Predicate to filter the results
  Maybe Scim.Filter ->
  -- | Spar endpoint
  SparReq ->
  TestSpar ResponseLBS
listUsers_ auth mbFilter spar_ = do
  call . get $
    ( spar_
        . paths ["scim", "v2", "Users"]
        . queryItem' "filter" (toByteString' . Scim.renderFilter <$> mbFilter)
        . scimAuth auth
        . acceptScim
    )

filterBy :: Text -> Text -> Filter.Filter
filterBy name value = Filter.FilterAttrCompare (Filter.topLevelAttrPath name) Filter.OpEq (Filter.ValString value)

filterForStoredUser :: HasCallStack => Scim.StoredUser SparTag -> Filter.Filter
filterForStoredUser = filterBy "externalId" . fromJust . Scim.User.externalId . Scim.value . Scim.thing

-- | Get one user.
getUser_ ::
  -- | Authentication
  Maybe ScimToken ->
  -- | User
  UserId ->
  -- | Spar endpoint
  SparReq ->
  TestSpar ResponseLBS
getUser_ auth userid spar_ = do
  call . get $
    ( spar_
        . paths ["scim", "v2", "Users", toByteString' userid]
        . scimAuth auth
        . acceptScim
    )

-- | Create a SCIM token.
createToken_ ::
  -- | User
  UserId ->
  CreateScimToken ->
  -- | Spar endpoint
  SparReq ->
  TestSpar ResponseLBS
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
deleteToken_ ::
  -- | User
  UserId ->
  -- | Token to delete
  ScimTokenId ->
  -- | Spar endpoint
  SparReq ->
  TestSpar ResponseLBS
deleteToken_ userid tokenid spar_ = do
  call . delete $
    ( spar_
        . paths ["scim", "auth-tokens"]
        . queryItem "id" (toByteString' tokenid)
        . zUser userid
    )

-- | List SCIM tokens.
listTokens_ ::
  -- | User
  UserId ->
  -- | Spar endpoint
  SparReq ->
  TestSpar ResponseLBS
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
scimUserId :: Scim.StoredUser SparTag -> UserId
scimUserId = Scim.id . Scim.thing

-- | There are a number of user types that all partially map on each other. This class
-- provides a uniform interface to data stored in those types.
--
-- Each field might be present or not present. For each present field this class provides an
-- accessor (if the field is not present in the type, the accessor will be 'Nothing').
--
-- In cases like 'maybeUserId' the accessor returns a raw value, because a user always has a
-- 'UserId'. In cases like 'maybeHandle' the accessor returns a 'Maybe', because a user may or
-- may not have a handle.
--
-- Note: we don't compare rich info here, because 'User' doesn't contain it. However, we have
-- separate tests for rich info that cover that.
class IsUser u where
  maybeUserId :: Maybe (u -> UserId)
  maybeHandle :: Maybe (u -> Maybe Handle)
  maybeName :: Maybe (u -> Maybe Name)
  maybeTenant :: Maybe (u -> Maybe SAML.Issuer)
  maybeSubject :: Maybe (u -> Maybe SAML.NameID)

  -- | Some types (e.g. 'Scim.User.User') have a subject ID as a raw string, i.e. not in a
  -- structured form. Having 'maybeSubjectRaw' available allows us to compare things like
  -- SCIM 'Scim.User.User' and a Brig 'User', even though they store subject IDs in a
  -- different way.
  maybeSubjectRaw :: Maybe (u -> Maybe Text)

-- | 'ValidScimUser' is tested in ScimSpec.hs exhaustively with literal inputs, so here we assume it
-- is correct and don't aim to verify that name, handle, etc correspond to ones in 'vsuUser'.
instance IsUser ValidScimUser where
  maybeUserId = Nothing
  maybeHandle = Just (Just . view vsuHandle)
  maybeName = Just (view vsuName)
  maybeTenant = Just (Just . view (vsuUserRef . SAML.uidTenant))
  maybeSubject = Just (Just . view (vsuUserRef . SAML.uidSubject))
  maybeSubjectRaw = Just (SAML.shortShowNameID . view (vsuUserRef . SAML.uidSubject))

instance IsUser (WrappedScimStoredUser SparTag) where
  maybeUserId = Just $ scimUserId . fromWrappedScimStoredUser
  maybeHandle = maybeHandle <&> _wrappedStoredUserToWrappedUser
  maybeName = maybeName <&> _wrappedStoredUserToWrappedUser
  maybeTenant = maybeTenant <&> _wrappedStoredUserToWrappedUser
  maybeSubject = maybeSubject <&> _wrappedStoredUserToWrappedUser
  maybeSubjectRaw = maybeSubjectRaw <&> _wrappedStoredUserToWrappedUser

_wrappedStoredUserToWrappedUser :: (WrappedScimUser tag -> a) -> (WrappedScimStoredUser tag -> a)
_wrappedStoredUserToWrappedUser f = f . WrappedScimUser . Scim.value . Scim.thing . fromWrappedScimStoredUser

instance IsUser (WrappedScimUser SparTag) where
  maybeUserId = Nothing
  maybeHandle = Just (Just . Handle . Scim.User.userName . fromWrappedScimUser)
  maybeName = Just (fmap Name . Scim.User.displayName . fromWrappedScimUser)
  maybeTenant = Nothing
  maybeSubject = Nothing
  maybeSubjectRaw = Just $ Scim.User.externalId . fromWrappedScimUser

instance IsUser User where
  maybeUserId = Just userId
  maybeHandle = Just userHandle
  maybeName = Just (Just . userDisplayName)
  maybeTenant = Just (fmap (view SAML.uidTenant) . urefFromBrig)
  maybeSubject = Just (fmap (view SAML.uidSubject) . urefFromBrig)
  maybeSubjectRaw = Just (SAML.shortShowNameID . view SAML.uidSubject <=< urefFromBrig)

-- | For all properties that are present in both @u1@ and @u2@, check that they match.
--
-- Example:
--
--   * 'maybeHandle' is @Nothing@ for one type and @Just ...@ for the other type -> ok.
--
--   * 'maybeHandle' is @Just ...@ for both types, but one user has a handle and the other
--     doesn't (or the handles are different) -> fail.
userShouldMatch ::
  (HasCallStack, MonadIO m, IsUser u1, IsUser u2) =>
  u1 ->
  u2 ->
  m ()
userShouldMatch u1 u2 = liftIO $ do
  check "userId" maybeUserId
  check "handle" maybeHandle
  check "name" maybeName
  check "tenant" maybeTenant
  check "subject" maybeSubject
  check "subject (raw)" maybeSubjectRaw
  where
    check ::
      (Eq a, Show a) =>
      Text -> -- field name
      (forall u. IsUser u => Maybe (u -> a)) -> -- accessor (polymorphic)
      IO ()
    check field getField = case (getField <&> ($ u1), getField <&> ($ u2)) of
      (Just a1, Just a2) -> (field, a1) `shouldBe` (field, a2)
      _ -> pure ()

urefFromBrig :: User -> Maybe SAML.UserRef
urefFromBrig brigUser = case userIdentity brigUser of
  Just (SSOIdentity ssoid _ _) -> case Intra.fromUserSSOId ssoid of
    Right uref -> Just uref
    Left e ->
      error $
        "urefFromBrig: bad SSO id: "
          <> "UserSSOId = "
          <> show ssoid
          <> ", error = "
          <> e
  _ -> Nothing

-- | The spar scim implementation makes use of its right to drop a lot of attributes on the
-- floor.  This function calls the spar functions that do that.  This allows us to express
-- what we expect a user that comes back from spar to look like in terms of what it looked
-- like when we sent it there.
whatSparReturnsFor :: HasCallStack => IdP -> Int -> Scim.User.User SparTag -> Either String (Scim.User.User SparTag)
whatSparReturnsFor idp richInfoSizeLimit = either (Left . show) (Right . synthesizeScimUser) . validateScimUser' idp richInfoSizeLimit

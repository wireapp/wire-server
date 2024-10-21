{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- Disabling to stop warnings on HasCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module API.User.Util where

import Bilge hiding (accept, timeout)
import Bilge.Assert
import Brig.ZAuth (Token)
import Cassandra qualified as DB
import Codec.MIME.Type qualified as MIME
import Control.Lens (preview, (^?))
import Control.Monad.Catch (MonadCatch)
import Data.Aeson hiding (json)
import Data.Aeson.Lens
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Conversion
import Data.ByteString.Lazy qualified as LB
import Data.Code qualified as Code
import Data.Domain
import Data.Handle (parseHandle)
import Data.Id
import Data.Kind
import Data.List1 qualified as List1
import Data.Qualified
import Data.Range (unsafeRange)
import Data.String.Conversions
import Data.Vector qualified as Vec
import Data.ZAuth.Token qualified as ZAuth
import Imports
import Test.Tasty.Cannon qualified as WS
import Test.Tasty.HUnit
import Util
import Wire.API.Asset
import Wire.API.Connection
import Wire.API.Event.Conversation qualified as Conv
import Wire.API.Event.LeaveReason
import Wire.API.Federation.API.Brig qualified as F
import Wire.API.Federation.Component
import Wire.API.Internal.Notification (Notification (..))
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Routes.MultiTablePaging (LocalOrRemoteTable, MultiTablePagingState)
import Wire.API.Team.Feature (IsFeatureConfig, featureNameBS)
import Wire.API.Team.Feature qualified as Public
import Wire.API.User
import Wire.API.User qualified as Public
import Wire.API.User.Activation
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Client.DPoPAccessToken (Proof)
import Wire.API.User.Handle
import Wire.VerificationCode qualified as Code
import Wire.VerificationCodeStore.Cassandra qualified as VerificationCodeStore

newtype ConnectionLimit = ConnectionLimit Int64

checkHandles ::
  (MonadHttp m, HasCallStack) =>
  Brig ->
  UserId ->
  [Text] ->
  Word ->
  m ResponseLBS
checkHandles brig uid hs num =
  let hs' = unsafeRange hs
      num' = unsafeRange num
      js = RequestBodyLBS $ encode $ CheckHandles hs' num'
   in post (brig . path "/users/handles" . contentJson . zUser uid . body js)

randomUserWithHandle ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
  Brig ->
  m User
randomUserWithHandle brig = do
  u <- randomUser brig
  setRandomHandle brig u

setRandomHandle ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
  Brig ->
  User ->
  m User
setRandomHandle brig user = do
  h <- randomHandle
  put
    ( brig
        . path "/self/handle"
        . contentJson
        . zUser (userId user)
        . zConn "c"
        . body (RequestBodyLBS . encode $ HandleUpdate h)
    )
    !!! const 200
      === statusCode
  pure user {userHandle = Just . fromJust . parseHandle $ h}

-- Note: This actually _will_ send out an email, so we ensure that the email
--       used here has a domain 'simulator.amazonses.com'.
registerUser :: (MonadIO m, MonadHttp m) => Text -> Brig -> m ResponseLBS
registerUser name brig = do
  e <- randomEmail
  let p =
        RequestBodyLBS . encode $
          object
            [ "name" .= name,
              "email" .= fromEmail e,
              "password" .= defPassword
            ]
  post (brig . path "/register" . contentJson . body p)

activateEmail :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> EmailAddress -> m ()
activateEmail brig email = do
  act <- getActivationCode brig (Left email)
  case act of
    Nothing -> liftIO $ assertFailure "missing activation key/code"
    Just kc ->
      activate brig kc !!! do
        const 200 === statusCode
        const (Just False) === fmap activatedFirst . responseJsonMaybe

checkEmail :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> UserId -> EmailAddress -> m ()
checkEmail brig uid expectedEmail =
  get (brig . path "/self" . zUser uid) !!! do
    const 200 === statusCode
    const (Just expectedEmail) === (userEmail <=< responseJsonMaybe)

initiateEmailUpdateLogin :: Brig -> EmailAddress -> Login -> UserId -> (MonadIO m, MonadCatch m, MonadHttp m) => m ResponseLBS
initiateEmailUpdateLogin brig email loginCreds uid = do
  (cky, tok) <- do
    rsp <-
      login brig loginCreds PersistentCookie
        <!! const 200 === statusCode
    pure (decodeCookie rsp, decodeToken rsp)
  initiateEmailUpdateCreds brig email (cky, tok) uid

initiateEmailUpdateCreds :: Brig -> EmailAddress -> (Bilge.Cookie, Brig.ZAuth.Token ZAuth.Access) -> UserId -> (MonadHttp m) => m ResponseLBS
initiateEmailUpdateCreds brig email (cky, tok) uid = do
  put $
    unversioned
      . brig
      . path "/access/self/email"
      . cookie cky
      . header "Authorization" ("Bearer " <> toByteString' tok)
      . zUser uid
      . Bilge.json (EmailUpdate email)

initiateEmailUpdateNoSend :: (MonadHttp m, MonadIO m, MonadCatch m) => Brig -> EmailAddress -> UserId -> m ResponseLBS
initiateEmailUpdateNoSend brig email uid =
  let emailUpdate = RequestBodyLBS . encode $ EmailUpdate email
   in put (brig . path "/i/self/email" . contentJson . zUser uid . body emailUpdate)
        <!! const 202 === statusCode

removeBlacklist :: Brig -> EmailAddress -> (MonadIO m, MonadHttp m) => m ()
removeBlacklist brig email =
  void $ delete (brig . path "/i/users/blacklist" . queryItem "email" (toByteString' email))

getClient :: Brig -> UserId -> ClientId -> (MonadHttp m) => m ResponseLBS
getClient brig u c =
  get $
    brig
      . paths ["clients", toByteString' c]
      . zUser u

putClient ::
  (MonadHttp m, HasCallStack) =>
  Brig ->
  UserId ->
  ClientId ->
  MLSPublicKeys ->
  m ResponseLBS
putClient brig uid c keys =
  put $
    brig
      . paths ["clients", toByteString' c]
      . zUser uid
      . json (UpdateClient [] Nothing Nothing Nothing keys)

getClientCapabilities :: Brig -> UserId -> ClientId -> (MonadHttp m) => m ResponseLBS
getClientCapabilities brig u c =
  get $
    brig
      . paths ["clients", toByteString' c, "capabilities"]
      . zUser u

getUserClientsUnqualified :: Brig -> UserId -> (MonadHttp m) => m ResponseLBS
getUserClientsUnqualified brig uid =
  get $
    apiVersion "v1"
      . brig
      . paths ["users", toByteString' uid, "clients"]
      . zUser uid

getUserClientsQualified :: Brig -> UserId -> Domain -> UserId -> (MonadHttp m) => m ResponseLBS
getUserClientsQualified brig zusr domain uid =
  get $
    brig
      . paths ["users", toByteString' domain, toByteString' uid, "clients"]
      . zUser zusr

deleteClient :: Brig -> UserId -> ClientId -> Maybe Text -> (MonadHttp m) => m ResponseLBS
deleteClient brig u c pw =
  delete $
    brig
      . paths ["clients", toByteString' c]
      . zUser u
      . zConn "conn"
      . contentJson
      . body payload
  where
    payload =
      RequestBodyLBS . encode . object . maybeToList $
        fmap ("password" .=) pw

listConnections :: (HasCallStack) => Brig -> UserId -> (MonadHttp m) => m ResponseLBS
listConnections brig u =
  get $
    apiVersion "v1"
      . brig
      . path "connections"
      . zUser u

listAllConnections :: (MonadHttp m, HasCallStack) => Brig -> UserId -> Maybe Int -> Maybe (MultiTablePagingState "Connections" LocalOrRemoteTable) -> m ResponseLBS
listAllConnections brig u size state =
  post $
    brig
      . path "list-connections"
      . zUser u
      . expect2xx
      . contentJson
      . body
        ( RequestBodyLBS $
            encode $
              object
                [ "size" .= size,
                  "paging_state" .= state
                ]
        )

getConnectionQualified :: (MonadHttp m) => Brig -> UserId -> Qualified UserId -> m ResponseLBS
getConnectionQualified brig from (Qualified toUser toDomain) =
  get $
    brig
      . paths ["connections", toByteString' toDomain, toByteString' toUser]
      . zUser from

setProperty :: Brig -> UserId -> ByteString -> Value -> (MonadHttp m) => m ResponseLBS
setProperty brig u k v =
  put $
    brig
      . paths ["/properties", k]
      . zUser u
      . zConn "conn"
      . contentJson
      . body (RequestBodyLBS $ encode v)

getProperty :: Brig -> UserId -> ByteString -> (MonadHttp m) => m ResponseLBS
getProperty brig u k =
  get $
    brig
      . paths ["/properties", k]
      . zUser u

countCookies :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> UserId -> CookieLabel -> m (Maybe Int)
countCookies brig u label = do
  r <-
    get
      ( brig
          . path "/cookies"
          . queryItem "labels" (toByteString' label)
          . header "Z-User" (toByteString' u)
      )
      <!! const 200
        === statusCode
  pure $ Vec.length <$> (preview (key "cookies" . _Array) =<< responseJsonMaybe @Value r)

assertConnections :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> UserId -> [ConnectionStatus] -> m ()
assertConnections brig u connections =
  listConnections brig u !!! do
    const 200 === statusCode
    const (Just True) === fmap (check . map status . clConnections) . responseJsonMaybe
  where
    check xs = all (`elem` xs) connections
    status c = ConnectionStatus (ucFrom c) (qUnqualified $ ucTo c) (ucStatus c)

assertConnectionQualified :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> UserId -> Qualified UserId -> Relation -> m ()
assertConnectionQualified brig u1 qu2 rel =
  getConnectionQualified brig u1 qu2 !!! do
    const 200 === statusCode
    const (Right rel) === fmap ucStatus . responseJsonEither

receiveConnectionAction ::
  (HasCallStack) =>
  Brig ->
  FedClient 'Brig ->
  UserId ->
  Qualified UserId ->
  F.RemoteConnectionAction ->
  Maybe F.RemoteConnectionAction ->
  Relation ->
  Http ()
receiveConnectionAction brig fedBrigClient uid1 quid2 action expectedReaction expectedRel = do
  res <-
    runFedClient @"send-connection-action" fedBrigClient (qDomain quid2) $
      F.NewConnectionRequest (qUnqualified quid2) Nothing uid1 action
  liftIO $ do
    res @?= F.NewConnectionResponseOk expectedReaction
  assertConnectionQualified brig uid1 quid2 expectedRel

assertEmailVisibility :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> User -> User -> Bool -> m ()
assertEmailVisibility brig a b visible =
  get (apiVersion "v1" . brig . paths ["users", pack . show $ userId b] . zUser (userId a)) !!! do
    const 200 === statusCode
    if visible
      then const (Just (userEmail b)) === fmap userEmail . responseJsonMaybe
      else const Nothing === (userEmail <=< responseJsonMaybe)

uploadAsset ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
  CargoHold ->
  UserId ->
  AssetSettings ->
  ByteString ->
  m (Response (Maybe LByteString))
uploadAsset c usr sts dat = do
  let ct = MIME.Type (MIME.Application "text") []
      mpb = buildMultipartBody sts ct (LB.fromStrict dat)
  post
    ( c
        . path "/assets"
        . zUser usr
        . zConn "conn"
        . content "multipart/mixed"
        . lbytes (toLazyByteString mpb)
    )
    <!! const 201
      === statusCode

downloadAsset ::
  (MonadHttp m) =>
  CargoHold ->
  UserId ->
  Qualified AssetKey ->
  m (Response (Maybe LB.ByteString))
downloadAsset c usr ast =
  get
    ( c
        . paths ["/assets", toByteString' (qDomain ast), toByteString' (qUnqualified ast)]
        . zUser usr
        . zConn "conn"
    )

matchDeleteUserNotification :: Qualified UserId -> Notification -> Assertion
matchDeleteUserNotification quid n = do
  let j = Object $ List1.head (ntfPayload n)
  let etype = j ^? key "type" . _String
  let eUnqualifiedId = maybeFromJSON =<< j ^? key "id"
  let eQualifiedId = maybeFromJSON =<< j ^? key "qualified_id"
  etype @?= Just "user.delete"
  eUnqualifiedId @?= Just (qUnqualified quid)
  eQualifiedId @?= Just quid

matchConvLeaveNotification :: Qualified ConvId -> Qualified UserId -> [Qualified UserId] -> EdMemberLeftReason -> Notification -> IO ()
matchConvLeaveNotification conv remover removeds reason n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  Conv.evtConv e @?= conv
  Conv.evtType e @?= Conv.MemberLeave
  Conv.evtFrom e @?= remover
  sorted (Conv.evtData e) @?= sorted (Conv.EdMembersLeave reason (Conv.QualifiedUserIdList removeds))
  where
    sorted (Conv.EdMembersLeave r (Conv.QualifiedUserIdList m)) = Conv.EdMembersLeave r (Conv.QualifiedUserIdList (sort m))
    sorted x = x

generateVerificationCode :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> Public.SendVerificationCode -> m ()
generateVerificationCode = generateVerificationCodeExpect 200

generateVerificationCodeExpect :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Int -> Brig -> Public.SendVerificationCode -> m ()
generateVerificationCodeExpect expectedStatus brig req = do
  generateVerificationCode' brig req !!! const expectedStatus === statusCode

generateVerificationCode' :: (MonadHttp m, HasCallStack) => Brig -> Public.SendVerificationCode -> m ResponseLBS
generateVerificationCode' brig req = do
  let js = RequestBodyLBS $ encode req
  post (brig . paths ["verification-code", "send"] . contentJson . body js)

setTeamSndFactorPasswordChallenge :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Galley -> TeamId -> Public.FeatureStatus -> m ()
setTeamSndFactorPasswordChallenge galley tid status = do
  let js = RequestBodyLBS $ encode $ Public.Feature status Public.SndFactorPasswordChallengeConfig
  put (galley . paths ["i", "teams", toByteString' tid, "features", featureNameBS @Public.SndFactorPasswordChallengeConfig] . contentJson . body js) !!! const 200 === statusCode

setTeamFeatureLockStatus ::
  forall (cfg :: Type) m.
  ( MonadCatch m,
    MonadIO m,
    MonadHttp m,
    HasCallStack,
    IsFeatureConfig cfg
  ) =>
  Galley ->
  TeamId ->
  Public.LockStatus ->
  m ()
setTeamFeatureLockStatus galley tid status =
  put (galley . paths ["i", "teams", toByteString' tid, "features", Public.featureNameBS @cfg, toByteString' status]) !!! const 200 === statusCode

lookupCode :: (MonadIO m) => DB.ClientState -> Code.Key -> Code.Scope -> m (Maybe Code.Code)
lookupCode db k = liftIO . DB.runClient db . VerificationCodeStore.lookupCodeImpl k

getNonce ::
  (MonadHttp m) =>
  Brig ->
  UserId ->
  ClientId ->
  m ResponseLBS
getNonce = nonce get

headNonce ::
  (MonadHttp m) =>
  Brig ->
  UserId ->
  ClientId ->
  m ResponseLBS
headNonce = nonce Bilge.head

nonce :: ((Request -> c) -> t) -> (Request -> c) -> UserId -> ClientId -> t
nonce m brig uid cid =
  m
    ( brig
        . paths ["clients", toByteString' cid, "nonce"]
        . zUser uid
    )

headNonceNginz ::
  (MonadHttp m) =>
  Nginz ->
  ZAuth.Token ZAuth.Access ->
  ClientId ->
  m ResponseLBS
headNonceNginz nginz t cid =
  Bilge.head
    ( nginz
        . paths ["clients", toByteString' cid, "nonce"]
        . header "Authorization" ("Bearer " <> toByteString' t)
    )

createAccessToken :: (MonadHttp m, HasCallStack) => Brig -> UserId -> Text -> ClientId -> Maybe Proof -> m ResponseLBS
createAccessToken brig uid h cid mProof =
  post $
    brig
      . paths ["clients", toByteString' cid, "access-token"]
      . zUser uid
      . header "Z-Host" (cs h)
      . maybe id (header "DPoP" . toByteString') mProof

createAccessTokenNginz :: (MonadHttp m, HasCallStack) => Nginz -> ZAuth.Token ZAuth.Access -> ClientId -> Maybe Proof -> m ResponseLBS
createAccessTokenNginz n t cid mProof =
  post $
    unversioned
      . n
      . paths ["clients", toByteString' cid, "access-token"]
      . header "Authorization" ("Bearer " <> toByteString' t)
      . maybe id (header "DPoP" . toByteString') mProof

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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
import qualified Brig.Code as Code
import Brig.Effects.CodeStore
import Brig.Effects.CodeStore.Cassandra
import Brig.Options (Opts)
import Brig.Types.Team.LegalHold (LegalHoldClientRequest (..))
import qualified Brig.ZAuth
import qualified Cassandra as DB
import qualified Codec.MIME.Type as MIME
import Control.Lens (preview, (^?))
import Control.Monad.Catch (MonadCatch)
import Data.Aeson hiding (json)
import Data.Aeson.Lens
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as LB
import Data.Domain
import Data.Handle (Handle (Handle))
import Data.Id hiding (client)
import Data.Kind
import qualified Data.List1 as List1
import Data.Misc (PlainTextPassword (..))
import Data.Qualified
import Data.Range (unsafeRange)
import qualified Data.Text.Ascii as Ascii
import qualified Data.Vector as Vec
import qualified Data.ZAuth.Token as ZAuth
import Federation.Util (withTempMockFederator)
import Federator.MockServer (FederatedRequest (..))
import GHC.TypeLits (KnownSymbol)
import Imports
import Polysemy
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import Util
import Wire.API.Asset
import Wire.API.Connection
import qualified Wire.API.Event.Conversation as Conv
import qualified Wire.API.Federation.API.Brig as F
import Wire.API.Federation.Component
import Wire.API.Internal.Notification (Notification (..))
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Routes.MultiTablePaging (LocalOrRemoteTable, MultiTablePagingState)
import Wire.API.Team.Feature (featureNameBS)
import qualified Wire.API.Team.Feature as Public
import Wire.API.User
import qualified Wire.API.User as Public
import Wire.API.User.Activation
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Client.DPoPAccessToken (Proof)
import Wire.API.User.Client.Prekey
import Wire.API.User.Handle
import Wire.API.User.Password

newtype ConnectionLimit = ConnectionLimit Int64

checkHandles ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
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
  pure user {userHandle = Just (Handle h)}

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

createRandomPhoneUser :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> m (UserId, Phone)
createRandomPhoneUser brig = do
  usr <- randomUser brig
  let uid = userId usr
  phn <- liftIO randomPhone
  -- update phone
  let phoneUpdate = RequestBodyLBS . encode $ PhoneUpdate phn
  put (brig . path "/self/phone" . contentJson . zUser uid . zConn "c" . body phoneUpdate)
    !!! (const 202 === statusCode)
  -- activate
  act <- getActivationCode brig (Right phn)
  case act of
    Nothing -> liftIO $ assertFailure "missing activation key/code"
    Just kc -> activate brig kc !!! const 200 === statusCode
  -- check new phone
  get (brig . path "/self" . zUser uid) !!! do
    const 200 === statusCode
    const (Just phn) === (userPhone <=< responseJsonMaybe)
  pure (uid, phn)

initiatePasswordReset :: Brig -> Email -> (MonadIO m, MonadHttp m) => m ResponseLBS
initiatePasswordReset brig email =
  post
    ( brig
        . path "/password-reset"
        . contentJson
        . body (RequestBodyLBS . encode $ NewPasswordReset (Left email))
    )

activateEmail :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> Email -> m ()
activateEmail brig email = do
  act <- getActivationCode brig (Left email)
  case act of
    Nothing -> liftIO $ assertFailure "missing activation key/code"
    Just kc ->
      activate brig kc !!! do
        const 200 === statusCode
        const (Just False) === fmap activatedFirst . responseJsonMaybe

checkEmail :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> UserId -> Email -> m ()
checkEmail brig uid expectedEmail =
  get (brig . path "/self" . zUser uid) !!! do
    const 200 === statusCode
    const (Just expectedEmail) === (userEmail <=< responseJsonMaybe)

initiateEmailUpdateLogin :: Brig -> Email -> Login -> UserId -> (MonadIO m, MonadCatch m, MonadHttp m) => m ResponseLBS
initiateEmailUpdateLogin brig email loginCreds uid = do
  (cky, tok) <- do
    rsp <-
      login brig loginCreds PersistentCookie
        <!! const 200 === statusCode
    pure (decodeCookie rsp, decodeToken rsp)
  initiateEmailUpdateCreds brig email (cky, tok) uid

initiateEmailUpdateCreds :: Brig -> Email -> (Bilge.Cookie, Brig.ZAuth.Token ZAuth.Access) -> UserId -> (MonadIO m, MonadCatch m, MonadHttp m) => m ResponseLBS
initiateEmailUpdateCreds brig email (cky, tok) uid = do
  put $
    unversioned
      . brig
      . path "/access/self/email"
      . cookie cky
      . header "Authorization" ("Bearer " <> toByteString' tok)
      . zUser uid
      . Bilge.json (EmailUpdate email)

initiateEmailUpdateNoSend :: Brig -> Email -> UserId -> (MonadIO m, MonadHttp m) => m ResponseLBS
initiateEmailUpdateNoSend brig email uid =
  let emailUpdate = RequestBodyLBS . encode $ EmailUpdate email
   in put (brig . path "/i/self/email" . contentJson . zUser uid . body emailUpdate)

preparePasswordReset ::
  (MonadIO m, MonadHttp m) =>
  Brig ->
  DB.ClientState ->
  Email ->
  UserId ->
  PlainTextPassword ->
  m CompletePasswordReset
preparePasswordReset brig cs email uid newpw = do
  let qry = queryItem "email" (toByteString' email)
  r <- get $ brig . path "/i/users/password-reset-code" . qry
  let lbs = fromMaybe "" $ responseBody r
  let Just pwcode = PasswordResetCode . Ascii.unsafeFromText <$> (lbs ^? key "code" . _String)
  ident <- PasswordResetIdentityKey <$> runSem (mkPasswordResetKey uid)
  let complete = CompletePasswordReset ident pwcode newpw
  pure complete
  where
    runSem = liftIO . runFinal @IO . interpretClientToIO cs . codeStoreToCassandra @DB.Client

completePasswordReset :: Brig -> CompletePasswordReset -> (MonadIO m, MonadHttp m) => m ResponseLBS
completePasswordReset brig passwordResetData =
  post
    ( brig
        . path "/password-reset/complete"
        . contentJson
        . body (RequestBodyLBS $ encode passwordResetData)
    )

removeBlacklist :: Brig -> Email -> (MonadIO m, MonadHttp m) => m ()
removeBlacklist brig email =
  void $ delete (brig . path "/i/users/blacklist" . queryItem "email" (toByteString' email))

getClient :: Brig -> UserId -> ClientId -> (MonadIO m, MonadHttp m) => m ResponseLBS
getClient brig u c =
  get $
    brig
      . paths ["clients", toByteString' c]
      . zUser u

putClient ::
  (MonadIO m, MonadHttp m, HasCallStack) =>
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

getClientCapabilities :: Brig -> UserId -> ClientId -> (MonadIO m, MonadHttp m) => m ResponseLBS
getClientCapabilities brig u c =
  get $
    brig
      . paths ["clients", toByteString' c, "capabilities"]
      . zUser u

getUserClientsUnqualified :: Brig -> UserId -> (MonadIO m, MonadHttp m) => m ResponseLBS
getUserClientsUnqualified brig uid =
  get $
    apiVersion "v1"
      . brig
      . paths ["users", toByteString' uid, "clients"]
      . zUser uid

getUserClientsQualified :: Brig -> UserId -> Domain -> UserId -> (MonadIO m, MonadHttp m) => m ResponseLBS
getUserClientsQualified brig zusr domain uid =
  get $
    brig
      . paths ["users", toByteString' domain, toByteString' uid, "clients"]
      . zUser zusr

deleteClient :: Brig -> UserId -> ClientId -> Maybe Text -> (MonadIO m, MonadHttp m) => m ResponseLBS
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

listConnections :: HasCallStack => Brig -> UserId -> (MonadIO m, MonadHttp m) => m ResponseLBS
listConnections brig u =
  get $
    apiVersion "v1"
      . brig
      . path "connections"
      . zUser u

listAllConnections :: (MonadIO m, MonadHttp m, HasCallStack) => Brig -> UserId -> Maybe Int -> Maybe (MultiTablePagingState "Connections" LocalOrRemoteTable) -> m ResponseLBS
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

getConnectionQualified :: (MonadIO m, MonadHttp m) => Brig -> UserId -> Qualified UserId -> m ResponseLBS
getConnectionQualified brig from (Qualified toUser toDomain) =
  get $
    brig
      . paths ["connections", toByteString' toDomain, toByteString' toUser]
      . zUser from

setProperty :: Brig -> UserId -> ByteString -> Value -> (MonadIO m, MonadHttp m) => m ResponseLBS
setProperty brig u k v =
  put $
    brig
      . paths ["/properties", k]
      . zUser u
      . zConn "conn"
      . contentJson
      . body (RequestBodyLBS $ encode v)

getProperty :: Brig -> UserId -> ByteString -> (MonadIO m, MonadHttp m) => m ResponseLBS
getProperty brig u k =
  get $
    brig
      . paths ["/properties", k]
      . zUser u

deleteProperty :: Brig -> UserId -> ByteString -> (MonadIO m, MonadHttp m) => m ResponseLBS
deleteProperty brig u k =
  delete $
    brig
      . paths ["/properties", k]
      . zConn "conn"
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
  HasCallStack =>
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
      F.NewConnectionRequest (qUnqualified quid2) uid1 action
  liftIO $ do
    res @?= F.NewConnectionResponseOk expectedReaction
  assertConnectionQualified brig uid1 quid2 expectedRel

sendConnectionAction ::
  HasCallStack =>
  Brig ->
  Opts ->
  UserId ->
  Qualified UserId ->
  Maybe F.RemoteConnectionAction ->
  Relation ->
  Http ()
sendConnectionAction brig opts uid1 quid2 reaction expectedRel = do
  let mockConnectionResponse = F.NewConnectionResponseOk reaction
      mockResponse = encode mockConnectionResponse
  (res, reqs) <-
    liftIO . withTempMockFederator opts mockResponse $
      postConnectionQualified brig uid1 quid2

  liftIO $ do
    req <- assertOne reqs
    frTargetDomain req @?= qDomain quid2
    frComponent req @?= Brig
    frRPC req @?= "send-connection-action"
    eitherDecode (frBody req)
      @?= Right (F.NewConnectionRequest uid1 (qUnqualified quid2) F.RemoteConnect)

  liftIO $ assertBool "postConnectionQualified failed" $ statusCode res `elem` [200, 201]
  assertConnectionQualified brig uid1 quid2 expectedRel

sendConnectionUpdateAction ::
  HasCallStack =>
  Brig ->
  Opts ->
  UserId ->
  Qualified UserId ->
  Maybe F.RemoteConnectionAction ->
  Relation ->
  Http ()
sendConnectionUpdateAction brig opts uid1 quid2 reaction expectedRel = do
  let mockConnectionResponse = F.NewConnectionResponseOk reaction
      mockResponse = encode mockConnectionResponse
  void $
    liftIO . withTempMockFederator opts mockResponse $
      putConnectionQualified brig uid1 quid2 expectedRel !!! const 200 === statusCode
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
  (MonadIO m, MonadHttp m) =>
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

requestLegalHoldDevice :: Brig -> UserId -> UserId -> LastPrekey -> (MonadIO m, MonadHttp m) => m ResponseLBS
requestLegalHoldDevice brig requesterId targetUserId lastPrekey' =
  post $
    brig
      . paths ["i", "clients", "legalhold", toByteString' targetUserId, "request"]
      . contentJson
      . body payload
  where
    payload =
      RequestBodyLBS . encode $
        LegalHoldClientRequest requesterId lastPrekey'

deleteLegalHoldDevice :: Brig -> UserId -> (MonadIO m, MonadHttp m) => m ResponseLBS
deleteLegalHoldDevice brig uid =
  delete $
    brig
      . paths ["i", "clients", "legalhold", toByteString' uid]
      . contentJson

matchDeleteUserNotification :: Qualified UserId -> Notification -> Assertion
matchDeleteUserNotification quid n = do
  let j = Object $ List1.head (ntfPayload n)
  let etype = j ^? key "type" . _String
  let eUnqualifiedId = maybeFromJSON =<< j ^? key "id"
  let eQualifiedId = maybeFromJSON =<< j ^? key "qualified_id"
  etype @?= Just "user.delete"
  eUnqualifiedId @?= Just (qUnqualified quid)
  eQualifiedId @?= Just quid

matchConvLeaveNotification :: Qualified ConvId -> Qualified UserId -> [Qualified UserId] -> Notification -> IO ()
matchConvLeaveNotification conv remover removeds n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  Conv.evtConv e @?= conv
  Conv.evtType e @?= Conv.MemberLeave
  Conv.evtFrom e @?= remover
  sorted (Conv.evtData e) @?= sorted (Conv.EdMembersLeave (Conv.QualifiedUserIdList removeds))
  where
    sorted (Conv.EdMembersLeave (Conv.QualifiedUserIdList m)) = Conv.EdMembersLeave (Conv.QualifiedUserIdList (sort m))
    sorted x = x

generateVerificationCode :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> Public.SendVerificationCode -> m ()
generateVerificationCode = generateVerificationCodeExpect 200

generateVerificationCodeExpect :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Int -> Brig -> Public.SendVerificationCode -> m ()
generateVerificationCodeExpect expectedStatus brig req = do
  generateVerificationCode' brig req !!! const expectedStatus === statusCode

generateVerificationCode' :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> Public.SendVerificationCode -> m ResponseLBS
generateVerificationCode' brig req = do
  let js = RequestBodyLBS $ encode req
  post (brig . paths ["verification-code", "send"] . contentJson . body js)

setTeamSndFactorPasswordChallenge :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Galley -> TeamId -> Public.FeatureStatus -> m ()
setTeamSndFactorPasswordChallenge galley tid status = do
  let js = RequestBodyLBS $ encode $ Public.WithStatusNoLock status Public.SndFactorPasswordChallengeConfig Public.FeatureTTLUnlimited
  put (galley . paths ["i", "teams", toByteString' tid, "features", featureNameBS @Public.SndFactorPasswordChallengeConfig] . contentJson . body js) !!! const 200 === statusCode

setTeamFeatureLockStatus ::
  forall (cfg :: Type) m.
  ( MonadCatch m,
    MonadIO m,
    MonadHttp m,
    HasCallStack,
    Public.IsFeatureConfig cfg,
    KnownSymbol (Public.FeatureSymbol cfg)
  ) =>
  Galley ->
  TeamId ->
  Public.LockStatus ->
  m ()
setTeamFeatureLockStatus galley tid status =
  put (galley . paths ["i", "teams", toByteString' tid, "features", Public.featureNameBS @cfg, toByteString' status]) !!! const 200 === statusCode

lookupCode :: MonadIO m => DB.ClientState -> Code.Key -> Code.Scope -> m (Maybe Code.Code)
lookupCode db k = liftIO . DB.runClient db . Code.lookup k

getNonce ::
  (MonadIO m, MonadHttp m) =>
  Brig ->
  UserId ->
  ClientId ->
  m ResponseLBS
getNonce = nonce get

headNonce ::
  (MonadIO m, MonadHttp m) =>
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

createAccessToken :: (MonadIO m, MonadHttp m, HasCallStack) => Brig -> UserId -> ClientId -> Maybe Proof -> m ResponseLBS
createAccessToken brig uid cid mProof =
  post
    ( brig
        . paths ["clients", toByteString' cid, "access-token"]
        . zUser uid
        . maybe id (header "DPoP" . toByteString') mProof
    )

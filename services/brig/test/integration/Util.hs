{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Disabling to stop warnings on HasCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
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
-- for SES notifications
{-# OPTIONS_GHC -fno-warn-orphans -Wno-deprecations #-}

module Util where

import Bilge hiding (host, port)
import Bilge.Assert
import Brig.AWS.Types
import Brig.App (applog, fsWatcher, sftEnv, turnEnv)
import Brig.Calling as Calling
import Brig.Options qualified as Opt
import Brig.Run qualified as Run
import Brig.Types.Activation
import Brig.ZAuth qualified as ZAuth
import Control.Concurrent.Async
import Control.Exception (throw)
import Control.Lens ((^.), (^?), (^?!))
import Control.Monad.Catch (MonadCatch, MonadMask)
import Control.Monad.Catch qualified as Catch
import Control.Monad.State qualified as State
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Class qualified as MonadState
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Control.Retry
import Data.Aeson hiding (json)
import Data.Aeson.Lens (key, _Integral, _JSON, _String)
import Data.Aeson.Types qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Conversion
import Data.Code qualified as Code
import Data.Default
import Data.Domain (Domain (..), domainText, mkDomain)
import Data.Handle (Handle (..))
import Data.Id
import Data.List1 (List1)
import Data.List1 qualified as List1
import Data.Misc
import Data.Proxy
import Data.Qualified
import Data.Range
import Data.Sequence qualified as Seq
import Data.String.Conversions
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.Ascii qualified as Ascii
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.ZAuth.Token qualified as ZAuth
import Federator.MockServer qualified as Mock
import GHC.TypeLits
import Galley.Types.Conversations.One2One (one2OneConvId)
import Imports
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Media.MediaType
import Network.HTTP.Media.RenderHeader (renderHeader)
import Network.HTTP.Types (Method, http11, renderQuery)
import Network.HTTP.Types qualified as HTTP
import Network.Wai (Application)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Test (Session)
import Network.Wai.Test qualified as WaiTest
import Network.Wai.Utilities.Error qualified as Wai
import OpenSSL.BN (randIntegerZeroToNMinusOne)
import Servant.Client (ClientError (FailureResponse))
import Servant.Client qualified as Servant
import Servant.Client.Core (RunClient (throwClientError))
import Servant.Client.Core qualified as Servant
import Servant.Client.Core.Request qualified as ServantRequest
import System.Exit
import System.Process
import System.Random (randomIO, randomRIO)
import System.Timeout qualified as System
import Test.QuickCheck (arbitrary, generate)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Cannon
import Test.Tasty.Cannon qualified as WS
import Test.Tasty.HUnit
import Test.Tasty.Pending (flakyTestCase)
import Text.Printf (printf)
import UnliftIO.Async qualified as Async
import Util.Options
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Conversation.Role (roleNameWireAdmin)
import Wire.API.Federation.API
import Wire.API.Federation.Domain
import Wire.API.Federation.Version
import Wire.API.Internal.Notification
import Wire.API.Routes.MultiTablePaging
import Wire.API.Team.Member hiding (userId)
import Wire.API.User hiding (AccountStatus (..))
import Wire.API.User qualified as WU
import Wire.API.User.Activation
import Wire.API.User.Auth
import Wire.API.User.Auth.LegalHold
import Wire.API.User.Auth.Sso
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.VersionInfo

type Brig = Request -> Request

type Cannon = Request -> Request

type Gundeck = Request -> Request

type CargoHold = Request -> Request

type Galley = Request -> Request

type Nginz = Request -> Request

type Spar = Request -> Request

data FedClient (comp :: Component) = FedClient HTTP.Manager Endpoint

-- | Note: Apply this function last when composing (Request -> Request) functions
apiVersion :: ByteString -> Request -> Request
apiVersion newVersion r = r {HTTP.path = setVersion newVersion (HTTP.path r)}
  where
    setVersion :: ByteString -> ByteString -> ByteString
    setVersion v p =
      let p' = removeSlash' p
       in v <> "/" <> fromMaybe p' (removeVersionPrefix p')

removeSlash' :: ByteString -> ByteString
removeSlash' s = case B8.uncons s of
  Just ('/', s') -> s'
  _ -> s

removeVersionPrefix :: ByteString -> Maybe ByteString
removeVersionPrefix bs = do
  let (x, s) = B8.splitAt 1 bs
  guard (x == B8.pack "v")
  (_, s') <- B8.readInteger s
  pure (B8.tail s')

-- | Note: Apply this function last when composing (Request -> Request) functions
unversioned :: Request -> Request
unversioned r =
  r
    { HTTP.path =
        maybe
          (HTTP.path r)
          (B8.pack "/" <>)
          (removeVersionPrefix . removeSlash' $ HTTP.path r)
    }

runFedClient ::
  forall (name :: Symbol) comp api.
  ( HasUnsafeFedEndpoint comp api name,
    Servant.HasClient Servant.ClientM api
  ) =>
  FedClient comp ->
  Domain ->
  Servant.Client Http api
runFedClient (FedClient mgr ep) domain =
  Servant.hoistClient (Proxy @api) (servantClientMToHttp domain) $
    Servant.clientIn (Proxy @api) (Proxy @Servant.ClientM)
  where
    servantClientMToHttp :: Domain -> Servant.ClientM a -> Http a
    servantClientMToHttp originDomain action = liftIO $ do
      let brigHost = Text.unpack $ ep ^. host
          brigPort = fromInteger . toInteger $ ep ^. port
          baseUrl = Servant.BaseUrl Servant.Http brigHost brigPort "/federation"
          clientEnv = Servant.ClientEnv mgr baseUrl Nothing (makeClientRequest originDomain)
      eitherRes <- Servant.runClientM action clientEnv
      case eitherRes of
        Right res -> pure res
        Left err -> assertFailure $ "Servant client failed with: " <> show err

    makeClientRequest :: Domain -> Servant.BaseUrl -> Servant.Request -> IO HTTP.Request
    makeClientRequest originDomain burl req = do
      req' <- Servant.defaultMakeClientRequest burl req
      pure
        req'
          { HTTP.requestHeaders =
              HTTP.requestHeaders req'
                <> [ (originDomainHeaderName, toByteString' originDomain),
                     (versionHeader, toByteString' (versionInt (maxBound :: Version)))
                   ]
          }

instance ToJSON SESBounceType where
  toJSON BounceUndetermined = String "Undetermined"
  toJSON BouncePermanent = String "Permanent"
  toJSON BounceTransient = String "Transient"

instance ToJSON SESNotification where
  toJSON (MailBounce typ ems) =
    object
      [ "notificationType" .= ("Bounce" :: Text),
        "bounce"
          .= object
            [ "bouncedRecipients" .= fmap (\e -> object ["emailAddress" .= e]) ems,
              "bounceType" .= typ
            ]
      ]
  toJSON (MailComplaint ems) =
    object
      [ "notificationType" .= ("Complaint" :: Text),
        "complaint"
          .= object
            [ "complainedRecipients" .= fmap (\e -> object ["emailAddress" .= e]) ems
            ]
      ]

test :: Manager -> TestName -> Http a -> TestTree
test m n h = testCase n (void $ runHttpT m h)

flakyTest :: Manager -> TestName -> Http a -> TestTree
flakyTest m n h = flakyTestCase n (void $ runHttpT m h)

twoRandomUsers :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> m (Qualified UserId, UserId, Qualified UserId, UserId)
twoRandomUsers brig = do
  quid1 <- userQualifiedId <$> randomUser brig
  quid2 <- userQualifiedId <$> randomUser brig
  let uid1 = qUnqualified quid1
      uid2 = qUnqualified quid2
  pure (quid1, uid1, quid2, uid2)

localAndRemoteUser ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
  Brig ->
  m (UserId, Qualified UserId)
localAndRemoteUser brig = do
  uid1 <- userId <$> randomUser brig
  quid2 <- fakeRemoteUser
  pure (uid1, quid2)

localAndRemoteUserWithConvId ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
  Brig ->
  Bool ->
  m (UserId, Qualified UserId, Qualified ConvId)
localAndRemoteUserWithConvId brig shouldBeLocal = do
  quid <- userQualifiedId <$> randomUser brig
  let go = do
        other <- Qualified <$> randomId <*> pure (Domain "far-away.example.com")
        let convId = one2OneConvId BaseProtocolProteusTag quid other
            isLocalUntagged = qDomain quid == qDomain convId
        if shouldBeLocal == isLocalUntagged
          then pure (qUnqualified quid, other, convId)
          else go
  go

fakeRemoteUser :: (HasCallStack, MonadIO m) => m (Qualified UserId)
fakeRemoteUser = Qualified <$> randomId <*> pure (Domain "far-away.example.com")

randomClient :: (MonadIO m) => m ClientId
randomClient = liftIO $ generate arbitrary

randomUser ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
  Brig ->
  m User
randomUser = randomUser' True

randomUser' ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
  Bool ->
  Brig ->
  m User
randomUser' hasPwd brig = do
  n <- fromName <$> randomName
  createUser' hasPwd n brig

createUser ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
  Text ->
  Brig ->
  m User
createUser = createUser' True

createUser' ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
  Bool ->
  Text ->
  Brig ->
  m User
createUser' hasPwd name brig = do
  r <-
    postUser' hasPwd True name True False Nothing Nothing brig
      <!! const 201 === statusCode
  responseJsonError r

createUserWithEmail :: (HasCallStack) => Text -> Email -> Brig -> Http User
createUserWithEmail name email brig = do
  r <-
    postUserWithEmail True True name (Just email) False Nothing Nothing brig
      <!! const 201 === statusCode
  responseJsonError r

createUserUntrustedEmail :: (HasCallStack) => Text -> Brig -> Http User
createUserUntrustedEmail name brig = do
  email <- randomUntrustedEmail
  createUserWithEmail name email brig

createAnonUser :: (HasCallStack) => Text -> Brig -> Http User
createAnonUser = createAnonUserExpiry Nothing

createAnonUserExpiry :: (HasCallStack) => Maybe Integer -> Text -> Brig -> Http User
createAnonUserExpiry expires name brig = do
  let p = RequestBodyLBS . encode $ object ["name" .= name, "expires_in" .= expires]
  r <- post (brig . path "/register" . contentJson . body p) <!! const 201 === statusCode
  responseJsonError r

requestActivationCode :: (HasCallStack) => Brig -> Int -> Either Email Phone -> Http ()
requestActivationCode brig expectedStatus ep =
  post (brig . path "/activate/send" . contentJson . body (RequestBodyLBS . encode $ bdy ep))
    !!! const expectedStatus === statusCode
  where
    bdy (Left e) = object ["email" .= fromEmail e]
    bdy (Right p) = object ["phone" .= fromPhone p]

getActivationCode ::
  (MonadCatch m, MonadHttp m, HasCallStack) =>
  Brig ->
  Either Email Phone ->
  m (Maybe (ActivationKey, ActivationCode))
getActivationCode brig ep = do
  let qry = either (queryItem "email" . toByteString') (queryItem "phone" . toByteString') ep
  r <- get $ brig . path "/i/users/activation-code" . qry
  let lbs = fromMaybe "" $ responseBody r
  let akey = ActivationKey . Ascii.unsafeFromText <$> (lbs ^? key "key" . _String)
  let acode = ActivationCode . Ascii.unsafeFromText <$> (lbs ^? key "code" . _String)
  pure $ (,) <$> akey <*> acode

assertUpdateNotification :: (HasCallStack) => WS.WebSocket -> UserId -> UserUpdate -> IO ()
assertUpdateNotification ws uid upd = WS.assertMatch (5 # Second) ws $ \n -> do
  let j = Object $ List1.head (ntfPayload n)
  j ^? key "type" . _String @?= Just "user.update"
  let u = j ^?! key "user"
  u ^? key "id" . _String @?= Just (UUID.toText (toUUID uid))
  u ^? key "name" . _String @?= fromName <$> uupName upd
  u ^? key "text_status" . _String @?= fromTextStatus <$> uupTextStatus upd
  u ^? key "accent_id" . _Integral @?= fromColourId <$> uupAccentId upd
  u ^? key "assets" @?= Just (toJSON (uupAssets upd))

--------------------------------------------------------------------------------
-- API Operations

getConnection :: Brig -> UserId -> UserId -> Http ResponseLBS
getConnection brig from to =
  get $
    apiVersion "v1"
      . brig
      . paths ["/connections", toByteString' to]
      . zUser from
      . zConn "conn"

-- | More flexible variant of 'createUser' (see above).
postUser :: Text -> Bool -> Bool -> Maybe UserSSOId -> Maybe TeamId -> Brig -> Http ResponseLBS
postUser = postUser' True True

-- | Use @postUser' True False@ instead of 'postUser' if you want to send broken bodies to test error
-- messages.  Or @postUser' False True@ if you want to validate the body, but not set a password.
postUser' ::
  (MonadIO m, MonadHttp m, HasCallStack) =>
  Bool ->
  Bool ->
  Text ->
  Bool ->
  Bool ->
  Maybe UserSSOId ->
  Maybe TeamId ->
  Brig ->
  m ResponseLBS
postUser' hasPassword validateBody name haveEmail havePhone ssoid teamid brig = do
  email <-
    if haveEmail
      then Just <$> randomEmail
      else pure Nothing
  postUserWithEmail hasPassword validateBody name email havePhone ssoid teamid brig

-- | More flexible variant of 'createUserUntrustedEmail' (see above).
postUserWithEmail ::
  (MonadIO m, MonadHttp m, HasCallStack) =>
  Bool ->
  Bool ->
  Text ->
  Maybe Email ->
  Bool ->
  Maybe UserSSOId ->
  Maybe TeamId ->
  Brig ->
  m ResponseLBS
postUserWithEmail hasPassword validateBody name email havePhone ssoid teamid brig = do
  phone <-
    if havePhone
      then Just <$> randomPhone
      else pure Nothing
  let o =
        object $
          [ "name" .= name,
            "email" .= (fromEmail <$> email),
            "phone" .= phone,
            "cookie" .= defCookieLabel,
            "sso_id" .= ssoid,
            "team_id" .= teamid
          ]
            <> ["password" .= defPassword | hasPassword]
      p = case Aeson.parse parseJSON o of
        Aeson.Success (p_ :: NewUser) -> p_
        bad -> error $ show (bad, o)
      bdy = if validateBody then Bilge.json p else Bilge.json o
  post (brig . path "/i/users" . bdy)

postUserInternal :: Object -> Brig -> Http User
postUserInternal payload brig = do
  rs <- post (brig . path "/i/users" . contentJson . body (RequestBodyLBS $ encode payload)) <!! const 201 === statusCode
  maybe (error $ "postUserInternal: Failed to decode user due to: " ++ show rs) pure (responseJsonMaybe rs)

postUserRegister :: Object -> Brig -> Http User
postUserRegister payload brig = do
  rs <- postUserRegister' payload brig <!! const 201 === statusCode
  maybe (error $ "postUserRegister: Failed to decode user due to: " ++ show rs) pure (responseJsonMaybe rs)

postUserRegister' :: (MonadHttp m) => Object -> Brig -> m ResponseLBS
postUserRegister' payload brig = do
  post (brig . path "/register" . contentJson . body (RequestBodyLBS $ encode payload))

deleteUser :: (MonadHttp m, HasCallStack) => UserId -> Maybe PlainTextPassword6 -> Brig -> m ResponseLBS
deleteUser u p brig =
  delete $
    brig
      . path "/self"
      . contentJson
      . zUser u
      . body (RequestBodyLBS (encode (mkDeleteUser p)))

deleteUserInternal :: UserId -> Brig -> Http ResponseLBS
deleteUserInternal u brig =
  delete $
    brig
      . paths ["/i/users", toByteString' u]

activate :: Brig -> ActivationPair -> (MonadHttp m) => m ResponseLBS
activate brig (k, c) =
  get $
    brig
      . path "activate"
      . queryItem "key" (toByteString' k)
      . queryItem "code" (toByteString' c)

getSelfProfile :: (MonadIO m, MonadCatch m, MonadHttp m, HasCallStack) => Brig -> UserId -> m SelfProfile
getSelfProfile brig usr = do
  responseJsonError =<< get (brig . path "/self" . zUser usr)

getUser :: Brig -> UserId -> UserId -> Http ResponseLBS
getUser brig zusr usr =
  get $
    apiVersion "v1"
      . brig
      . paths ["users", toByteString' usr]
      . zUser zusr

-- | NB: you can also use nginz as the first argument here.  The type aliases are compatible,
-- and so are the end-points.  This is important in tests where the cookie must come from the
-- nginz domain, so it can be passed back to it.
login :: Brig -> Login -> CookieType -> (MonadHttp m) => m ResponseLBS
login b l t =
  let js = RequestBodyLBS (encode l)
   in post $
        unversioned
          . b
          . path "/login"
          . contentJson
          . (if t == PersistentCookie then queryItem "persist" "true" else id)
          . body js

ssoLogin :: Brig -> SsoLogin -> CookieType -> Http ResponseLBS
ssoLogin b l t =
  let js = RequestBodyLBS (encode l)
   in post $
        b
          . path "/i/sso-login"
          . contentJson
          . (if t == PersistentCookie then queryItem "persist" "true" else id)
          . body js

legalHoldLogin :: Brig -> LegalHoldLogin -> CookieType -> Http ResponseLBS
legalHoldLogin b l t =
  let js = RequestBodyLBS (encode l)
   in post $
        b
          . path "/i/legalhold-login"
          . contentJson
          . (if t == PersistentCookie then queryItem "persist" "true" else id)
          . body js

decodeCookie :: (HasCallStack) => Response a -> Bilge.Cookie
decodeCookie = fromMaybe (error "missing zuid cookie") . getCookie "zuid"

decodeToken :: (HasCallStack) => Response (Maybe LByteString) -> ZAuth.Token ZAuth.Access
decodeToken = decodeToken'

decodeToken' :: (HasCallStack, ZAuth.AccessTokenLike a) => Response (Maybe LByteString) -> ZAuth.Token a
decodeToken' r = fromMaybe (error "invalid access_token") $ do
  x <- responseBody r
  t <- x ^? key "access_token" . _String
  fromByteString (encodeUtf8 t)

data LoginCodeType = LoginCodeSMS | LoginCodeVoice
  deriving (Eq)

postConnection :: Brig -> UserId -> UserId -> (MonadHttp m) => m ResponseLBS
postConnection brig from to =
  post $
    apiVersion "v1"
      . brig
      . path "/connections"
      . contentJson
      . body payload
      . zUser from
      . zConn "conn"
  where
    payload =
      RequestBodyLBS
        . encode
        $ ConnectionRequest to (unsafeRange "some conv name")

postConnectionQualified :: (MonadHttp m) => Brig -> UserId -> Qualified UserId -> m ResponseLBS
postConnectionQualified brig from (Qualified toUser toDomain) =
  post $
    brig
      . paths ["/connections", toByteString' toDomain, toByteString' toUser]
      . contentJson
      . zUser from
      . zConn "conn"

putConnection :: Brig -> UserId -> UserId -> Relation -> (MonadHttp m) => m ResponseLBS
putConnection brig from to r =
  put $
    apiVersion "v1"
      . brig
      . paths ["/connections", toByteString' to]
      . contentJson
      . body payload
      . zUser from
      . zConn "conn"
  where
    payload = RequestBodyLBS . encode $ object ["status" .= r]

putConnectionQualified :: Brig -> UserId -> Qualified UserId -> Relation -> (MonadHttp m) => m ResponseLBS
putConnectionQualified brig from (Qualified to toDomain) r =
  put $
    brig
      . paths ["/connections", toByteString' toDomain, toByteString' to]
      . contentJson
      . body payload
      . zUser from
      . zConn "conn"
  where
    payload = RequestBodyLBS . encode $ object ["status" .= r]

connectUsers :: (MonadIO m, MonadHttp m) => Brig -> UserId -> List1 UserId -> m ()
connectUsers b u = mapM_ connectTo
  where
    connectTo v = do
      void $ postConnection b u v
      void $ putConnection b v u Accepted

putHandle ::
  (MonadHttp m, HasCallStack) =>
  Brig ->
  UserId ->
  Text ->
  m ResponseLBS
putHandle brig usr h =
  put $
    brig
      . path "/self/handle"
      . contentJson
      . body payload
      . zUser usr
      . zConn "conn"
  where
    payload = RequestBodyLBS . encode $ object ["handle" .= h]

createUserWithHandle :: Brig -> (MonadCatch m, MonadIO m, MonadHttp m) => m (Handle, User)
createUserWithHandle brig = do
  u <- randomUser brig
  h <- randomHandle
  void $ putHandle brig (userId u) h
  userWithHandle <- selfUser <$> getSelfProfile brig (userId u)
  -- Verify if creating user and setting handle succeeded
  let handle = fromJust (userHandle userWithHandle)
  liftIO $ assertEqual "creating user with handle should return handle" h (fromHandle handle)
  -- We return the handle separately in this function for convenience
  -- of not needing to de-maybe-ify the user handle field of the user object
  -- when using this function.
  pure (handle, userWithHandle)

addClient ::
  (MonadHttp m, HasCallStack) =>
  Brig ->
  UserId ->
  NewClient ->
  m ResponseLBS
addClient brig uid new = post (addClientReq brig uid new)

addClientInternal :: Brig -> UserId -> NewClient -> Http ResponseLBS
addClientInternal brig uid new =
  post $
    brig
      . paths ["i", "clients", toByteString' uid]
      . contentJson
      . body (RequestBodyLBS $ encode new)

addClientReq :: Brig -> UserId -> NewClient -> (Request -> Request)
addClientReq brig uid new =
  brig
    . path "/clients"
    . zUser uid
    . zConn "conn"
    . contentJson
    . body (RequestBodyLBS $ encode new)

defNewClient :: ClientType -> [Prekey] -> LastPrekey -> NewClient
defNewClient = defNewClientWithVerificationCode Nothing

defNewClientWithVerificationCode :: Maybe Code.Value -> ClientType -> [Prekey] -> LastPrekey -> NewClient
defNewClientWithVerificationCode mbCode ty pks lpk =
  (newClient ty lpk)
    { newClientPassword = Just defPassword,
      newClientPrekeys = pks,
      newClientLabel = Just "Test Device",
      newClientModel = Just "Test Model",
      newClientVerificationCode = mbCode
    }

getPreKey ::
  (MonadHttp m, HasCallStack) =>
  Brig ->
  UserId ->
  UserId ->
  ClientId ->
  m ResponseLBS
getPreKey brig zusr u c =
  get $
    apiVersion "v1"
      . brig
      . paths ["users", toByteString' u, "prekeys", toByteString' c]
      . zUser zusr

getTeamMember ::
  (MonadIO m, MonadCatch m, MonadHttp m, HasCallStack) =>
  UserId ->
  TeamId ->
  Galley ->
  m TeamMember
getTeamMember u tid galley =
  responseJsonError
    =<< get
      ( galley
          . paths ["i", "teams", toByteString' tid, "members", toByteString' u]
          . zUser u
          . expect2xx
      )

getConversationQualified :: (MonadHttp m) => Galley -> UserId -> Qualified ConvId -> m ResponseLBS
getConversationQualified galley usr cnv =
  get $
    galley
      . paths ["conversations", toByteString' (qDomain cnv), toByteString' (qUnqualified cnv)]
      . zAuthAccess usr "conn"

createConversation :: (MonadHttp m) => Galley -> UserId -> [Qualified UserId] -> m ResponseLBS
createConversation galley zusr usersToAdd = do
  let conv =
        NewConv
          []
          usersToAdd
          (checked "gossip")
          mempty
          Nothing
          Nothing
          Nothing
          Nothing
          roleNameWireAdmin
          BaseProtocolProteusTag
  post $
    galley
      . path "/conversations"
      . zUser zusr
      . zConn "conn"
      . json conv

listConvIdsFirstPage :: (MonadHttp m) => Galley -> UserId -> m ResponseLBS
listConvIdsFirstPage galley zusr = do
  let req = GetMultiTablePageRequest (toRange (Proxy @1000)) Nothing :: GetPaginatedConversationIds
  post $
    galley
      . path "/conversations/list-ids"
      . zUser zusr
      . zConn "conn"
      . json req

listConvs ::
  (MonadHttp m) =>
  Galley ->
  UserId ->
  Range 1 1000 [Qualified ConvId] ->
  m ResponseLBS
listConvs galley zusr convs = do
  post $
    apiVersion "v1"
      . galley
      . path "/conversations/list/v2"
      . zUser zusr
      . zConn "conn"
      . json (ListConversations convs)

isMember :: Galley -> Local UserId -> ConvId -> (MonadIO m, MonadHttp m) => m Bool
isMember g usr cnv = do
  res <-
    get $
      g
        . paths ["i", "conversations", toByteString' cnv, "members", toByteString' (tUnqualified usr)]
        . expect2xx
  case responseJsonMaybe res of
    Nothing -> pure False
    Just m -> pure (tUntagged usr == memId m)

getStatus :: (HasCallStack) => Brig -> UserId -> (MonadIO m, MonadHttp m) => m WU.AccountStatus
getStatus brig u =
  (^?! key "status" . (_JSON @Value @WU.AccountStatus))
    . (responseJsonUnsafe @Value)
    <$> get
      ( brig
          . paths ["i", "users", toByteString' u, "status"]
          . expect2xx
      )

chkStatus :: (HasCallStack) => Brig -> UserId -> WU.AccountStatus -> (MonadIO m, MonadHttp m, MonadCatch m) => m ()
chkStatus brig u s =
  get (brig . paths ["i", "users", toByteString' u, "status"]) !!! do
    const 200 === statusCode
    const (Just (toJSON s)) === ((^? key "status") <=< responseBody)

setStatus :: Brig -> UserId -> WU.AccountStatus -> Http ()
setStatus brig u s =
  let js = RequestBodyLBS . encode $ AccountStatusUpdate s
   in put
        ( brig
            . paths ["i", "users", toByteString' u, "status"]
            . contentJson
            . body js
        )
        !!! const 200
          === statusCode

--------------------------------------------------------------------------------
-- Utilities

queryRange :: Maybe ByteString -> Maybe Int -> Request -> Request
queryRange start size =
  maybe id (queryItem "size" . pack . show) size
    . maybe id (queryItem "start") start

maybeFromJSON :: (FromJSON a) => Value -> Maybe a
maybeFromJSON v = case fromJSON v of
  Success a -> Just a
  _ -> Nothing

zAuthAccess :: UserId -> ByteString -> Request -> Request
zAuthAccess u c = header "Z-Type" "access" . zUser u . zConn c

zUser :: UserId -> Request -> Request
zUser = header "Z-User" . B8.pack . show

zClient :: ClientId -> Request -> Request
zClient = header "Z-Client" . toByteString'

zConn :: ByteString -> Request -> Request
zConn = header "Z-Connection"

mkEmailRandomLocalSuffix :: (MonadIO m) => Text -> m Email
mkEmailRandomLocalSuffix e = do
  uid <- liftIO UUID.nextRandom
  case parseEmail e of
    Just (Email loc dom) -> pure $ Email (loc <> "+" <> UUID.toText uid) dom
    Nothing -> error $ "Invalid email address: " ++ Text.unpack e

-- | Generate emails that are in the trusted whitelist of domains whose @+@ suffices count for email
-- disambiguation.  See also: 'Brig.Email.mkEmailKey'.
randomEmail :: (MonadIO m) => m Email
randomEmail = mkSimulatorEmail "success"

-- | To test the behavior of email addresses with untrusted domains (two emails are equal even if
-- their local part after @+@ differs), we need to generate them.
randomUntrustedEmail :: (MonadIO m) => m Email
randomUntrustedEmail = do
  -- NOTE: local part cannot be longer than 64 octets
  rd <- liftIO (randomIO :: IO Integer)
  pure $ Email (Text.pack $ show rd) "zinfra.io"

mkSimulatorEmail :: (MonadIO m) => Text -> m Email
mkSimulatorEmail loc = mkEmailRandomLocalSuffix (loc <> "@simulator.amazonses.com")

randomPhone :: (MonadIO m) => m Phone
randomPhone = liftIO $ do
  nrs <- map show <$> replicateM 14 (randomRIO (0, 9) :: IO Int)
  let phone = parsePhone . Text.pack $ "+0" ++ concat nrs
  pure $ fromMaybe (error "Invalid random phone#") phone

randomActivationCode :: (HasCallStack, MonadIO m) => m ActivationCode
randomActivationCode =
  liftIO $
    ActivationCode
      . Ascii.unsafeFromText
      . T.pack
      . printf "%06d"
      <$> randIntegerZeroToNMinusOne 1000000

updatePhone :: (HasCallStack) => Brig -> UserId -> Phone -> Http ()
updatePhone brig uid phn = do
  -- update phone
  let phoneUpdate = RequestBodyLBS . encode $ PhoneUpdate phn
  put (brig . path "/self/phone" . contentJson . zUser uid . zConn "c" . body phoneUpdate) !!! do
    const 400 === statusCode
    const (Just "invalid-phone") === fmap Wai.label . responseJsonMaybe

defEmailLogin :: Email -> Login
defEmailLogin e = emailLogin e defPassword (Just defCookieLabel)

emailLogin :: Email -> PlainTextPassword6 -> Maybe CookieLabel -> Login
emailLogin e pw cl = PasswordLogin (PasswordLoginData (LoginByEmail e) pw cl Nothing)

somePrekeys :: [Prekey]
somePrekeys =
  [ Prekey (PrekeyId 1) "pQABAQECoQBYIOjl7hw0D8YRNqkkBQETCxyr7/ywE/2R5RWcUPM+GJACA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 2) "pQABAQICoQBYIGoXawUQWQ9ZW+MXhvuo9ALOBUjLff8S5VdAokN29C1OA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 3) "pQABAQMCoQBYIEjdt+YWd3lHmG8pamULLMubAMZw556IO8kW7s1MLFytA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 4) "pQABAQQCoQBYIPIaOA3Xqfk4Lh2/pU88Owd2eW5eplHpywr+Mx4QGyiMA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 5) "pQABAQUCoQBYIHnafNR4Gh3ID71lYzToewEVag4EKskDFq+gaeraOlSJA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 6) "pQABAQYCoQBYIFXUkVftE7kK22waAzhOjOmJVex3EBTU8RHZFx2o1Ed8A6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 7) "pQABAQcCoQBYIDXdN8VlKb5lbgPmoDPLPyqNIEyShG4oT/DlW0peRRZUA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 8) "pQABAQgCoQBYIJH1ewvIVV3yGqQvdr/QM9HARzMgo5ksOTRyKEuN2aZzA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 9) "pQABAQkCoQBYIFcAnXdx0M1Q1hoDDfgMK9r+Zchn8YlVHHaQwQYhRk1dA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 10) "pQABAQoCoQBYIGs3vyxwmzEZ+qKNy4wpFkxc+Bgkb0D76ZEbxeeh/9DVA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 11) "pQABAQsCoQBYIGUiBeOJALP5dkMduUZ/u6MDhHNrsrBUa3f0YlSSWZbzA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 12) "pQABAQwCoQBYIMp6QNNTPDZgL3DSSD/QWWnBI7LsTZp2RhY/HLqnIwRZA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 13) "pQABAQ0CoQBYIJXSSUrE5RCNyB5pg+m6vGwK7RvJ+rs9dsdHitxnfDhuA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 14) "pQABAQ4CoQBYIHmtOX7jCKBHFDysb4H0z/QWoCSaEyjerZaT/HOP8bgDA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 15) "pQABAQ8CoQBYIIaMCTcPKj2HuYQ7i9ZaxUw9j5Bz8TPjoAaTZ5eB0w1kA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 16) "pQABARACoQBYIHWAOacKuWH81moJVveJ0FSfipWocfspOIBhaU6VLWUsA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 17) "pQABARECoQBYIA8XtUXtnMxQslULnNAeHBIivlLRe/+qdh2j6nTfDAchA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 18) "pQABARICoQBYIGgzg6SzgTTOgnk48pa6y2Rgjy004DkeBo4CMld3Jlr6A6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 19) "pQABARMCoQBYIEoEFiIpCHgn74CAD+GhIfIgbQtdCqQqkOXHWxRlG6Y6A6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 20) "pQABARQCoQBYINVEwTRxNSe0rxZxon4Rifz2l4rtQZn7mHtKYCiFAK9IA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 21) "pQABARUCoQBYIN3aeX2Ayi2rPFbiaYb+O2rdHUpFhzRs2j28pCmbGpflA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 22) "pQABARYCoQBYIJe5OJ17YKQrNmIH3sE++r++4Z5ld36axqAMjjQ3jtQWA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 23) "pQABARcCoQBYIASE94LjK6Raipk/lN/YewouqO+kcQGpxIqP+iW2hyHiA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 24) "pQABARgYAqEAWCBZ222LpS6/99Btlw+83PihrA655skwsNevt//8oz5axQOhAKEAWCCy39UyMEgetquvTo7P19bcyfnWBzQMOEG1v+0wub0magT2",
    Prekey (PrekeyId 25) "pQABARgZAqEAWCDGEwo61w4O8T8lyw0HdoOjGWBKQUNqo6+jSfrPR9alrAOhAKEAWCCy39UyMEgetquvTo7P19bcyfnWBzQMOEG1v+0wub0magT2",
    Prekey (PrekeyId 26) "pQABARgaAqEAWCBMSQoQ6B35plB80i1O3AWlJSftCEbCbju97Iykg5+NWQOhAKEAWCCy39UyMEgetquvTo7P19bcyfnWBzQMOEG1v+0wub0magT2"
  ]

someLastPrekeys :: [LastPrekey]
someLastPrekeys =
  [ lastPrekey "pQABARn//wKhAFggnCcZIK1pbtlJf4wRQ44h4w7/sfSgj5oWXMQaUGYAJ/sDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggwO2any+CjiGP8XFYrY67zHPvLgp+ysY5k7vci57aaLwDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggoChErA5oTI5JT769hJV+VINmU8kougGdYqGd2U7hPa8DoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggPLk4BBJ8THVLGm7r0K7EJITRlJnt6bpNzM9GTNRYcCcDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggqHASsRlZ1i8dESXRXBL2OvR+0yGUtqK9vJfzol1E+osDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggx/N1YhKXSJYJQxhWgHSA4ASaJKIHDJfmEnojfnp9VQ8DoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggVL6QIpoqmtKxmB8HToiAPxfjSDEzJEUAoFKfhXou06YDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggRs74/ViOrHN+aS2RbGCwC0sJv1Sp/Q0pmRB15s9DCBMDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggtNO/hrwzt9M/1X6eK2sG6YFmA7BDqlFMEipbZOsg0vcDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFgg1rZEY6vbAnEz+Ern5kRny/uKiIrXTb/usQxGnceV2HADoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFgg2647mOAVeOdhW57Q1zXDigDxRz/hB8ITFSZ7uo+pXH4DoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggjddbHizABYOY0T6rvJeZCvV20dvTT9BYv95ri9bqSb8DoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggCKT/GspZquUY6vKC4TFvaFqTH1QGG1ptauiaulnfqkUDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggv7bf/kEsTKFDGSgswsywq6AIxBq5AqZbLjDYDHfGjrcDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggUbjGhhh8EwZEPSz+Y31rYNUu7jsRR8dy1F5FSiJXfXEDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFgg/4nz1uHiPBVGFvYjTMwGQ31bSFNctbU0r2nBtpsK9kcDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggwbJDyKl7T3+3Ihc0YF06Dz2J11My5qn7JKG+U+ti8lQDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFgglc6nCoZR2/qjLp0tr7vRyuXqb7ugdHHDadjX7zSl4uMDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFgg5ER8h0/bIADXjBXe/XPKdzekgv6nhJ4hp3vJ3jtTSbUDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggsgV6jq+GuNuvXk+ctHh570cNqEmfPhz34wcYCMCf9xIDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggdQdlPqkBw6+phKhohp3YaWQL710euZDnyMLFwf2cS0oDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggKlsI/snuQMoYcZRw/kN+BobPV5gwYeBClp0Wx9btTGUDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggtruFBClEgdPKvjpHsYLlWMev9L4OmYZwlxbY0NwvzOwDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggRUdh4cuYtFNL46RLnPy65goYInyreStKwsEcY3pPlLkDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggQtT7lLZzH171F4jCbHNwxEAt28FwdQ8Kt2tbxFzPgC0DoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggQeUPM119c+6zRsEupA8zshTfrZiLpXx1Ji0UMMumq9IDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g=="
  ]

defPassword :: PlainTextPassword6
defPassword = plainTextPassword6Unsafe defPasswordText

defPasswordText :: Text
defPasswordText = "topsecretdefaultpassword"

defWrongPassword :: PlainTextPassword6
defWrongPassword = plainTextPassword6Unsafe "not secret"

defCookieLabel :: CookieLabel
defCookieLabel = CookieLabel "auth"

randomBytes :: Int -> IO ByteString
randomBytes n = BS.pack <$> replicateM n randomIO

randomHandle :: (MonadIO m) => m Text
randomHandle = liftIO $ do
  nrs <- replicateM 21 (randomRIO (97, 122)) -- a-z
  pure (Text.pack (map chr nrs))

randomName :: (MonadIO m) => m Name
randomName = randomNameWithMaxLen 128

-- | For testing purposes we restrict ourselves to code points in the
-- Basic Multilingual Plane that are considered to be numbers, letters,
-- punctuation or symbols and ensure the name starts with a "letter".
-- That is in order for the name to be searchable at all, since the standard
-- ElasticSearch tokenizer may otherwise produce an empty list of tokens,
-- e.g. if the name is entirely made of characters from categories that
-- the standard tokenizer considers as word boundaries (or which are
-- simply unassigned code points), yielding no tokens to match and thus
-- no results in search queries.
randomNameWithMaxLen :: (MonadIO m) => Word -> m Name
randomNameWithMaxLen maxLen = liftIO $ do
  len <- randomRIO (2, maxLen)
  chars <- fill len []
  pure $ Name (Text.pack chars)
  where
    fill 0 chars = pure chars
    fill 1 chars = (: chars) <$> randLetter
    fill n chars = do
      c <- randChar
      if isLetter c || isNumber c || isPunctuation c || isSymbol c
        then fill (n - 1) (c : chars)
        else fill n chars
    randChar = chr <$> randomRIO (0x0000, 0xFFFF)
    randLetter = do
      c <- randChar
      if isLetter c
        then pure c
        else randLetter

retryWhileN :: (MonadIO m) => Int -> (a -> Bool) -> m a -> m a
retryWhileN n f m =
  retrying
    (constantDelay 1000000 <> limitRetries n)
    (const (pure . f))
    (const m)

recoverN :: (MonadIO m, MonadMask m) => Int -> m a -> m a
recoverN n m =
  recoverAll
    (constantDelay 1000000 <> limitRetries n)
    (const m)

-- | This is required as any HTTP call made using bilge when running under
-- 'withSettingsOverrides' goes to the server started by
-- 'withSettingsOverrides'. Sometimes, a call needs to be made to another
-- service which is not being mocked, this helper can be used to do that.
--
-- This is just an alias to 'runHttpT' to make the intent clear.
circumventSettingsOverride :: Manager -> HttpT m a -> m a
circumventSettingsOverride = runHttpT

-- | This allows you to run requests against a brig instantiated using the given options.
--   Note that ONLY 'brig' calls should occur within the provided action, calls to other
--   services will fail.
--
--   Beware: (1) Not all async parts of brig are running in this.  (2) other services will
--   see the old, unaltered brig.
withSettingsOverrides :: (MonadIO m, HasCallStack) => Opt.Opts -> WaiTest.Session a -> m a
withSettingsOverrides opts action = liftIO $ do
  (brigApp, env) <- Run.mkApp opts
  sftDiscovery <-
    forM (env ^. sftEnv) $ \sftEnv' ->
      Async.async $ Calling.startSFTServiceDiscovery (env ^. applog) sftEnv'
  turnDiscovery <- Calling.startTurnDiscovery (env ^. applog) (env ^. fsWatcher) (env ^. turnEnv)
  res <- WaiTest.runSession action brigApp
  mapM_ Async.cancel sftDiscovery
  mapM_ Async.cancel turnDiscovery
  pure res

-- | When we remove the customer-specific extension of domain blocking, this test will fail to
-- compile.
withDomainsBlockedForRegistration :: (MonadIO m) => Opt.Opts -> [Text] -> WaiTest.Session a -> m a
withDomainsBlockedForRegistration opts domains sess = do
  let opts' = opts {Opt.optSettings = (Opt.optSettings opts) {Opt.setCustomerExtensions = Just blocked}}
      blocked = Opt.CustomerExtensions (Opt.DomainsBlockedForRegistration (unsafeMkDomain <$> domains))
      unsafeMkDomain = either error id . mkDomain
  withSettingsOverrides opts' sess

-- | Run a probe several times, until a "good" value materializes or until patience runs out
aFewTimes ::
  (HasCallStack, MonadIO m) =>
  -- | Number of retries. Exponentially: 11 ~ total of 2 secs delay, 12 ~ 4 secs delay, ...
  Int ->
  m a ->
  (a -> Bool) ->
  m a
aFewTimes
  retries
  action
  good = do
    retrying
      (exponentialBackoff 1000 <> limitRetries retries)
      (\_ -> pure . not . good)
      (const action)

assertOne :: (HasCallStack, MonadIO m, Show a) => [a] -> m a
assertOne [a] = pure a
assertOne xs = liftIO . assertFailure $ "Expected exactly one element, found " <> show xs

--------------------------------------------------------------------------------

newtype MockT m a = MockT {unMock :: ReaderT (IORef MockState) m a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (IORef MockState), MonadIO)

instance (MonadIO m) => MonadState MockState (MockT m) where
  get = readIORef =<< ask
  put x = do
    ref <- ask
    writeIORef ref x

data ReceivedRequest = ReceivedRequest Method [Text] LByteString

data MockState = MockState
  { receivedRequests :: [ReceivedRequest],
    serverThread :: Async.Async (),
    serverPort :: Integer,
    mockHandler :: ReceivedRequest -> MockT IO Wai.Response
  }

mkMockApp :: IORef MockState -> Application
mkMockApp ref request mkResponse = do
  let action = do
        req <- liftIO $ getReceivedRequest request
        handler <- mockHandler <$> liftIO (readIORef ref)
        response <- handler req
        MonadState.modify (\ms -> ms {receivedRequests = receivedRequests ms <> [req]})
        pure response
  runMockT ref action >>= mkResponse

getReceivedRequest :: Wai.Request -> IO ReceivedRequest
getReceivedRequest r =
  ReceivedRequest (Wai.requestMethod r) (Wai.pathInfo r) <$> Wai.strictRequestBody r

runMockT :: IORef MockState -> MockT m a -> m a
runMockT ref mock = runReaderT (unMock mock) ref

startMockService :: (MonadIO m) => IORef MockState -> ExceptT String m ()
startMockService ref = ExceptT . liftIO $ do
  (sPort, sock) <- Warp.openFreePort
  serverStarted <- newEmptyMVar
  let settings =
        Warp.defaultSettings
          & Warp.setPort sPort
          & Warp.setGracefulCloseTimeout2 0 -- Defaults to 2 seconds, causes server stop to take very long
          & Warp.setBeforeMainLoop (putMVar serverStarted ())
  let app = mkMockApp ref
  serviceThread <- Async.async $ Warp.runSettingsSocket settings sock app
  serverStartedSignal <- System.timeout 10_000_000 (takeMVar serverStarted)
  case serverStartedSignal of
    Nothing -> do
      liftIO $ Async.cancel serviceThread
      pure . Left $ "Failed to start the mock server within 10 seconds on port: " <> show sPort
    _ -> do
      liftIO . modifyIORef ref $ \s -> s {serverThread = serviceThread, serverPort = toInteger sPort}
      pure (Right ())

initState :: MockState
initState = MockState [] (error "server not started") (error "server not started") (error "No mock response provided")

stopMockedService :: (MonadIO m) => IORef MockState -> m ()
stopMockedService ref =
  liftIO $ Async.cancel . serverThread <=< readIORef $ ref

withTempMockedService ::
  (MonadIO m, MonadMask m) =>
  MockState ->
  (ReceivedRequest -> MockT IO Wai.Response) ->
  (MockState -> ExceptT String m a) ->
  ExceptT String m (a, [ReceivedRequest])
withTempMockedService state handler action = do
  ref <- newIORef state
  startMockService ref
  ( do
      liftIO . modifyIORef ref $ \s -> s {mockHandler = handler}
      st <- liftIO $ readIORef ref
      actualResponse <- action st
      st' <- liftIO $ readIORef ref
      pure (actualResponse, receivedRequests st')
    )
    `Catch.finally` stopMockedService ref

assertRight :: (MonadIO m, Show a, HasCallStack) => Either a b -> m b
assertRight = \case
  Left e -> liftIO $ assertFailure $ "Expected Right, got Left: " <> show e
  Right x -> pure x

withMockedGalley :: (MonadIO m, MonadMask m) => Opt.Opts -> (ReceivedRequest -> MockT IO Wai.Response) -> Session a -> m (a, [ReceivedRequest])
withMockedGalley opts handler action =
  assertRight
    <=< runExceptT
    $ withTempMockedService initState handler
    $ \st -> lift $ do
      let opts' =
            opts
              { Opt.galley = Endpoint "127.0.0.1" (fromIntegral (serverPort st))
              }
      withSettingsOverrides opts' action

withMockedFederatorAndGalley ::
  Opt.Opts ->
  Domain ->
  (Mock.FederatedRequest -> IO LByteString) ->
  (ReceivedRequest -> MockT IO Wai.Response) ->
  Session a ->
  IO (a, [Mock.FederatedRequest], [ReceivedRequest])
withMockedFederatorAndGalley opts _domain fedResp galleyHandler action = do
  result <- assertRight
    <=< runExceptT
    $ withTempMockedService initState galleyHandler
    $ \galleyMockState ->
      Mock.withTempMockFederator
        def {Mock.handler = (\r -> pure ("application" // "json", r)) <=< fedResp}
        $ \fedMockPort -> do
          let opts' =
                opts
                  { Opt.galley = Endpoint "127.0.0.1" (fromIntegral (serverPort galleyMockState)),
                    Opt.federatorInternal = Just (Endpoint "127.0.0.1" (fromIntegral fedMockPort))
                  }
          withSettingsOverrides opts' action
  pure (combineResults result)
  where
    combineResults :: ((a, [Mock.FederatedRequest]), [ReceivedRequest]) -> (a, [Mock.FederatedRequest], [ReceivedRequest])
    combineResults ((a, mrr), rr) = (a, mrr, rr)

newtype WaiTestFedClient a = WaiTestFedClient {unWaiTestFedClient :: ReaderT Domain WaiTest.Session a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance Servant.RunClient WaiTestFedClient where
  runRequestAcceptStatus expectedStatuses servantRequest = WaiTestFedClient $ do
    domain <- ask
    let req' = fromServantRequest domain servantRequest
    res <- lift $ WaiTest.srequest req'
    let servantResponse = toServantResponse res
    let status = Servant.responseStatusCode servantResponse
    let statusIsSuccess =
          case expectedStatuses of
            Nothing -> HTTP.statusIsSuccessful status
            Just ex -> status `elem` ex
    unless statusIsSuccess $
      unWaiTestFedClient $
        throwClientError (FailureResponse (bimap (const ()) (\x -> (Servant.BaseUrl Servant.Http "" 80 "", cs (toLazyByteString x))) servantRequest) servantResponse)
    pure servantResponse
  throwClientError = liftIO . throw

instance VersionedMonad v WaiTestFedClient where
  guardVersion _ = pure ()

fromServantRequest :: Domain -> Servant.Request -> WaiTest.SRequest
fromServantRequest domain r =
  let pathBS = "/federation" <> cs (toLazyByteString (Servant.requestPath r))
      bodyBS = case Servant.requestBody r of
        Nothing -> ""
        Just (bdy, _) -> case bdy of
          Servant.RequestBodyLBS lbs -> cs lbs
          Servant.RequestBodyBS bs -> bs
          Servant.RequestBodySource _ -> error "fromServantRequest: not implemented for RequestBodySource"

      -- Content-Type and Accept are specified by requestBody and requestAccept
      headers =
        filter (\(h, _) -> h /= "Accept" && h /= "Content-Type") $
          toList $
            Servant.requestHeaders r
      acceptHdr
        | null hs = Nothing
        | otherwise = Just ("Accept", renderHeader hs)
        where
          hs = toList $ ServantRequest.requestAccept r
      contentTypeHdr = case ServantRequest.requestBody r of
        Nothing -> Nothing
        Just (_', typ) -> Just (HTTP.hContentType, renderHeader typ)
      req =
        Wai.defaultRequest
          { Wai.requestMethod = Servant.requestMethod r,
            Wai.rawPathInfo = pathBS,
            Wai.rawQueryString = renderQuery True (toList (Servant.requestQueryString r)),
            Wai.requestHeaders =
              -- Inspired by 'Servant.Client.Internal.HttpClient.defaultMakeClientRequest',
              -- the Servant function that maps @Request@ to @Client.Request@.
              -- This solution is a bit sophisticated due to two constraints:
              --   - Accept header may contain a list of accepted media types.
              --   - Accept and Content-Type headers should only appear once in the result.
              maybeToList acceptHdr
                <> maybeToList contentTypeHdr
                <> headers
                <> [(originDomainHeaderName, T.encodeUtf8 (domainText domain))],
            Wai.isSecure = True,
            Wai.pathInfo = filter (not . T.null) (map cs (B8.split '/' pathBS)),
            Wai.queryString = toList (Servant.requestQueryString r)
          }
   in WaiTest.SRequest req (cs bodyBS)

toServantResponse :: WaiTest.SResponse -> Servant.Response
toServantResponse res =
  Servant.Response
    { Servant.responseStatusCode = WaiTest.simpleStatus res,
      Servant.responseHeaders = Seq.fromList (WaiTest.simpleHeaders res),
      Servant.responseBody = WaiTest.simpleBody res,
      Servant.responseHttpVersion = http11
    }

createWaiTestFedClient ::
  forall (name :: Symbol) comp api.
  ( HasUnsafeFedEndpoint comp api name,
    Servant.HasClient WaiTestFedClient api
  ) =>
  Servant.Client WaiTestFedClient api
createWaiTestFedClient =
  Servant.clientIn (Proxy @api) (Proxy @WaiTestFedClient)

runWaiTestFedClient ::
  Domain ->
  WaiTestFedClient a ->
  WaiTest.Session a
runWaiTestFedClient domain action =
  runReaderT (unWaiTestFedClient action) domain

spawn :: (HasCallStack) => CreateProcess -> Maybe ByteString -> IO ByteString
spawn cp minput = do
  (mout, ex) <- withCreateProcess
    cp
      { std_out = CreatePipe,
        std_in = CreatePipe
      }
    $ \minh mouth _ ph ->
      let writeInput = for_ minh $ \inh -> do
            forM_ minput $ BS.hPutStr inh
            hClose inh
          readOutput = (,) <$> traverse BS.hGetContents mouth <*> waitForProcess ph
       in snd <$> concurrently writeInput readOutput
  case (mout, ex) of
    (Just out, ExitSuccess) -> pure out
    _ -> assertFailure "Failed spawning process"

assertJust :: (HasCallStack, MonadIO m) => Maybe a -> m a
assertJust (Just a) = pure a
assertJust Nothing = liftIO $ error "Expected Just, got Nothing"

assertElem :: (HasCallStack, Eq a, Show a) => String -> a -> [a] -> Assertion
assertElem msg x xs =
  unless (x `elem` xs) $
    assertFailure (msg <> "\nExpected to find: \n" <> show x <> "\nin:\n" <> show xs)

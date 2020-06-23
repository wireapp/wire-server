{-# OPTIONS_GHC -fno-warn-orphans #-}

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

module Util where

import Bilge
import Bilge.Assert
import qualified Brig.AWS as AWS
import Brig.AWS.Types
import qualified Brig.Options as Opts
import qualified Brig.Run as Run
import Brig.Types.Activation
import Brig.Types.Client
import Brig.Types.Connection
import Brig.Types.Intra
import Brig.Types.User
import Brig.Types.User.Auth
import Control.Lens ((^?), (^?!))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Fail (MonadFail)
import Control.Retry
import Data.Aeson
import Data.Aeson.Lens (key, _Integral, _JSON, _String)
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Conversion
import Data.Domain (mkDomain)
import Data.Id
import Data.List1 (List1)
import qualified Data.List1 as List1
import Data.Misc (PlainTextPassword (..))
import qualified Data.Text as Text
import qualified Data.Text.Ascii as Ascii
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Galley.Types.Teams as Team
import Gundeck.Types.Notification
import Imports
import qualified Network.Wai.Test as WaiTest
import System.Random (randomIO, randomRIO)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Cannon
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import Util.AWS
import Wire.API.Conversation.Member (Member (..))

type Brig = Request -> Request

type Cannon = Request -> Request

type CargoHold = Request -> Request

type Galley = Request -> Request

type Nginz = Request -> Request

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
            [ "bouncedRecipients" .= (fmap (\e -> object ["emailAddress" .= e]) ems),
              "bounceType" .= typ
            ]
      ]
  toJSON (MailComplaint ems) =
    object
      [ "notificationType" .= ("Complaint" :: Text),
        "complaint"
          .= object
            [ "complainedRecipients" .= (fmap (\e -> object ["emailAddress" .= e]) ems)
            ]
      ]

test :: Manager -> TestName -> Http a -> TestTree
test m n h = testCase n (void $ runHttpT m h)

test' :: AWS.Env -> Manager -> TestName -> Http a -> TestTree
test' e m n h = testCase n $ void $ runHttpT m (liftIO (purgeJournalQueue e) >> h)

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

createUserWithEmail :: HasCallStack => Text -> Email -> Brig -> Http User
createUserWithEmail name email brig = do
  r <-
    postUserWithEmail True True name (Just email) False Nothing Nothing brig
      <!! const 201 === statusCode
  responseJsonError r

createUserUntrustedEmail :: HasCallStack => Text -> Brig -> Http User
createUserUntrustedEmail name brig = do
  email <- randomUntrustedEmail
  createUserWithEmail name email brig

createAnonUser :: HasCallStack => Text -> Brig -> Http User
createAnonUser = createAnonUserExpiry Nothing

createAnonUserExpiry :: HasCallStack => Maybe Integer -> Text -> Brig -> Http User
createAnonUserExpiry expires name brig = do
  let p = RequestBodyLBS . encode $ object ["name" .= name, "expires_in" .= expires]
  r <- post (brig . path "/register" . contentJson . body p) <!! const 201 === statusCode
  responseJsonError r

requestActivationCode :: HasCallStack => Brig -> Int -> Either Email Phone -> Http ()
requestActivationCode brig expectedStatus ep =
  post (brig . path "/activate/send" . contentJson . body (RequestBodyLBS . encode $ bdy ep))
    !!! const expectedStatus === statusCode
  where
    bdy (Left e) = object ["email" .= fromEmail e]
    bdy (Right p) = object ["phone" .= fromPhone p]

getActivationCode ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
  Brig ->
  Either Email Phone ->
  m (Maybe (ActivationKey, ActivationCode))
getActivationCode brig ep = do
  let qry = either (queryItem "email" . toByteString') (queryItem "phone" . toByteString') ep
  r <- get $ brig . path "/i/users/activation-code" . qry
  let lbs = fromMaybe "" $ responseBody r
  let akey = ActivationKey . Ascii.unsafeFromText <$> (lbs ^? key "key" . _String)
  let acode = ActivationCode . Ascii.unsafeFromText <$> (lbs ^? key "code" . _String)
  return $ (,) <$> akey <*> acode

getPhoneLoginCode :: Brig -> Phone -> Http (Maybe LoginCode)
getPhoneLoginCode brig p = do
  r <- get $ brig . path "/i/users/login-code" . queryItem "phone" (toByteString' p)
  let lbs = fromMaybe "" $ responseBody r
  return (LoginCode <$> (lbs ^? key "code" . _String))

assertUpdateNotification :: WS.WebSocket -> UserId -> UserUpdate -> IO Notification
assertUpdateNotification ws uid upd = WS.assertMatch (5 # Second) ws $ \n -> do
  let j = Object $ List1.head (ntfPayload n)
  j ^? key "type" . _String @?= Just "user.update"
  let u = j ^?! key "user"
  u ^? key "id" . _String @?= Just (UUID.toText (toUUID uid))
  u ^? key "name" . _String @?= fromName <$> uupName upd
  u ^? key "accent_id" . _Integral @?= fromColourId <$> uupAccentId upd
  u ^? key "assets" @?= Just (toJSON (uupAssets upd))

--------------------------------------------------------------------------------
-- API Operations

getConnection :: Brig -> UserId -> UserId -> Http ResponseLBS
getConnection brig from to =
  get $
    brig
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
  maybe (error $ "postUserInternal: Failed to decode user due to: " ++ show rs) return (responseJsonMaybe rs)

postUserRegister :: Object -> Brig -> Http User
postUserRegister payload brig = do
  rs <- postUserRegister' payload brig <!! const 201 === statusCode
  maybe (error $ "postUserRegister: Failed to decode user due to: " ++ show rs) return (responseJsonMaybe rs)

postUserRegister' :: (MonadIO m, MonadCatch m, MonadHttp m) => Object -> Brig -> m ResponseLBS
postUserRegister' payload brig = do
  post (brig . path "/register" . contentJson . body (RequestBodyLBS $ encode payload))

deleteUser :: UserId -> Maybe PlainTextPassword -> Brig -> Http ResponseLBS
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

activate :: Brig -> ActivationPair -> (MonadIO m, MonadHttp m) => m ResponseLBS
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
    brig
      . paths ["users", toByteString' usr]
      . zUser zusr

login :: Brig -> Login -> CookieType -> (MonadIO m, MonadHttp m) => m ResponseLBS
login b l t =
  let js = RequestBodyLBS (encode l)
   in post $
        b
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

data LoginCodeType = LoginCodeSMS | LoginCodeVoice
  deriving (Eq)

sendLoginCode :: Brig -> Phone -> LoginCodeType -> Bool -> Http ResponseLBS
sendLoginCode b p typ force =
  post $
    b
      . path "/login/send"
      . contentJson
      . body js
  where
    js =
      RequestBodyLBS . encode $
        object
          [ "phone" .= fromPhone p,
            "voice_call" .= (typ == LoginCodeVoice),
            "force" .= force
          ]

postConnection :: Brig -> UserId -> UserId -> Http ResponseLBS
postConnection brig from to =
  post $
    brig
      . path "/connections"
      . contentJson
      . body payload
      . zUser from
      . zConn "conn"
  where
    payload =
      RequestBodyLBS . encode $
        ConnectionRequest (makeIdOpaque to) "some conv name" (Message "some message")

putConnection :: Brig -> UserId -> UserId -> Relation -> Http ResponseLBS
putConnection brig from to r =
  put $
    brig
      . paths ["/connections", toByteString' to]
      . contentJson
      . body payload
      . zUser from
      . zConn "conn"
  where
    payload = RequestBodyLBS . encode $ object ["status" .= r]

connectUsers :: Brig -> UserId -> List1 UserId -> Http ()
connectUsers b u = mapM_ connectTo
  where
    connectTo v = do
      void $ postConnection b u v
      void $ putConnection b v u Accepted

putHandle ::
  (MonadIO m, MonadHttp m, HasCallStack) =>
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

addClient ::
  (Monad m, MonadCatch m, MonadIO m, MonadHttp m, MonadFail m, HasCallStack) =>
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
defNewClient ty pks lpk =
  (newClient ty lpk)
    { newClientPassword = Just defPassword,
      newClientPrekeys = pks,
      newClientLabel = Just "Test Device",
      newClientModel = Just "Test Model"
    }

getPreKey :: Brig -> UserId -> ClientId -> Http ResponseLBS
getPreKey brig u c =
  get $
    brig
      . paths ["users", toByteString' u, "prekeys", toByteString' c]

getTeamMember ::
  (MonadIO m, MonadCatch m, MonadFail m, MonadHttp m, HasCallStack) =>
  UserId ->
  TeamId ->
  Galley ->
  m Team.TeamMember
getTeamMember u tid galley =
  responseJsonError
    =<< get
      ( galley
          . paths ["i", "teams", toByteString' tid, "members", toByteString' u]
          . zUser u
          . expect2xx
      )

getConversation :: Galley -> UserId -> ConvId -> (MonadIO m, MonadHttp m) => m ResponseLBS
getConversation galley usr cnv =
  get $
    galley
      . paths ["conversations", toByteString' cnv]
      . zAuthAccess usr "conn"

isMember :: Galley -> UserId -> ConvId -> (MonadIO m, MonadHttp m) => m Bool
isMember g usr cnv = do
  res <-
    get $
      g
        . paths ["i", "conversations", toByteString' cnv, "members", toByteString' usr]
        . expect2xx
  case responseJsonMaybe res of
    Nothing -> return False
    Just m -> return (makeIdOpaque usr == memId m)

getStatus :: HasCallStack => Brig -> UserId -> (MonadIO m, MonadHttp m) => m AccountStatus
getStatus brig u =
  (^?! key "status" . (_JSON @Value @AccountStatus)) . (responseJsonUnsafe @Value)
    <$> get
      ( brig . paths ["i", "users", toByteString' u, "status"]
          . expect2xx
      )

chkStatus :: HasCallStack => Brig -> UserId -> AccountStatus -> (MonadIO m, MonadHttp m, MonadCatch m) => m ()
chkStatus brig u s =
  get (brig . paths ["i", "users", toByteString' u, "status"]) !!! do
    const 200 === statusCode
    const (Just (toJSON s)) === ((^? key "status") <=< responseBody)

setStatus :: Brig -> UserId -> AccountStatus -> Http ()
setStatus brig u s =
  let js = RequestBodyLBS . encode $ AccountStatusUpdate s
   in put
        ( brig . paths ["i", "users", toByteString' u, "status"]
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

maybeFromJSON :: FromJSON a => Value -> Maybe a
maybeFromJSON v = case fromJSON v of
  Success a -> Just a
  _ -> Nothing

zAuthAccess :: UserId -> ByteString -> Request -> Request
zAuthAccess u c = header "Z-Type" "access" . zUser u . zConn c

zUser :: UserId -> Request -> Request
zUser = header "Z-User" . C8.pack . show

zConn :: ByteString -> Request -> Request
zConn = header "Z-Connection"

mkEmailRandomLocalSuffix :: MonadIO m => Text -> m Email
mkEmailRandomLocalSuffix e = do
  uid <- liftIO UUID.nextRandom
  case parseEmail e of
    Just (Email loc dom) -> return $ Email (loc <> "+" <> UUID.toText uid) dom
    Nothing -> fail $ "Invalid email address: " ++ Text.unpack e

-- | Generate emails that are in the trusted whitelist of domains whose @+@ suffices count for email
-- disambiguation.  See also: 'Brig.Email.mkEmailKey'.
randomEmail :: MonadIO m => m Email
randomEmail = mkSimulatorEmail "success"

-- | To test the behavior of email addresses with untrusted domains (two emails are equal even if
-- their local part after @+@ differs), we need to generate them.
randomUntrustedEmail :: MonadIO m => m Email
randomUntrustedEmail = do
  -- NOTE: local part cannot be longer than 64 octets
  rd <- liftIO (randomIO :: IO Integer)
  pure $ Email (Text.pack $ show rd) "zinfra.io"

mkSimulatorEmail :: MonadIO m => Text -> m Email
mkSimulatorEmail loc = mkEmailRandomLocalSuffix (loc <> "@simulator.amazonses.com")

randomPhone :: MonadIO m => m Phone
randomPhone = liftIO $ do
  nrs <- map show <$> replicateM 14 (randomRIO (0, 9) :: IO Int)
  let phone = parsePhone . Text.pack $ "+0" ++ concat nrs
  return $ fromMaybe (error "Invalid random phone#") phone

updatePhone :: Brig -> UserId -> Phone -> Http ()
updatePhone brig uid phn = do
  -- update phone
  let phoneUpdate = RequestBodyLBS . encode $ PhoneUpdate phn
  put (brig . path "/self/phone" . contentJson . zUser uid . zConn "c" . body phoneUpdate)
    !!! (const 202 === statusCode)
  -- activate
  act <- getActivationCode brig (Right phn)
  case act of
    Nothing -> liftIO $ assertFailure "missing activation key/code"
    Just kc ->
      activate brig kc !!! do
        const 200 === statusCode
        const (Just False) === fmap activatedFirst . responseJsonMaybe

defEmailLogin :: Email -> Login
defEmailLogin e = emailLogin e defPassword (Just defCookieLabel)

emailLogin :: Email -> PlainTextPassword -> Maybe CookieLabel -> Login
emailLogin e = PasswordLogin (LoginByEmail e)

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
    Prekey (PrekeyId 26) "pQABARgaAqEAWCBMSQoQ6B35plC80i1O3AWlJSftCEbCbju97Iykg5+NWQOhAKEAWCCy39UyMEgetquvTo7P19bcyfnWBzQMOEG1v+0wub0magT2"
  ]

-- | The client ID of the first of 'someLastPrekeys'
someClientId :: ClientId
someClientId = ClientId "1dbfbe22c8a35cb2"

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

defPassword :: PlainTextPassword
defPassword = PlainTextPassword "secret"

defWrongPassword :: PlainTextPassword
defWrongPassword = PlainTextPassword "not secret"

defCookieLabel :: CookieLabel
defCookieLabel = CookieLabel "auth"

randomBytes :: Int -> IO ByteString
randomBytes n = BS.pack <$> replicateM n randomIO

randomHandle :: MonadIO m => m Text
randomHandle = liftIO $ do
  nrs <- replicateM 21 (randomRIO (97, 122)) -- a-z
  return (Text.pack (map chr nrs))

randomName :: MonadIO m => m Name
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
randomNameWithMaxLen :: MonadIO m => Word -> m Name
randomNameWithMaxLen maxLen = liftIO $ do
  len <- randomRIO (2, maxLen)
  chars <- fill len []
  return $ Name (Text.pack chars)
  where
    fill 0 cs = return cs
    fill 1 cs = (: cs) <$> randLetter
    fill n cs = do
      c <- randChar
      if isLetter c || isNumber c || isPunctuation c || isSymbol c
        then fill (n - 1) (c : cs)
        else fill n cs
    randChar = chr <$> randomRIO (0x0000, 0xFFFF)
    randLetter = do
      c <- randChar
      if isLetter c
        then return c
        else randLetter

retryWhileN :: (MonadIO m) => Int -> (a -> Bool) -> m a -> m a
retryWhileN n f m =
  retrying
    (constantDelay 1000000 <> limitRetries n)
    (const (return . f))
    (const m)

-- | This allows you to run requests against a brig instantiated using the given options.
--   Note that ONLY 'brig' calls should occur within the provided action, calls to other
--   services will fail.
withSettingsOverrides :: MonadIO m => Opts.Opts -> WaiTest.Session a -> m a
withSettingsOverrides opts action = liftIO $ do
  (brigApp, _) <- Run.mkApp opts
  WaiTest.runSession action brigApp

-- | When we remove the customer-specific extension of domain blocking, this test will fail to
-- compile.
withDomainsBlockedForRegistration :: (MonadIO m) => Opts.Opts -> [Text] -> WaiTest.Session a -> m a
withDomainsBlockedForRegistration opts domains sess = do
  let opts' = opts {Opts.optSettings = (Opts.optSettings opts) {Opts.setCustomerExtensions = Just blocked}}
      blocked = Opts.CustomerExtensions (Opts.DomainsBlockedForRegistration (unsafeMkDomain <$> domains))
      unsafeMkDomain = either error id . mkDomain
  withSettingsOverrides opts' sess

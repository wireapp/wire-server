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

module Test.Federator.InwardSpec where

import Bilge
import Bilge.Assert
import Control.Lens (view)
import Control.Monad.Catch
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Char8 as C8
import Data.Id
import Data.Misc
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Federator.Options
import Imports
import Mu.GRpc.Client.TyApps
import qualified Network.HTTP.Types as HTTP
import System.Random
import Test.Federator.Util
import Test.Hspec
import Test.Tasty.HUnit (assertFailure)
import Util.Options (Endpoint (Endpoint))
import Wire.API.Federation.GRPC.Client
import Wire.API.Federation.GRPC.Types (Component (..), HTTPMethod (..), HTTPResponse (..), Inward, InwardResponse (..), QueryParam (..), Request (Request))
import Wire.API.User
import Wire.API.User.Auth
import Wire.API.User.Handle (UserHandleInfo (UserHandleInfo))

-- FUTUREWORK(federation): move these tests to brig-integration (benefit: avoid duplicating all of the brig helper code)

-- | Path covered by this test
--
--  +----------+
--  |federator-|          +------+--+
--  |integration  grpc    |federator|
--  |          |--------->+         +
--  |          |          +----+----+
--  +----------+               |
--                             | http
--                             v
--                         +---+--+
--                         | brig |
--                         |      |
--                         +------+
--
--  (ascii diagrams from asciiflow.com)
spec :: TestEnv -> Spec
spec env =
  describe "Inward" $ do
    it "should be able to call brig" $
      runTestFederator env $ do
        brig <- view teBrig <$> ask
        user <- randomUser brig
        hdl <- randomHandle
        _ <- putHandle brig (userId user) hdl

        Endpoint fedHost fedPort <- federatorExternal . view teOpts <$> ask
        client <- createGrpcClient (grpcClientConfigSimple (Text.unpack fedHost) (fromIntegral fedPort) False)
        c <- case client of
          Left (err) -> liftIO $ assertFailure (show err)
          Right cli -> pure cli
        let brigCall = Request Brig (HTTPMethod HTTP.GET) "users/by-handle" [QueryParam "handle" (Text.encodeUtf8 hdl)] mempty
        res <- liftIO $ gRpcCall @'MsgProtoBuf @Inward @"Inward" @"call" c brigCall

        liftIO $ case res of
          GRpcOk (InwardResponseHTTPResponse (HTTPResponse sts bdy)) -> do
            sts `shouldBe` 200
            eitherDecodeStrict bdy `shouldBe` Right (UserHandleInfo (userQualifiedId user))
          GRpcOk (InwardResponseErr err) -> assertFailure $ "Unexpected error response: " <> show err
          x -> assertFailure $ "GRpc call failed unexpectedly: " <> show x

-- All the code below is copied from brig-integrration tests
-- FUTUREWORK: This should live in another package and shared by all the integration tests

randomUser ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
  BrigReq ->
  m User
randomUser = randomUser' True

randomUser' ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
  Bool ->
  BrigReq ->
  m User
randomUser' hasPwd brig = do
  n <- fromName <$> randomName
  createUser' hasPwd n brig

createUser ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
  Text ->
  BrigReq ->
  m User
createUser = createUser' True

createUser' ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
  Bool ->
  Text ->
  BrigReq ->
  m User
createUser' hasPwd name brig = do
  r <-
    postUser' hasPwd True name True False Nothing Nothing brig
      <!! const 201 === statusCode
  responseJsonError r

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
  BrigReq ->
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
  BrigReq ->
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

putHandle ::
  (MonadIO m, MonadHttp m, HasCallStack) =>
  BrigReq ->
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

randomPhone :: MonadIO m => m Phone
randomPhone = liftIO $ do
  nrs <- map show <$> replicateM 14 (randomRIO (0, 9) :: IO Int)
  let phone = parsePhone . Text.pack $ "+0" ++ concat nrs
  return $ fromMaybe (error "Invalid random phone#") phone

defPassword :: PlainTextPassword
defPassword = PlainTextPassword "secret"

defCookieLabel :: CookieLabel
defCookieLabel = CookieLabel "auth"

-- | Generate emails that are in the trusted whitelist of domains whose @+@ suffices count for email
-- disambiguation.  See also: 'Brig.Email.mkEmailKey'.
randomEmail :: MonadIO m => m Email
randomEmail = mkSimulatorEmail "success"

mkSimulatorEmail :: MonadIO m => Text -> m Email
mkSimulatorEmail loc = mkEmailRandomLocalSuffix (loc <> "@simulator.amazonses.com")

mkEmailRandomLocalSuffix :: MonadIO m => Text -> m Email
mkEmailRandomLocalSuffix e = do
  uid <- liftIO UUID.nextRandom
  case parseEmail e of
    Just (Email loc dom) -> return $ Email (loc <> "+" <> UUID.toText uid) dom
    Nothing -> error $ "Invalid email address: " ++ Text.unpack e

zUser :: UserId -> Bilge.Request -> Bilge.Request
zUser = header "Z-User" . C8.pack . show

zConn :: ByteString -> Bilge.Request -> Bilge.Request
zConn = header "Z-Connection"

randomHandle :: MonadIO m => m Text
randomHandle = liftIO $ do
  nrs <- replicateM 21 (randomRIO (97, 122)) -- a-z
  return (Text.pack (map chr nrs))

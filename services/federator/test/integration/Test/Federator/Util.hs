{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

-- FUTUREWORK: this is all copied from galley and spar. Maybe at some point reduce duplication?
module Test.Federator.Util where

import Bilge
import Bilge.Assert
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.Except
import Crypto.Random.Types (MonadRandom, getRandomBytes)
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as C8
import Data.Data (typeRep)
import Data.Id
import Data.Misc
import Data.Proxy
import Data.String.Conversions
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Yaml as Yaml
import Federator.Env (TLSSettings (..))
import Federator.Options
import Federator.Run (mkTLSSettings)
import Imports
import Mu.GRpc.Client.TyApps
import qualified Options.Applicative as OPA
import System.Random
import Test.Federator.JSON
import Test.Tasty.HUnit
import Util.Options
import Wire.API.Federation.GRPC.Types (InwardError, InwardResponse (..))
import Wire.API.User
import Wire.API.User.Auth

type BrigReq = Request -> Request

newtype TestFederator m a = TestFederator {unwrapTestFederator :: ReaderT TestEnv m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadTrans,
      MonadReader TestEnv,
      MonadFail,
      MonadThrow,
      MonadCatch
    )

instance MonadRandom m => MonadRandom (TestFederator m) where
  getRandomBytes = lift . getRandomBytes

instance MonadIO m => MonadHttp (TestFederator m) where
  handleRequestWithCont req handler = do
    manager <- _teMgr <$> ask
    liftIO $ withResponse req manager handler

runTestFederator :: TestEnv -> TestFederator IO a -> IO a
runTestFederator env = flip runReaderT env . unwrapTestFederator

-- | See 'mkEnv' about what's in here.
data TestEnv = TestEnv
  { _teMgr :: Manager,
    _teTLSSettings :: TLSSettings,
    _teBrig :: BrigReq,
    -- | federator config
    _teOpts :: Opts,
    -- | integration test config
    _teTstOpts :: IntegrationConfig
  }

type Select = TestEnv -> (Request -> Request)

data IntegrationConfig = IntegrationConfig
  { cfgBrig :: Endpoint,
    cfgFederatorExternal :: Endpoint,
    cfgNginxIngress :: Endpoint,
    cfgOriginDomain :: Text
  }
  deriving (Show, Generic)

deriveFromJSON deriveJSONOptions ''IntegrationConfig

makeLenses ''TestEnv

-- | Call 'mkEnv' with options from config files.
mkEnvFromOptions :: IO TestEnv
mkEnvFromOptions = do
  let desc = "Federator - SSO Service Integration Test Suite"
  (integrationCfgFilePath, cfgFilePath) <- OPA.execParser (OPA.info (OPA.helper <*> cliOptsParser) (OPA.header desc <> OPA.fullDesc))
  integrationOpts :: IntegrationConfig <- Yaml.decodeFileEither integrationCfgFilePath >>= either (error . show) pure
  serviceOpts :: Opts <- Yaml.decodeFileEither cfgFilePath >>= either (throwIO . ErrorCall . show) pure
  mkEnv integrationOpts serviceOpts

-- | Accept config file locations as cli options.
cliOptsParser :: OPA.Parser (String, String)
cliOptsParser =
  (,)
    <$> OPA.strOption
      ( OPA.long "integration-config"
          <> OPA.short 'i'
          <> OPA.help "Integration config to load"
          <> OPA.showDefault
          <> OPA.value defaultIntPath
      )
    <*> OPA.strOption
      ( OPA.long "service-config"
          <> OPA.short 's'
          <> OPA.help "Federator application config to load"
          <> OPA.showDefault
          <> OPA.value defaultFederatorPath
      )
  where
    defaultIntPath = "/etc/wire/integration/integration.yaml"
    defaultFederatorPath = "/etc/wire/federator/conf/federator.yaml"

-- | Create an environment for integration tests from integration and federator config files.
mkEnv :: HasCallStack => IntegrationConfig -> Opts -> IO TestEnv
mkEnv _teTstOpts _teOpts = do
  _teMgr :: Manager <- newManager defaultManagerSettings
  let _teBrig = endpointToReq (cfgBrig _teTstOpts)
  _teTLSSettings <- mkTLSSettings (optSettings _teOpts)
  pure TestEnv {..}

destroyEnv :: HasCallStack => TestEnv -> IO ()
destroyEnv _ = pure ()

endpointToReq :: Endpoint -> (Bilge.Request -> Bilge.Request)
endpointToReq ep = Bilge.host (ep ^. epHost . to cs) . Bilge.port (ep ^. epPort)

-- grpc utilities

asInwardBody :: forall a. (HasCallStack, Typeable a, FromJSON a) => GRpcReply InwardResponse -> TestFederator IO a
asInwardBody = either (liftIO . assertFailure) pure . asInwardBodyEither

asInwardBodyUnsafe :: (HasCallStack, Typeable a, FromJSON a) => GRpcReply InwardResponse -> a
asInwardBodyUnsafe = either err id . asInwardBodyEither
  where
    err parserErr = error . unwords $ ["asInwardBodyUnsafe:"] <> [parserErr]

asInwardBodyEither :: forall a. (HasCallStack, Typeable a, FromJSON a) => GRpcReply InwardResponse -> Either String a
asInwardBodyEither (GRpcOk (InwardResponseError err)) = Left (show err)
asInwardBodyEither (GRpcOk (InwardResponseBody bdy)) = first addTypeInfo $ eitherDecodeStrict bdy
  where
    addTypeInfo :: String -> String
    addTypeInfo = (("Could not parse InwardResponseBody as '" <> show (typeRep (Proxy @a)) <> "': ") <>)
asInwardBodyEither other = Left $ "GRpc call failed unexpectedly: " <> show other

asInwardError :: HasCallStack => GRpcReply InwardResponse -> TestFederator IO InwardError
asInwardError = either err pure . asInwardErrorEither
  where
    err parserErr = liftIO $ assertFailure (unwords $ ["asInwardError:"] <> [parserErr])

asInwardErrorUnsafe :: HasCallStack => GRpcReply InwardResponse -> InwardError
asInwardErrorUnsafe = either err id . asInwardErrorEither
  where
    err parserErr = error . unwords $ ["asInwardErrorUnsafe:"] <> [parserErr]

asInwardErrorEither :: HasCallStack => GRpcReply InwardResponse -> Either String InwardError
asInwardErrorEither (GRpcOk (InwardResponseError err)) = Right err
asInwardErrorEither (GRpcOk (InwardResponseBody bdy)) = Left ("expected InwardError, but got InwardResponseBody: " <> show bdy)
asInwardErrorEither other = Left $ "GRpc call failed unexpectedly: " <> show other

-- All the code below is copied from brig-integration tests
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
    fill 0 characters = return characters
    fill 1 characters = (: characters) <$> randLetter
    fill n characters = do
      c <- randChar
      if isLetter c || isNumber c || isPunctuation c || isSymbol c
        then fill (n - 1) (c : characters)
        else fill n characters
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

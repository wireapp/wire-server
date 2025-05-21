module Testlib.RunServices (main) where

import Control.Concurrent
import Control.Monad.Codensity
import Options.Applicative
import System.Directory
import System.Exit
import System.FilePath
import System.Posix (getWorkingDirectory)
import System.Process
import Testlib.Ports
import Testlib.Prelude

parentDir :: FilePath -> Maybe FilePath
parentDir path =
  let dirs = splitPath path
   in if null dirs
        then Nothing
        else Just $ joinPath (init dirs)

containsGit :: FilePath -> IO Bool
containsGit path =
  doesPathExist $ joinPath [path, ".git"]

findProjectRoot :: FilePath -> IO (Maybe FilePath)
findProjectRoot path = do
  c <- containsGit path
  if c
    then pure (Just path)
    else case parentDir path of
      Nothing -> pure Nothing
      Just p -> findProjectRoot p

data Opts = Opts
  { withManualTestingOverrides :: Bool,
    runSubprocess :: [String]
  }
  deriving (Show)

optsParser :: Parser Opts
optsParser =
  Opts
    <$> switch
      ( long "with-manual-testing-overrides"
          <> short 'm'
          <> help "Run services with settings tuned for manual app usage (not recommended for running integration tests)"
      )
    <*> many
      ( strArgument
          ( metavar "COMMAND_WITH_ARGS"
              <> help "When specified, the command will be run after services have started and service will be killed after the command exits"
          )
      )

main :: IO ()
main = do
  cwd <- getWorkingDirectory
  mbProjectRoot <- findProjectRoot cwd
  opts <- execParser (info (optsParser <**> helper) fullDesc)
  cfg <- case mbProjectRoot of
    Nothing -> error "Could not find project root. Please make sure you call run-services from somewhere in wire-server."
    Just projectRoot ->
      pure $ joinPath [projectRoot, "services/integration.yaml"]

  let run = case opts.runSubprocess of
        [] -> do
          putStrLn "services started"
          forever (threadDelay maxBound)
        _ -> do
          let cp = proc "sh" (["-c", "exec \"$@\"", "--"] <> opts.runSubprocess)
          (_, _, _, ph) <- createProcess cp
          exitWith =<< waitForProcess ph

  runCodensity (mkGlobalEnv cfg >>= mkEnv Nothing) $ \env ->
    runAppWithEnv env
      $ lowerCodensity
      $ do
        void
          $ traverseConcurrentlyCodensity
            ( \r ->
                void
                  $ if opts.withManualTestingOverrides
                    then startDynamicBackend r manualTestingOverrides
                    else startDynamicBackend r mempty
            )
            [backendA, backendB]
        liftIO run

backendA :: BackendResource
backendA =
  BackendResource
    { berName = BackendA,
      berBrigKeyspace = "brig_test",
      berGalleyKeyspace = "galley_test",
      berSparKeyspace = "spar_test",
      berGundeckKeyspace = "gundeck_test",
      berElasticsearchIndex = "directory_test",
      berFederatorInternal = servicePort (ServiceInternal FederatorInternal) BackendA,
      berFederatorExternal = servicePort FederatorExternal BackendA,
      berDomain = "example.com",
      berAwsUserJournalQueue = "integration-user-events.fifo",
      berAwsPrekeyTable = "integration-brig-prekeys",
      berAwsS3Bucket = "dummy-bucket",
      berAwsQueueName = "integration-gundeck-events",
      berBrigInternalEvents = "integration-brig-events-internal",
      berEmailSMSSesQueue = "integration-brig-events",
      berEmailSMSEmailSender = "backend-integration@wire.com",
      berGalleyJournal = "integration-team-events.fifo",
      berVHost = "backendA",
      berNginzSslPort = servicePort NginzSSL BackendA,
      berInternalServicePorts = internalServicePorts BackendA,
      berEnableService = const True,
      berNginzHttp2Port = servicePort NginzHttp2 BackendA,
      berMlsPrivateKeyPaths =
        object
          [ fromString "removal"
              .= object
                [ fromString "ed25519" .= "test/resources/backendA/ed25519.pem",
                  fromString "ecdsa_secp256r1_sha256" .= "test/resources/backendA/ecdsa_secp256r1_sha256.pem",
                  fromString "ecdsa_secp384r1_sha384" .= "test/resources/backendA/ecdsa_secp384r1_sha384.pem",
                  fromString "ecdsa_secp521r1_sha512" .= "test/resources/backendA/ecdsa_secp521r1_sha512.pem"
                ]
          ]
    }

backendB :: BackendResource
backendB =
  BackendResource
    { berName = BackendB,
      berBrigKeyspace = "brig_test2",
      berGalleyKeyspace = "galley_test2",
      berSparKeyspace = "spar_test2",
      berGundeckKeyspace = "gundeck_test2",
      berElasticsearchIndex = "directory2_test",
      berFederatorInternal = servicePort (ServiceInternal FederatorInternal) BackendB,
      berFederatorExternal = servicePort FederatorExternal BackendB,
      berDomain = "b.example.com",
      berAwsUserJournalQueue = "integration-user-events2.fifo",
      berAwsPrekeyTable = "integration-brig-prekeys2",
      berAwsS3Bucket = "dummy-bucket2",
      berAwsQueueName = "integration-gundeck-events2",
      berBrigInternalEvents = "integration-brig-events-internal2",
      berEmailSMSSesQueue = "integration-brig-events2",
      berEmailSMSEmailSender = "backend-integration2@wire.com",
      berGalleyJournal = "integration-team-events2.fifo",
      -- FUTUREWORK: set up vhosts in dev/ci for example.com and b.example.com
      -- in case we want backendA and backendB to federate with a third backend
      -- (because otherwise both queues will overlap)
      berVHost = "backendB",
      berNginzSslPort = servicePort NginzSSL BackendB,
      berInternalServicePorts = internalServicePorts BackendB,
      berEnableService = \case
        WireServerEnterprise -> False
        _ -> True,
      berNginzHttp2Port = servicePort NginzHttp2 BackendB,
      berMlsPrivateKeyPaths =
        object
          [ fromString "removal"
              .= object
                [ fromString "ed25519" .= "test/resources/backendB/ed25519.pem",
                  fromString "ecdsa_secp256r1_sha256" .= "test/resources/backendB/ecdsa_secp256r1_sha256.pem",
                  fromString "ecdsa_secp384r1_sha384" .= "test/resources/backendB/ecdsa_secp384r1_sha384.pem",
                  fromString "ecdsa_secp521r1_sha512" .= "test/resources/backendB/ecdsa_secp521r1_sha512.pem"
                ]
          ]
    }

manualTestingOverrides :: ServiceOverrides
manualTestingOverrides =
  let smtpEndpoint = object ["host" .= "localhost", "port" .= (2500 :: Int)]
      authSettings =
        object
          [ "userTokenTimeout" .= (4838400 :: Int),
            "sessionTokenTimeout" .= (86400 :: Int),
            "accessTokenTimeout" .= (900 :: Int),
            "providerTokenTimeout" .= (900 :: Int),
            "legalHoldUserTokenTimeout" .= (4838400 :: Int),
            "legalHoldAccessTokenTimeout" .= (900 :: Int)
          ]
   in def
        { brigCfg =
            mergeField "emailSMS.email.smtpEndpoint" smtpEndpoint
              >=> setField "emailSMS.email.smtpConnType" "plain"
              >=> removeField "emailSMS.email.sesQueue"
              >=> removeField "emailSMS.email.sesEndpoint"
              >=> mergeField "zauth.authSettings" authSettings
              >=> setField @_ @Int "optSettings.setActivationTimeout" 3600
              >=> setField @_ @Int "optSettings.setVerificationTimeout" 3600
              >=> setField @_ @Int "optSettings.setTeamInvitationTimeout" 3600
              >=> setField @_ @Int "optSettings.setUserCookieRenewAge" 1209600
              >=> removeField "optSettings.setSuspendInactiveUsers"
        }

module Testlib.RunServices (main) where

import Control.Concurrent
import Control.Monad.Codensity
import Options.Applicative
import System.Directory
import System.Exit
import System.FilePath
import System.Posix (getWorkingDirectory)
import System.Process
import Testlib.Prelude
import Testlib.ResourcePool

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

data Mode
  = IntegrationTesting
  | ManualTesting
  | PerformanceTesting
  deriving (Show)

parseMode :: String -> Either String Mode
parseMode = \case
  "integration" -> Right IntegrationTesting
  "int" -> Right IntegrationTesting
  "manual" -> Right ManualTesting
  "man" -> Right ManualTesting
  "performance" -> Right PerformanceTesting
  "perf" -> Right PerformanceTesting
  x -> Left $ "Invalid mode: " <> x

showMode :: Mode -> String
showMode = \case
  IntegrationTesting -> "integration"
  PerformanceTesting -> "performance"
  ManualTesting -> "manual"

data Opts = Opts
  { mode :: Mode,
    runSubprocess :: [String]
  }
  deriving (Show)

optsParser :: Parser Opts
optsParser =
  Opts
    <$> option
      (eitherReader parseMode)
      ( long "mode"
          <> short 'm'
          <> value IntegrationTesting
          <> showDefaultWith showMode
          <> help "Run services with settings tuned for particular type of testing"
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

  runCodensity (mkGlobalEnv cfg >>= mkEnv) $ \env ->
    runAppWithEnv env
      $ lowerCodensity
      $ do
        _modifyEnv <-
          traverseConcurrentlyCodensity
            ( \r -> do
                let overrides = case opts.mode of
                      IntegrationTesting -> mempty
                      PerformanceTesting -> performanceTestingOverrides
                      ManualTesting -> manualTestingOverrides
                void $ startDynamicBackend r overrides
            )
            [backendA, backendB]
        liftIO run

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

performanceTestingOverrides :: ServiceOverrides
performanceTestingOverrides =
  let authSettings =
        object
          [ "userTokenTimeout" .= (4838400 :: Int),
            "sessionTokenTimeout" .= (86400 :: Int),
            "accessTokenTimeout" .= (900 :: Int),
            "providerTokenTimeout" .= (900 :: Int),
            "legalHoldUserTokenTimeout" .= (4838400 :: Int),
            "legalHoldAccessTokenTimeout" .= (900 :: Int)
          ]
      maxTeamSize = 2000
      maxConvSize = 2000
      maxFanoutSize = 2000
   in def
        { brigCfg =
            -- Ensure that users don't get logged out quickly, as the tests
            -- might need them for longer and logging them out automatically
            -- just makes the tests do more annoying things.
            mergeField "zauth.authSettings" authSettings
              >=> setField @_ @Int "optSettings.setUserCookieRenewAge" 1209600
              >=> removeField "optSettings.setSuspendInactiveUsers"
              -- Ensure that big teams and conversations can be created.
              >=> setField @_ @Int "optSettings.setMaxTeamSize" maxTeamSize
              >=> setField @_ @Int "optSettings.setMaxConvSize" maxConvSize,
          galleyCfg =
            -- Ensure that big teams and conversations can be created.
            setField @_ @Int "settings.maxTeamSize" maxTeamSize
              >=> setField @_ @Int "settings.maxFanoutSize" maxFanoutSize
              >=> setField @_ @Int "settings.maxConvSize" maxConvSize
        }

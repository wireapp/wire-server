module Test.Defederation where

import API.BrigInternal
import API.Common
-- import API.Galley
import API.Gundeck
import API.GundeckInternal
import Control.Concurrent (threadDelay)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.Foldable.Extra (anyM)
import Data.Text qualified as T
import SetupHelpers
import System.Timeout (timeout)
import Testlib.Prelude

examplePush :: MakesValue u => u -> App Value
examplePush u = do
  r <- recipient u
  pure $
    object
      [ "recipients" .= [r],
        "payload" .= [object ["hello" .= "world"]]
      ]

testDefederationRemoteNotifications :: HasCallStack => App ()
testDefederationRemoteNotifications = do
  -- The timeout for waiting on notifications
  let tSec = 60
  -- Setup a remote user we can get notifications for.
  user <- randomUser OtherDomain def
  push <- examplePush user
  bindResponse (postPush user [push]) $ \res ->
    res.status `shouldMatchInt` 200
  let client = "deadbeeef"

  -- Defederate from a domain that doesn't exist. This won't do anything to the databases
  -- But it will send out notifications that we can wait on.
  -- Begin the whole process at Brig, the same as an operator would.
  resp <- deleteFedConn OwnDomain "example.example.com"
  resp.status `shouldMatchInt` 200

  -- Get notifications for the client
  let loop predicate = do
        n <- getNotifications user client def >>= getJSON 200
        b <- predicate n
        if b
          then pure n
          else do
            liftIO $ threadDelay 1000000
            loop predicate
  env <- ask
  -- Loop until we have a notification that we expect
  -- TODO: this needs a timeout, otherwise it can go on forever!
  m <- liftIO $
    timeout (tSec * 1000 * 1000) $
      flip runReaderT env $
        unApp $
          loop $ \ns -> do
            allNotifs <- ns %. "notifications" & asList
            actual <- traverse (%. "payload") allNotifs
            let findConenctionRemoved v = do
                  v' <- lookupField v "0"
                  v'' <- lookupField v' "type"
                  pure $ v'' == pure (String $ T.pack "federation.connectionRemoved")
            anyM findConenctionRemoved actual
  case m of
    Nothing -> assertFailure "Didn't get the expected notification before the timeout"
    Just _ -> pure ()

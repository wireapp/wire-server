module Test.Migration.Util where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Stack
import SetupHelpers hiding (deleteUser)
import Testlib.Prelude
import Text.Regex.TDFA ((=~))

waitForMigration :: (HasCallStack) => String -> String -> App ()
waitForMigration domain name = do
  metrics <-
    getMetrics domain BackgroundWorker `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      pure $ Text.decodeUtf8 resp.body
  let (_, _, _, finishedMatches) :: (Text, Text, Text, [Text]) = (metrics =~ Text.pack (name <> "\\ ([0-9]+\\.[0-9]+)$"))
  when (finishedMatches /= [Text.pack "1.0"]) $ do
    liftIO $ threadDelay 100_000
    waitForMigration domain name

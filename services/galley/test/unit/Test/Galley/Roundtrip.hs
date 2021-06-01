{-# LANGUAGE DeriveAnyClass #-}

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

module Test.Galley.Roundtrip
  ( tests,
  )
where

-- import API.SQS
-- import API.Util
-- import Bilge hiding (accept, head, timeout, trace)
-- import Bilge.Assert
-- import Brig.Types.Client
-- import Brig.Types.Intra (ConnectionStatus (ConnectionStatus), UserSet (..))
-- import Brig.Types.Provider
-- import Brig.Types.Team.LegalHold hiding (userId)
import Brig.Types.Test.Arbitrary ()
-- import qualified Brig.Types.User.Event as Ev
-- import qualified Cassandra.Exec as Cql
-- import qualified Control.Concurrent.Async as Async
-- import Control.Concurrent.Chan
-- import Control.Concurrent.Timeout hiding (threadDelay)
-- import Control.Exception (asyncExceptionFromException)
-- import Control.Lens
-- import Control.Monad.Catch
-- import Control.Retry (RetryPolicy, RetryStatus, exponentialBackoff, limitRetries, retrying)
-- import qualified Data.Aeson as Aeson
-- import Data.Aeson.Types (FromJSON, withObject, (.:))
-- import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Char8 as BS
-- import Data.ByteString.Conversion
-- import Data.Id
-- import Data.Json.Util (toUTCTimeMillis)
-- import Data.LegalHold
-- import qualified Data.List.NonEmpty as NonEmpty
-- import qualified Data.List1 as List1
-- import qualified Data.Map.Strict as Map
-- import Data.Misc (PlainTextPassword)
-- import Data.PEM
import Data.Proxy (Proxy (Proxy))
-- import Data.Range
-- import qualified Data.Set as Set
-- import Data.String.Conversions (LBS, cs)
-- import Data.Text.Encoding (encodeUtf8)
-- import qualified Data.Time.Clock as Time
-- import qualified Galley.App as Galley
-- import qualified Galley.Data as Data
-- import qualified Galley.Data.LegalHold as LegalHoldData
-- import Galley.External.LegalHoldService (validateServiceKey)
-- import Galley.Options (optSettings, setFeatureFlags)
-- import qualified Galley.Types.Clients as Clients
-- import Galley.Types.Teams
-- import Gundeck.Types.Notification (ntfPayload)
import Imports
-- import Network.HTTP.Types.Status (status200, status400, status404)
-- import Network.Wai
-- import Network.Wai as Wai
-- import qualified Network.Wai.Handler.Warp as Warp
-- import qualified Network.Wai.Handler.Warp.Internal as Warp
-- import qualified Network.Wai.Handler.WarpTLS as Warp
-- import qualified Network.Wai.Test as WaiTest
-- import qualified Network.Wai.Utilities.Error as Error
-- import qualified Network.Wai.Utilities.Response as Wai
import Servant.Swagger (validateEveryToJSON)
-- import System.Environment (withArgs)
-- import System.IO (hPutStrLn)
-- import Test.Hspec (hspec)
import Test.QuickCheck.Instances ()
import Test.Tasty
-- import qualified Test.Tasty.Cannon as WS
-- import Test.Tasty.HUnit
-- import TestHelpers
-- import TestSetup
-- import Wire.API.Connection (UserConnection)
-- import qualified Wire.API.Connection as Conn
-- import qualified Wire.API.Message as Msg

-- import qualified Wire.API.Team.Feature as Public
-- import Wire.API.User (UserProfile (..))
-- import Wire.API.User.Client (UserClients (..), UserClientsFull (userClientsFull))
-- import qualified Wire.API.User.Client as Client
import Test.Tasty.Hspec (testSpec)
import qualified Wire.API.Routes.Public.LegalHold as LegalHoldAPI

tests :: IO TestTree
tests = testSpec "Roundtrip" $ validateEveryToJSON (Proxy @LegalHoldAPI.ServantAPI)

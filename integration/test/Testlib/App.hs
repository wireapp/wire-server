-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Testlib.App where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import qualified Control.Retry as Retry
import Data.Aeson hiding ((.=))
import Data.Bool (bool)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import GHC.Exception
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack, callStack)
import System.FilePath
import Testlib.JSON
import Testlib.Types
import Prelude

failApp :: (HasCallStack) => String -> App a
failApp msg = throw (AppFailure msg callStack)

readServiceConfig :: Service -> App Value
readServiceConfig = readServiceConfig' . configName

readServiceConfig' :: String -> App Value
readServiceConfig' srvName = do
  cfgFile <- asks \env -> case env.servicesCwdBase of
    Nothing -> "/etc/wire" </> srvName </> "conf" </> (srvName <> ".yaml")
    Just p -> p </> srvName </> (srvName <> ".integration.yaml")

  eith <- liftIO (Yaml.decodeFileEither cfgFile)
  case eith of
    Left err -> failApp ("Error while parsing " <> cfgFile <> ": " <> Yaml.prettyPrintParseException err)
    Right value -> pure value

data Domain = OwnDomain | OtherDomain
  deriving stock (Eq, Show, Generic)

instance MakesValue Domain where
  make OwnDomain = asks (String . T.pack . (.domain1))
  make OtherDomain = asks (String . T.pack . (.domain2))

-- | Run an action, `recoverAll`ing with exponential backoff (min step 8ms, total timeout
-- ~15s).  Search this package for examples how to use it.
--
-- Ideally, this will be the only thing you'll ever need from the retry package when writing
-- integration tests.  If you are unhappy with it, please consider making it more general in a
-- backwards-compatible way so everybody can benefit.
retryT :: App a -> App a
retryT action = Retry.recoverAll (Retry.exponentialBackoff 8000 <> Retry.limitRetries 10) (const action)

-- | Retry a request that fans out over federation on transient errors.
--
-- Conversation creates and membership changes federate to all involved remote
-- backends concurrently, and 'ensureNoUnreachableBackends' fails fast on the
-- first unreachable backend with no retries (HTTP 533). Under CI load a single
-- transient federation-ping failure (connection refused, TLS handshake, DNS) to
-- one backend can therefore surface as 533 even though the backend is healthy.
-- This tolerates 533 (unreachable backends / unexpected federation response),
-- 521 (connection refused) and 525 (SSL), in addition to 500/422. Bounded by a
-- cumulative 30s cap so genuine failures still surface.
retryTransient :: App Response -> App Response
retryTransient action = go (0 :: Int) (100_000 :: Int)
  where
    go spent delay = do
      resp <- action
      if resp.status `elem` [500, 422, 521, 525, 533] && spent < maxCumulative
        then do
          liftIO $ threadDelay delay
          go (spent + delay) (min 2_000_000 (delay * 2))
        else pure resp
    maxCumulative = 30_000_000

-- | make Bool lazy
liftBool :: (Functor f) => f Bool -> BoolT f
liftBool = MaybeT . fmap (bool Nothing (Just ()))

-- | make Bool strict
unliftBool :: (Functor f) => BoolT f -> f Bool
unliftBool = fmap isJust . runMaybeT

-- | lazy (&&)
(&&~) :: App Bool -> App Bool -> App Bool
b1 &&~ b2 = unliftBool $ liftBool b1 *> liftBool b2

infixr 3 &&~

-- | lazy (||)
(||~) :: App Bool -> App Bool -> App Bool
b1 ||~ b2 = unliftBool $ liftBool b1 <|> liftBool b2

infixr 2 ||~

-- | lazy (&&): (*>)
--   lazy (||): (<|>)
type BoolT f = MaybeT f ()

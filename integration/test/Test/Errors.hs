{-# OPTIONS -Wno-ambiguous-fields #-}

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

module Test.Errors where

import API.Brig
import Control.Monad.Codensity
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import SetupHelpers
import Testlib.Mock
import Testlib.Prelude
import Testlib.ResourcePool

testNestedError :: (HasCallStack) => App ()
testNestedError = do
  let innerError =
        object
          [ "code" .= (400 :: Int),
            "label" .= "example",
            "message" .= "Example remote federator failure"
          ]

  resourcePool <- asks resourcePool
  lowerCodensity $ do
    [res] <- acquireResources 1 resourcePool
    mockConfig <- do
      mBase <- asks (.servicesCwdBase)
      pure $ case mBase of
        Just _ ->
          -- when running locally, spawn a fake ingress returning an error
          def
            { port = Just (fromIntegral res.berNginzSslPort),
              tls = True
            }
        Nothing -> do
          -- on CI, the real federation ingress is available, so we spawn its federator upstream instead
          def
            { port = Just (fromIntegral res.berFederatorExternal),
              tls = False
            }
    void
      $ startMockServer mockConfig
      $ codensityApp
      $ \_req -> pure $ Wai.responseLBS HTTP.status400 mempty $ Aeson.encode innerError

    -- get remote user
    lift $ do
      user <- randomUser OwnDomain def
      targetId <- randomId
      let target = object ["id" .= targetId, "domain" .= res.berDomain]
      bindResponse (getUser user target) $ \resp -> do
        resp.status `shouldMatchInt` 533
        resp.json %. "inner" `shouldMatch` innerError

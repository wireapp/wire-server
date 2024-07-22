-- Disabling to stop warnings on HasCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

-- | Cloned from "/services/brig/test/integration/{Uril.hs,Util/Email.hs}"
module Util.Email where

import Bilge hiding (accept, timeout)
import Bilge.Assert
import Brig.Types.Activation
import Control.Lens (view)
import Control.Monad.Catch (MonadCatch)
import Data.ByteString.Conversion
import Data.Id
import qualified Data.ZAuth.Token as ZAuth
import Imports
import Test.Tasty.HUnit
import Util.Activation
import Util.Core
import Util.Types
import qualified Wire.API.Team.Feature as Feature
import Wire.API.User
import Wire.API.User.Activation

changeEmailBrigCreds ::
  (MonadHttp m, HasCallStack) =>
  BrigReq ->
  Cookie ->
  ZAuth.Token ZAuth.Access ->
  Email ->
  m ResponseLBS
changeEmailBrigCreds brig cky tok newEmail = do
  put
    ( brig
        . path "/access/self/email"
        . forceCookie cky
        . header "Authorization" ("Bearer " <> toByteString' tok)
        . json (EmailUpdate newEmail)
    )

-- (the proper way to do this is via 'Bilge.Request.cookie', but in our CI setup,
-- there is some issue with the cookie domain setup, and 'Bilge.cookie' adds it
-- only to the cookie jar, where it gets dropped during request compilation.)
forceCookie :: Cookie -> Request -> Request
forceCookie cky = header "Cookie" $ cookie_name cky <> "=" <> cookie_value cky

activateEmail ::
  (MonadCatch m, MonadIO m, HasCallStack) =>
  BrigReq ->
  Email ->
  (MonadHttp m) => m ()
activateEmail brig email = do
  act <- getActivationCode brig email
  case act of
    Nothing -> liftIO $ assertFailure "missing activation key/code"
    Just kc ->
      activate brig kc !!! do
        const 200 === statusCode
        const (Just False) === fmap activatedFirst . responseJsonMaybe

failActivatingEmail ::
  (MonadCatch m, MonadIO m, HasCallStack) =>
  BrigReq ->
  Email ->
  (MonadHttp m) => m ()
failActivatingEmail brig email = do
  act <- getActivationCode brig email
  liftIO $ assertEqual "there should be no pending activation" act Nothing

checkEmail ::
  (HasCallStack) =>
  UserId ->
  Maybe Email ->
  TestSpar ()
checkEmail uid expectedEmail = do
  brig <- view teBrig
  call $
    get (brig . path "/self" . zUser uid) !!! do
      const 200 === statusCode
      const expectedEmail === (userEmail <=< responseJsonMaybe)

activate ::
  (MonadHttp m, HasCallStack) =>
  BrigReq ->
  ActivationPair ->
  m ResponseLBS
activate brig (k, c) =
  get $
    brig
      . path "activate"
      . queryItem "key" (toByteString' k)
      . queryItem "code" (toByteString' c)

setSamlEmailValidation :: (HasCallStack) => TeamId -> Feature.FeatureStatus -> TestSpar ()
setSamlEmailValidation tid status = do
  galley <- view teGalley
  let req = put $ galley . paths p . json (Feature.WithStatusNoLock @Feature.ValidateSAMLEmailsConfig status Feature.ValidateSAMLEmailsConfig Feature.FeatureTTLUnlimited)
      p = ["/i/teams", toByteString' tid, "features", Feature.featureNameBS @Feature.ValidateSAMLEmailsConfig]
  call req !!! const 200 === statusCode

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
import Control.Lens (view, (^?))
import Control.Monad.Catch (MonadCatch)
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.Id hiding (client)
import qualified Data.Misc as Misc
import qualified Data.Text.Ascii as Ascii
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ZAuth.Token as ZAuth
import Imports
import Test.Tasty.HUnit
import Util.Core
import Util.Types
import qualified Wire.API.Team.Feature as Feature
import Wire.API.User
import Wire.API.User.Activation
import qualified Wire.API.User.Auth as Auth

changeEmailBrig ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
  BrigReq ->
  User ->
  Email ->
  m ResponseLBS
changeEmailBrig brig usr newEmail = do
  -- most of this code is stolen from brig integration tests
  let oldEmail = fromJust (userEmail usr)
  (cky, tok) <- do
    rsp <-
      login (emailLogin oldEmail defPassword Nothing) Auth.PersistentCookie
        <!! const 200 === statusCode
    pure (decodeCookie rsp, decodeToken rsp)
  changeEmailBrigCreds brig cky tok newEmail
  where
    emailLogin :: Email -> Misc.PlainTextPassword6 -> Maybe Auth.CookieLabel -> Auth.Login
    emailLogin e pw cl =
      Auth.PasswordLogin $
        Auth.PasswordLoginData (Auth.LoginByEmail e) pw cl Nothing

    login :: Auth.Login -> Auth.CookieType -> MonadHttp m => m ResponseLBS
    login l t =
      post $
        brig
          . path "/login"
          . (if t == Auth.PersistentCookie then queryItem "persist" "true" else id)
          . json l

    decodeCookie :: HasCallStack => Response a -> Bilge.Cookie
    decodeCookie = fromMaybe (error "missing zuid cookie") . Bilge.getCookie "zuid"

    decodeToken :: HasCallStack => Response (Maybe LByteString) -> ZAuth.Token ZAuth.Access
    decodeToken r = fromMaybe (error "invalid access_token") $ do
      x <- responseBody r
      t <- x ^? key "access_token" . _String
      fromByteString (encodeUtf8 t)

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
  MonadHttp m => m ()
activateEmail brig email = do
  act <- getActivationCode brig (Left email)
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
  MonadHttp m => m ()
failActivatingEmail brig email = do
  act <- getActivationCode brig (Left email)
  liftIO $ assertEqual "there should be no pending activation" act Nothing

checkEmail ::
  HasCallStack =>
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

getActivationCode ::
  (MonadCatch m, MonadHttp m, HasCallStack) =>
  BrigReq ->
  Either Email Phone ->
  m (Maybe (ActivationKey, ActivationCode))
getActivationCode brig ep = do
  let qry = either (queryItem "email" . toByteString') (queryItem "phone" . toByteString') ep
  r <- get $ brig . path "/i/users/activation-code" . qry
  let lbs = fromMaybe "" $ responseBody r
  let akey = ActivationKey . Ascii.unsafeFromText <$> (lbs ^? key "key" . _String)
  let acode = ActivationCode . Ascii.unsafeFromText <$> (lbs ^? key "code" . _String)
  pure $ (,) <$> akey <*> acode

setSamlEmailValidation :: HasCallStack => TeamId -> Feature.FeatureStatus -> TestSpar ()
setSamlEmailValidation tid status = do
  galley <- view teGalley
  let req = put $ galley . paths p . json (Feature.WithStatusNoLock @Feature.ValidateSAMLEmailsConfig status Feature.trivialConfig Feature.FeatureTTLUnlimited)
      p = ["/i/teams", toByteString' tid, "features", Feature.featureNameBS @Feature.ValidateSAMLEmailsConfig]
  call req !!! const 200 === statusCode

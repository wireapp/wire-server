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

module API.User.Util where

import Bilge hiding (accept, timeout)
import Bilge.Assert
import Brig.Data.PasswordReset
import Brig.Types
import Brig.Types.Intra
import Brig.Types.Team.LegalHold (LegalHoldClientRequest (..))
import Brig.Types.User.Auth hiding (user)
import qualified CargoHold.Types.V3 as CHV3
import qualified Codec.MIME.Type as MIME
import Control.Lens (preview, (^?))
import Control.Monad.Catch (MonadCatch)
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as LB
import Data.Handle (Handle (Handle))
import Data.Id hiding (client)
import Data.Misc (PlainTextPassword (..))
import Data.Range (unsafeRange)
import qualified Data.Set as Set
import qualified Data.Text.Ascii as Ascii
import qualified Data.Vector as Vec
import Imports
import Test.Tasty.HUnit
import Util

newtype ConnectionLimit = ConnectionLimit Int64

checkHandles :: Brig -> UserId -> [Text] -> Word -> Http ResponseLBS
checkHandles brig uid hs num =
  let hs' = unsafeRange hs
      num' = unsafeRange num
      js = RequestBodyLBS $ encode $ CheckHandles hs' num'
   in post (brig . path "/users/handles" . contentJson . zUser uid . body js)

randomUserWithHandle ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
  Brig ->
  m User
randomUserWithHandle brig = do
  u <- randomUser brig
  setRandomHandle brig u

setRandomHandle ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
  Brig ->
  User ->
  m User
setRandomHandle brig user = do
  h <- randomHandle
  put
    ( brig
        . path "/self/handle"
        . contentJson
        . zUser (userId user)
        . zConn "c"
        . body (RequestBodyLBS . encode $ HandleUpdate h)
    )
    !!! const 200
    === statusCode
  return user {userHandle = Just (Handle h)}

-- Note: This actually _will_ send out an email, so we ensure that the email
--       used here has a domain 'simulator.amazonses.com'.
registerUser :: Text -> Brig -> Http ResponseLBS
registerUser name brig = do
  e <- randomEmail
  let p =
        RequestBodyLBS . encode $
          object
            [ "name" .= name,
              "email" .= fromEmail e,
              "password" .= defPassword
            ]
  post (brig . path "/register" . contentJson . body p)

createRandomPhoneUser :: HasCallStack => Brig -> Http (UserId, Phone)
createRandomPhoneUser brig = do
  usr <- randomUser brig
  let uid = userId usr
  phn <- liftIO randomPhone
  -- update phone
  let phoneUpdate = RequestBodyLBS . encode $ PhoneUpdate phn
  put (brig . path "/self/phone" . contentJson . zUser uid . zConn "c" . body phoneUpdate)
    !!! (const 202 === statusCode)
  -- activate
  act <- getActivationCode brig (Right phn)
  case act of
    Nothing -> liftIO $ assertFailure "missing activation key/code"
    Just kc -> activate brig kc !!! const 200 === statusCode
  -- check new phone
  get (brig . path "/self" . zUser uid) !!! do
    const 200 === statusCode
    const (Just phn) === (userPhone <=< responseJsonMaybe)
  return (uid, phn)

initiatePasswordReset :: Brig -> Email -> Http ResponseLBS
initiatePasswordReset brig email =
  post
    ( brig
        . path "/password-reset"
        . contentJson
        . body (RequestBodyLBS . encode $ NewPasswordReset (Left email))
    )

activateEmail :: HasCallStack => Brig -> Email -> HttpT IO ()
activateEmail brig email = do
  act <- getActivationCode brig (Left email)
  case act of
    Nothing -> liftIO $ assertFailure "missing activation key/code"
    Just kc ->
      activate brig kc !!! do
        const 200 === statusCode
        const (Just False) === fmap activatedFirst . responseJsonMaybe

checkEmail :: HasCallStack => Brig -> UserId -> Email -> HttpT IO ()
checkEmail brig uid expectedEmail =
  get (brig . path "/self" . zUser uid) !!! do
    const 200 === statusCode
    const (Just expectedEmail) === (userEmail <=< responseJsonMaybe)

initiateEmailUpdate :: Brig -> Email -> UserId -> Http ResponseLBS
initiateEmailUpdate brig email uid =
  let emailUpdate = RequestBodyLBS . encode $ EmailUpdate email
   in put (brig . path "/self/email" . contentJson . zUser uid . zConn "c" . body emailUpdate)

initiateEmailUpdateNoSend :: Brig -> Email -> UserId -> Http ResponseLBS
initiateEmailUpdateNoSend brig email uid =
  let emailUpdate = RequestBodyLBS . encode $ EmailUpdate email
   in put (brig . path "/i/self/email" . contentJson . zUser uid . body emailUpdate)

preparePasswordReset :: Brig -> Email -> UserId -> PlainTextPassword -> Http CompletePasswordReset
preparePasswordReset brig email uid newpw = do
  let qry = queryItem "email" (toByteString' email)
  r <- get $ brig . path "/i/users/password-reset-code" . qry
  let lbs = fromMaybe "" $ responseBody r
  let Just pwcode = PasswordResetCode . Ascii.unsafeFromText <$> (lbs ^? key "code" . _String)
  ident <- PasswordResetIdentityKey <$> mkPasswordResetKey uid
  let complete = CompletePasswordReset ident pwcode newpw
  return complete

completePasswordReset :: Brig -> CompletePasswordReset -> Http ResponseLBS
completePasswordReset brig passwordResetData =
  post
    ( brig
        . path "/password-reset/complete"
        . contentJson
        . body (RequestBodyLBS $ encode passwordResetData)
    )

removeBlacklist :: Brig -> Email -> Http ()
removeBlacklist brig email =
  void $ delete (brig . path "/i/users/blacklist" . queryItem "email" (toByteString' email))

getClient :: Brig -> UserId -> ClientId -> Http ResponseLBS
getClient brig u c =
  get $
    brig
      . paths ["clients", toByteString' c]
      . zUser u

deleteClient :: Brig -> UserId -> ClientId -> Maybe PlainTextPassword -> Http ResponseLBS
deleteClient brig u c pw =
  delete $
    brig
      . paths ["clients", toByteString' c]
      . zUser u
      . zConn "conn"
      . contentJson
      . body payload
  where
    payload =
      RequestBodyLBS . encode $
        object
          [ "password" .= pw
          ]

listConnections :: Brig -> UserId -> Http ResponseLBS
listConnections brig u =
  get $
    brig
      . path "connections"
      . zUser u

postAutoConnection :: Brig -> UserId -> [UserId] -> Http ResponseLBS
postAutoConnection brig from to =
  post $
    brig
      . paths ["/i/users", toByteString' from, "auto-connect"]
      . contentJson
      . body payload
      . zConn "conn"
  where
    payload = RequestBodyLBS . encode $ UserSet (Set.fromList to)

setProperty :: Brig -> UserId -> ByteString -> Value -> Http ResponseLBS
setProperty brig u k v =
  put $
    brig
      . paths ["/properties", k]
      . zUser u
      . zConn "conn"
      . contentJson
      . body (RequestBodyLBS $ encode v)

getProperty :: Brig -> UserId -> ByteString -> Http ResponseLBS
getProperty brig u k =
  get $
    brig
      . paths ["/properties", k]
      . zUser u

deleteProperty :: Brig -> UserId -> ByteString -> Http ResponseLBS
deleteProperty brig u k =
  delete $
    brig
      . paths ["/properties", k]
      . zConn "conn"
      . zUser u

countCookies :: HasCallStack => Brig -> UserId -> CookieLabel -> Http (Maybe Int)
countCookies brig u label = do
  r <-
    get
      ( brig
          . path "/cookies"
          . queryItem "labels" (toByteString' label)
          . header "Z-User" (toByteString' u)
      )
      <!! const 200
      === statusCode
  return $ Vec.length <$> (preview (key "cookies" . _Array) =<< responseJsonMaybe @Value r)

assertConnections :: HasCallStack => Brig -> UserId -> [ConnectionStatus] -> Http ()
assertConnections brig u cs =
  listConnections brig u !!! do
    const 200 === statusCode
    const (Just True) === fmap (check . map status . clConnections) . responseJsonMaybe
  where
    check xs = all (`elem` xs) cs
    status c = ConnectionStatus (ucFrom c) (ucTo c) (ucStatus c)

assertEmailVisibility :: HasCallStack => Brig -> User -> User -> Bool -> Http ()
assertEmailVisibility brig a b visible =
  get (brig . paths ["users", pack . show $ userId b] . zUser (userId a)) !!! do
    const 200 === statusCode
    if visible
      then const (Just (userEmail b)) === fmap userEmail . responseJsonMaybe
      else const Nothing === (userEmail <=< responseJsonMaybe)

uploadAsset :: HasCallStack => CargoHold -> UserId -> ByteString -> Http CHV3.Asset
uploadAsset c usr dat = do
  let sts = CHV3.defAssetSettings
      ct = MIME.Type (MIME.Application "text") []
      mpb = CHV3.buildMultipartBody sts ct (LB.fromStrict dat)
  rsp <-
    post
      ( c
          . path "/assets/v3"
          . zUser usr
          . zConn "conn"
          . content "multipart/mixed"
          . lbytes (toLazyByteString mpb)
      )
      <!! const 201
      === statusCode
  responseJsonError rsp

downloadAsset :: CargoHold -> UserId -> ByteString -> Http (Response (Maybe LB.ByteString))
downloadAsset c usr ast =
  get
    ( c
        . paths ["/assets/v3", ast]
        . zUser usr
        . zConn "conn"
    )

requestLegalHoldDevice :: Brig -> UserId -> UserId -> LastPrekey -> Http ResponseLBS
requestLegalHoldDevice brig requesterId targetUserId lastPrekey' =
  post $
    brig
      . paths ["i", "clients", "legalhold", toByteString' targetUserId, "request"]
      . contentJson
      . body payload
  where
    payload =
      RequestBodyLBS . encode $
        LegalHoldClientRequest requesterId lastPrekey'

deleteLegalHoldDevice :: Brig -> UserId -> Http ResponseLBS
deleteLegalHoldDevice brig uid =
  delete $
    brig
      . paths ["i", "clients", "legalhold", toByteString' uid]
      . contentJson

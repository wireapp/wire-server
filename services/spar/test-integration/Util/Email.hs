-- | Cloned from "/services/brig/test/integration/{Uril.hs,Util/Email.hs}"
module Util.Email where

import Bilge hiding (accept, timeout)
import Bilge.Assert
import Brig.Types
import Control.Lens ((^?))
import Control.Monad.Catch (MonadCatch)
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.Id hiding (client)
import qualified Data.Text.Ascii as Ascii
import Imports
import Test.Tasty.HUnit
import Util.Core
import Util.Types

activateEmail ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
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
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
  BrigReq ->
  Email ->
  MonadHttp m => m ()
failActivatingEmail brig email = do
  act <- getActivationCode brig (Left email)
  liftIO $ assertEqual "there should be no pending activation" act Nothing

checkEmail ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
  BrigReq ->
  UserId ->
  Email ->
  m ()
checkEmail brig uid expectedEmail =
  get (brig . path "/self" . zUser uid) !!! do
    const 200 === statusCode
    const (Just expectedEmail) === (userEmail <=< responseJsonMaybe)

activate ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
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
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
  BrigReq ->
  Either Email Phone ->
  m (Maybe (ActivationKey, ActivationCode))
getActivationCode brig ep = do
  let qry = either (queryItem "email" . toByteString') (queryItem "phone" . toByteString') ep
  r <- get $ brig . path "/i/users/activation-code" . qry
  let lbs = fromMaybe "" $ responseBody r
  let akey = ActivationKey . Ascii.unsafeFromText <$> (lbs ^? key "key" . _String)
  let acode = ActivationCode . Ascii.unsafeFromText <$> (lbs ^? key "code" . _String)
  return $ (,) <$> akey <*> acode

{-# LANGUAGE OverloadedStrings #-}

module Network.Wire.Client.API.User
  ( registerUser,
    activateKey,
    getSelfProfile,
    getProfile,
    connectTo,
    updateConnection,
    getConnection,
    module M,
  )
where

import Bilge
import Brig.Types as M
import Control.Monad.Catch (MonadMask)
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Conversion
import Data.Id
import Data.List.NonEmpty
import Data.Text (pack)
import Imports
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status hiding (statusCode)
import Network.Wire.Client.HTTP
import Network.Wire.Client.Monad
import Network.Wire.Client.Session

-------------------------------------------------------------------------------
-- Unauthenticated

registerUser :: (MonadClient m, MonadUnliftIO m, MonadMask m) => NewUser -> m User
registerUser u = clientRequest req rsc readBody
  where
    req =
      method POST
        . path "/register"
        . acceptJson
        . json u
        $ empty
    rsc = status201 :| []

activateKey :: (MonadClient m, MonadUnliftIO m, MonadMask m) => ActivationKey -> ActivationCode -> m Bool
activateKey (ActivationKey key) (ActivationCode code) = do
  status <- clientRequest req rsc (return . statusCode)
  return $ status /= 404
  where
    req =
      method GET
        . path "/activate"
        . query [("key", Just (toByteString' key)), ("code", Just (toByteString' code))]
        $ empty
    rsc = status200 :| [status204, status404]

-------------------------------------------------------------------------------
-- Authenticated

getSelfProfile :: (MonadSession m, MonadUnliftIO m, MonadMask m) => m User
getSelfProfile = sessionRequest req rsc readBody
  where
    req =
      method GET
        . path "/self"
        . acceptJson
        $ empty
    rsc = status200 :| []

getProfile :: (MonadSession m, MonadUnliftIO m, MonadMask m) => UserId -> m UserProfile
getProfile uid = sessionRequest req rsc readBody
  where
    req =
      method GET
        . paths ["users", C.pack (show uid)]
        . acceptJson
        $ empty
    rsc = status200 :| []

connectTo :: (MonadSession m, MonadUnliftIO m, MonadMask m) => ConnectionRequest -> m UserConnection
connectTo cr = sessionRequest req rsc readBody
  where
    req =
      method POST
        . path "/connections"
        . acceptJson
        . json cr
        $ empty
    rsc = status201 :| [status200]

updateConnection :: (MonadSession m, MonadUnliftIO m, MonadMask m) => UserId -> ConnectionUpdate -> m UserConnection
updateConnection u cu = sessionRequest req rsc readBody
  where
    req =
      method PUT
        . paths ["connections", C.pack (show u)]
        . acceptJson
        . json cu
        $ empty
    rsc = status200 :| []

getConnection :: (MonadSession m, MonadUnliftIO m, MonadMask m) => UserId -> m (Maybe UserConnection)
getConnection u = do
  rs <- sessionRequest req rsc consumeBody
  case statusCode rs of
    200 -> responseJsonThrow (ParseError . pack) rs
    404 -> return Nothing
    _ -> unexpected rs "getConnection: status code"
  where
    req =
      method GET
        . paths ["connections", C.pack (show u)]
        . acceptJson
        $ empty
    rsc = status200 :| [status404]

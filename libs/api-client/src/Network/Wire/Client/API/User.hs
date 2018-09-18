{-# LANGUAGE OverloadedStrings #-}

module Network.Wire.Client.API.User
    ( registerUser
    , activateKey
    , getSelfProfile
    , getProfile
    , connectTo
    , updateConnection
    , getConnection
    , module M
    ) where

import Bilge
import Brig.Types as M
import Data.ByteString.Conversion
import Data.Id
import Data.List.NonEmpty
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status hiding (statusCode)
import Network.Wire.Client.HTTP
import Network.Wire.Client.Monad
import Network.Wire.Client.Session

import qualified Data.ByteString.Char8 as C

-------------------------------------------------------------------------------
-- Unauthenticated

registerUser :: MonadClient m => NewUser -> m User
registerUser u = clientRequest Brig req rsc readBody
  where
    req = method POST
        . path "/register"
        . acceptJson
        . json u
    rsc = status201 :| []

activateKey :: MonadClient m => ActivationKey -> ActivationCode -> m Bool
activateKey (ActivationKey key) (ActivationCode code) = do
    status <- clientRequest Brig req rsc (return . statusCode)
    return $ status /= 404
  where
    req = method GET
        . path "/activate"
        . query [("key", Just (toByteString' key)), ("code", Just (toByteString' code))]
    rsc = status200 :| [status204, status404]

-------------------------------------------------------------------------------
-- Authenticated

getSelfProfile :: MonadSession m => m User
getSelfProfile = sessionRequest Brig req rsc readBody
  where
    req = method GET
        . path "/self"
        . acceptJson
    rsc = status200 :| []

getProfile :: MonadSession m => UserId -> m UserProfile
getProfile uid = sessionRequest Brig req rsc readBody
  where
    req = method GET
        . paths ["users", C.pack (show uid)]
        . acceptJson
    rsc = status200 :| []

connectTo :: MonadSession m => ConnectionRequest -> m UserConnection
connectTo cr = sessionRequest Brig req rsc readBody
  where
    req = method POST
        . path "/connections"
        . acceptJson
        . json cr
    rsc = status201 :| [status200]

updateConnection :: MonadSession m => UserId -> ConnectionUpdate -> m UserConnection
updateConnection u cu = sessionRequest Brig req rsc readBody
  where
    req = method PUT
        . paths ["connections", C.pack (show u)]
        . acceptJson
        . json cu
    rsc = status200 :| []

getConnection :: MonadSession m => UserId -> m (Maybe UserConnection)
getConnection u = do
    rs <- sessionRequest Brig req rsc consumeBody
    case statusCode rs of
        200 -> fromBody rs
        404 -> return Nothing
        _   -> unexpected rs "getConnection: status code"
  where
    req = method GET
        . paths ["connections", C.pack (show u)]
        . acceptJson
    rsc = status200 :| [status404]

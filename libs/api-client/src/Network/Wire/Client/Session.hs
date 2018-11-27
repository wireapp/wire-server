{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Wire.Client.Session
    ( Session
    , MonadSession (..)
    , runSession
    , evalSession
    , asyncSession
    , asyncEvalSession
    , sessionRequest
    ) where

import Imports hiding (log)
import Bilge
import Control.Concurrent.Async
import Control.Exception (throwIO)
import Control.Monad.Trans.State.Strict
import Data.List.NonEmpty
import Network.HTTP.Types.Status
import Network.Wire.Client.API.Auth
import Network.Wire.Client.HTTP
import Network.Wire.Client.Monad
import System.Logger.Class

import qualified Control.Monad.Trans.State.Strict as State

newtype Session a = Session (StateT Auth Client a)
    deriving (Functor, Applicative, Monad, MonadIO)

class (Functor m, MonadClient m) => MonadSession m where
    getAuth :: m Auth
    setAuth :: Auth -> m ()

instance MonadHttp Session where
    getManager = Session $ lift getManager

instance MonadClient Session where
    getServer = Session $ lift getServer
    getLogger = Session $ lift getLogger

instance MonadSession Session where
    getAuth = Session State.get
    setAuth = Session . State.put

instance MonadLogger Session where
    log l = Session . lift . log l

-- | Perform an HTTP request against the API in the context of a session,
-- i.e. including an 'Authorization' header.
sessionRequest :: MonadSession m
               => Request                       -- ^ The request to send.
               -> NonEmpty Status               -- ^ Expected response codes.
               -> (Response BodyReader -> IO a) -- ^ Handler function.
               -> m a
sessionRequest rq expected f =
    either retry return =<< exec (\rs ->
        if Bilge.statusCode rs == 401
            then Left  <$> mkErrorResponse rs
            else Right <$> f rs)
  where
    exec h = do
        Auth _ t <- getAuth
        clientRequest (token t rq) (status401 <| expected) h

    retry e = do
        a  <- getAuth >>= refreshAuth
        maybe (liftIO $ throwIO e) setAuth a
        exec f

runSession :: Auth -> Session a -> Client (a, Auth)
runSession auth (Session s) = runStateT s auth

evalSession :: Auth -> Session a -> Client a
evalSession auth (Session s) = evalStateT s auth

asyncSession :: Session a -> Session (Async (a, Auth))
asyncSession s = do
    auth <- Session State.get
    Session . lift $ asyncClient (runSession auth s)

asyncEvalSession :: Session a -> Session (Async a)
asyncEvalSession s = fmap fst <$> asyncSession s

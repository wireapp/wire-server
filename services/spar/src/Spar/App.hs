{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Spar.App where

import Bilge
import Cassandra
import Control.Exception (SomeException, assert)
import Control.Monad.Except
import Control.Monad.Reader
import Data.EitherR (fmapL)
import Data.Id
import Data.String.Conversions
import GHC.Stack
import Lens.Micro
import SAML2.WebSSO hiding (UserRef(..))
import Servant
import Spar.API.Instances ()
import Spar.API.Swagger ()
import Spar.Error
import Spar.Options as Options
import Spar.Types
import Web.Cookie (SetCookie, renderSetCookie)

import qualified Cassandra as Cas
import qualified Control.Monad.Catch as Catch
import qualified Data.ByteString.Builder as Builder
import qualified Data.UUID.V4 as UUID
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data as Data
import qualified Spar.Intra.Brig as Intra
import qualified System.Logger as Log


newtype Spar a = Spar { fromSpar :: ReaderT Env (ExceptT SparError IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError SparError)

data Env = Env
  { sparCtxOpts         :: Opts
  , sparCtxLogger       :: Log.Logger
  , sparCtxCas          :: Cas.ClientState
  , sparCtxHttpManager  :: Bilge.Manager
  , sparCtxHttpBrig     :: Bilge.Request
  }

instance HasConfig Spar where
  type ConfigExtra Spar = IdPExtra
  getConfig = asks (saml . sparCtxOpts)

instance SP Spar where
  -- FUTUREWORK: optionally use 'field' to index user or idp ids for easier logfile processing.
  -- (FUTUREWORK: we could also call 'show' on the message instead of manually removing newlines,
  -- then we could cut&paste it into ghci and make everything visible again.)
  logger lv mg = asks sparCtxLogger >>= \lg -> Spar $ Log.log lg (toLevel lv) mg'
    where
      mg' = Log.msg $ flatten <$> mg
      flatten '\n' = ' '
      flatten '\r' = ' '
      flatten '\t' = ' '
      flatten c    = c

toLevel :: SAML.Level -> Log.Level
toLevel = \case
  SAML.Fatal -> Log.Fatal
  SAML.Error -> Log.Error
  SAML.Warn  -> Log.Warn
  SAML.Info  -> Log.Info
  SAML.Debug -> Log.Debug
  SAML.Trace -> Log.Trace

fromLevel :: Log.Level -> SAML.Level
fromLevel = \case
  Log.Fatal -> SAML.Fatal
  Log.Error -> SAML.Error
  Log.Warn  -> SAML.Warn
  Log.Info  -> SAML.Info
  Log.Debug -> SAML.Debug
  Log.Trace -> SAML.Trace

instance SPStore Spar where
  storeRequest i r      = wrapMonadClientWithEnv $ Data.storeRequest i r
  checkAgainstRequest r = wrapMonadClientWithEnv $ Data.checkAgainstRequest r
  storeAssertion i r    = wrapMonadClientWithEnv $ Data.storeAssertion i r

instance SPStoreIdP SparError Spar where
  storeIdPConfig :: IdPConfig IdPExtra -> Spar ()
  storeIdPConfig idp = wrapMonadClient $ Data.storeIdPConfig idp

  getIdPConfig :: IdPId -> Spar (IdPConfig IdPExtra)
  getIdPConfig = (>>= maybe (throwSpar SparNotFound) pure) . wrapMonadClientWithEnv . Data.getIdPConfig

  getIdPConfigByIssuer :: Issuer -> Spar (IdPConfig IdPExtra)
  getIdPConfigByIssuer = (>>= maybe (throwSpar SparNotFound) pure) . wrapMonadClientWithEnv . Data.getIdPConfigByIssuer

-- | 'wrapMonadClient' with an 'Env' in a 'ReaderT'.
wrapMonadClientWithEnv :: ReaderT Data.Env Cas.Client a -> Spar a
wrapMonadClientWithEnv action = do
  denv <- Data.mkEnv <$> (sparCtxOpts <$> ask) <*> (fromTime <$> getNow)
  wrapMonadClient (action `runReaderT` denv)

-- | Call a cassandra command in the 'Spar' monad.  Catch all exceptions and re-throw them as 500 in
-- Handler.
wrapMonadClient :: Cas.Client a -> Spar a
wrapMonadClient action = do
  Spar $ do
    ctx <- asks sparCtxCas
    runClient ctx action `Catch.catch`
      (throwSpar . SparCassandraError . cs . show @SomeException)

insertUser :: SAML.UserRef -> UserId -> Spar ()
insertUser uref uid = wrapMonadClient $ Data.insertUser uref uid

-- | Look up user locally, then in brig, then return the 'UserId'.  If either lookup fails, return
-- 'Nothing'.  See also: 'Spar.App.createUser'.
--
-- ASSUMPTIONS: User creation on brig/galley is idempotent.  Any incomplete creation (because of
-- brig or galley crashing) will cause the lookup here to yield invalid user.
getUser :: SAML.UserRef -> Spar (Maybe UserId)
getUser uref = do
  muid <- wrapMonadClient $ Data.getUser uref
  case muid of
    Nothing -> pure Nothing
    Just uid -> Intra.confirmUserId uid

-- | Create a fresh 'Data.Id.UserId', store it on C* locally together with 'SAML.UserRef', then
-- create user on brig with that 'UserId'.  See also: 'Spar.App.getUser'.
--
-- The manual for the team admin should say this: when deleting a user, delete it on the IdP first,
-- then delete it on the team admin page in wire.  If a user is deleted in wire but not in the IdP,
-- it will be recreated on the next successful login attempt.
--
-- When an sso login succeeds for a user that is marked as deleted in brig, it is recreated by spar.
-- This is necessary because brig does not talk to spar when deleting users, and we may have
-- 'UserId' records on spar that are deleted on brig.  Without this lenient behavior, there would be
-- no way for admins to reuse a 'SAML.UserRef' if it has ever been associated with a deleted user in
-- the past.
--
-- FUTUREWORK: once we support <https://github.com/wireapp/hscim scim>, brig will refuse to delete
-- users that have an sso id, unless the request comes from spar.  then we can make users
-- undeletable in the team admin page, and ask admins to go talk to their IdP system.
createUser :: SAML.UserRef -> Spar UserId
createUser suid = do
  buid <- Id <$> liftIO UUID.nextRandom
  teamid <- (^. idpExtraInfo . idpeTeam) <$> getIdPConfigByIssuer (suid ^. uidTenant)
  insertUser suid buid
  buid' <- Intra.createUser suid buid teamid
  assert (buid == buid') $ pure buid


instance SPHandler SparError Spar where
  type NTCTX Spar = Env
  nt ctx (Spar action) = Handler . ExceptT . fmap (fmapL sparToServantErr) . runExceptT $ runReaderT action ctx

instance MonadHttp Spar where
  getManager = asks sparCtxHttpManager

instance Intra.MonadSparToBrig Spar where
  call modreq = do
    req <- asks sparCtxHttpBrig
    httpLbs req modreq


-- | The from of the response on the finalize-login request depends on the verdict (denied or
-- granted), plus the choice that the client has made during the initiate-login request.  If the
-- client is mobile, it has picked error and success redirect urls; if the client is web, it has
-- done nothing and will be served with an HTML page that it can process to decide
-- whether to log the user in or show an error.
--
-- The HTML page is empty and has a title element with contents @wire:sso:<outcome>@.  This is
-- chosen to be easily parseable and not be the title of any page sent by the IdP while it
-- negotiates with the user.
--
-- (coming up: mobile case)  -- TODO
verdictHandler :: HasCallStack => SAML.AuthnResponse -> SAML.AccessVerdict -> Spar SAML.ResponseVerdict
verdictHandler _ = verdictHandler'
  -- Since saml2-web-sso validation guarantees that the signed in-response-to info in the assertions
  -- matches the unsigned in-response-to field in the 'SAML.Response', we can just go for the latter.

verdictHandler' :: HasCallStack => SAML.AccessVerdict -> Spar SAML.ResponseVerdict
verdictHandler' = \case
  SAML.AccessDenied reasons -> do
    SAML.logger SAML.Debug (show reasons)
    pure forbiddenPage
  SAML.AccessGranted userref -> do
    uid :: UserId    <- maybe (createUser userref) pure =<< getUser userref
    cky :: SetCookie <- Intra.ssoLogin uid  -- TODO: can this be a race condition?  (user is not
                                            -- quite created yet when we ask for a cookie?  do we do
                                            -- quorum reads / writes here?  writes: probably yes,
                                            -- reads: probably no.)
    pure $ successPage cky
  where
    forbiddenPage :: SAML.ResponseVerdict
    forbiddenPage = ServantErr
      { errHTTPCode     = 200
      , errReasonPhrase = "forbidden"  -- (not sure what this is used for)
      , errBody         = easyHtml $ "<head><title>wire:sso:error:forbidden</title></head>"
      , errHeaders      = []
      }

    successPage :: SetCookie -> SAML.ResponseVerdict
    successPage cky = ServantErr
      { errHTTPCode     = 200
      , errReasonPhrase = "success"
      , errBody         = easyHtml $ "<head><title>wire:sso:success</title></head>"
      , errHeaders      = [("Set-Cookie", cs . Builder.toLazyByteString . renderSetCookie $ cky)]
      }

easyHtml :: LBS -> LBS
easyHtml doc =
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" <>
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" <>
  "<html xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\">" <> doc <> "</html>"

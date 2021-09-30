{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.SAML2.SAML2WebSso where

import qualified Control.Monad.Catch as Catch
import Control.Monad.Except
import Data.Id (TeamId)
import Data.String.Conversions (cs)
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal.Tactics
import SAML2.WebSSO hiding (Error)
import qualified SAML2.WebSSO as SAML hiding (Error)
import qualified Spar.App as App
import Spar.Error (SparCustomError (SparCassandraError), SparError)
import Spar.Sem.AReqIDStore (AReqIDStore)
import qualified Spar.Sem.AReqIDStore as AReqIDStore
import Spar.Sem.AssIDStore (AssIDStore)
import qualified Spar.Sem.AssIDStore as AssIDStore
import qualified Spar.Sem.IdP as IdPEffect
import Spar.Sem.Logger (Logger)
import qualified Spar.Sem.Logger as Logger
import Spar.Sem.SAML2
import Wire.API.User.IdentityProvider (WireIdP)
import Wire.API.User.Saml

wrapMonadClientBlah :: Members '[Error SparError, Final IO] r => Sem r a -> Blah r a
wrapMonadClientBlah action =
  Blah $
    action
      `Catch.catch` (throw . SAML.CustomError . SparCassandraError . cs . show @SomeException)

newtype Blah r a = Blah {unBlah :: Sem r a}
  deriving (Functor, Applicative, Monad)

instance Member (Input Opts) r => HasConfig (Blah r) where
  getConfig = Blah $ inputs saml

instance Members '[Input Opts, Logger String] r => HasLogger (Blah r) where
  logger lvl = Blah . Logger.log lvl

instance Member (Embed IO) r => MonadIO (Blah r) where
  liftIO = Blah . embed @IO

instance Member (Embed IO) r => HasCreateUUID (Blah r)

instance Member (Embed IO) r => HasNow (Blah r)

instance Members '[Error SparError, Final IO, AReqIDStore] r => SPStoreID AuthnRequest (Blah r) where
  storeID = (wrapMonadClientBlah .) . AReqIDStore.store
  unStoreID = wrapMonadClientBlah . AReqIDStore.unStore
  isAliveID = wrapMonadClientBlah . AReqIDStore.isAlive

instance Members '[Error SparError, Final IO, AssIDStore] r => SPStoreID Assertion (Blah r) where
  storeID = (wrapMonadClientBlah .) . AssIDStore.store
  unStoreID = wrapMonadClientBlah . AssIDStore.unStore
  isAliveID = wrapMonadClientBlah . AssIDStore.isAlive

instance Members '[Error SparError, IdPEffect.IdP, Final IO] r => SPStoreIdP SparError (Blah r) where
  type IdPConfigExtra (Blah r) = WireIdP
  type IdPConfigSPId (Blah r) = TeamId

  storeIdPConfig = Blah . App.runSparInSem . App.storeIdPConfig
  getIdPConfig = Blah . App.runSparInSem . App.getIdPConfig
  getIdPConfigByIssuerOptionalSPId a = Blah . App.runSparInSem . App.getIdPConfigByIssuerOptionalSPId a

instance Member (Error SparError) r => MonadError SparError (Blah r) where
  throwError = Blah . throw
  catchError m handler = Blah $ catch (unBlah m) $ unBlah . handler

saml2ToSaml2WebSso ::
  forall r a.
  Members
    '[ AReqIDStore,
       AssIDStore,
       Error SparError,
       IdPEffect.IdP,
       Input Opts,
       Logger String,
       Embed IO,
       Final IO
     ]
    r =>
  Sem (SAML2 ': r) a ->
  Sem r a
saml2ToSaml2WebSso =
  interpretH $ \case
    AuthReq n ma i -> do
      get_a <- runT ma
      ins <- getInspectorT
      x <- raise $ unBlah $ SAML.authreq @_ @SparError n (inspectOrBomb ins get_a) i
      s <- getInitialStateT
      pure $ x <$ s
    AuthResp mitlt ma mb mc ab -> do
      get_a <- runT ma
      get_b <- runT mb
      get_c <- bindT $ uncurry mc
      ins <- getInspectorT
      s <- getInitialStateT

      x <- raise $ unBlah $ SAML.authresp mitlt (inspectOrBomb ins get_a) (inspectOrBomb ins get_b) (\x y -> inspectOrBomb ins $ get_c $ (x, y) <$ s) ab
      pure $ x <$ s
    Meta t ma mb -> do
      get_a <- runT ma
      get_b <- runT mb
      ins <- getInspectorT
      x <- raise $ unBlah $ SAML.meta t (inspectOrBomb ins get_a) (inspectOrBomb ins get_b)
      s <- getInitialStateT
      pure $ x <$ s
    ToggleCookie sbs mp -> do
      liftT $ unBlah $ SAML.toggleCookie sbs mp

inspectOrBomb ::
  Members
    '[ AReqIDStore,
       AssIDStore,
       Error SparError,
       IdPEffect.IdP,
       Logger String,
       Input Opts,
       Embed IO,
       Final IO
     ]
    r =>
  Inspector f ->
  Sem (SAML2 : r) (f b) ->
  Blah r b
inspectOrBomb ins get_a = do
  fa <- Blah $ saml2ToSaml2WebSso get_a
  maybe
    (error "saml2ToSaml2WebSso called with an uninspectable weaving functor")
    pure
    $ inspect ins fa

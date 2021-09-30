{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.SAML2.SAML2WebSso where

import Control.Monad.Except
import Data.Id (TeamId)
import Imports
import Polysemy
import Spar.Sem.SAML2
import Imports
import qualified SAML2.WebSSO as SAML
import SAML2.WebSSO
import qualified Spar.Sem.Logger as Logger
import Wire.API.User.Saml
import Polysemy.Input
import Polysemy.Internal.Tactics
import SAML2.WebSSO
import qualified SAML2.WebSSO as SAML
import Spar.Error (SparError)
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

wrapMonadClientSem :: a -> a
wrapMonadClientSem = id

newtype Blah r a = Blah { unBlah :: Sem r a }
  deriving (Functor, Applicative, Monad)

instance Member (Input Opts) r => HasConfig (Blah r) where
  getConfig = Blah $ inputs saml

instance Members '[Input Opts, Logger String] r => HasLogger (Blah r) where
  logger lvl = Blah . Logger.log lvl

instance Member (Embed IO) r => MonadIO (Blah r) where
  liftIO = Blah . embed @IO

instance Member (Embed IO) r => HasCreateUUID (Blah r)

instance Member (Embed IO) r => HasNow (Blah r)

instance Member AReqIDStore r => SPStoreID AuthnRequest (Blah r) where
  storeID i r = Blah $ wrapMonadClientSem $ AReqIDStore.store i r
  unStoreID r = Blah $ wrapMonadClientSem $ AReqIDStore.unStore r
  isAliveID r = Blah $ wrapMonadClientSem $ AReqIDStore.isAlive r

instance Member AssIDStore r => SPStoreID Assertion (Blah r) where
  storeID i r = Blah $ wrapMonadClientSem $ AssIDStore.store i r
  unStoreID r = Blah $ wrapMonadClientSem $ AssIDStore.unStore r
  isAliveID r = Blah $ wrapMonadClientSem $ AssIDStore.isAlive r

instance Member IdPEffect.IdP r => SPStoreIdP SparError (Blah r) where
  type IdPConfigExtra (Blah r) = WireIdP
  type IdPConfigSPId (Blah r) = TeamId

  storeIdPConfig = undefined -- storeIdPConfig'
  getIdPConfig = undefined -- getIdPConfig'
  getIdPConfigByIssuerOptionalSPId = undefined -- getIdPConfigByIssuerOptionalSPId'

instance MonadError SparError (Blah r) where
  throwError = undefined
  catchError = undefined

saml2ToSaml2WebSso ::
  forall r a.
  Members '[AReqIDStore, AssIDStore, IdPEffect.IdP, Input Opts, Logger String, Embed IO] r =>
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
  Members '[AReqIDStore, AssIDStore, IdPEffect.IdP, Logger String, Input Opts, Embed IO] r =>
  Inspector f ->
  Sem (SAML2 : r) (f b) ->
  Blah r b
inspectOrBomb ins get_a = do
  fa <- Blah $ saml2ToSaml2WebSso get_a
  maybe
    (error "saml2ToSaml2WebSso called with an uninspectable weaving functor")
    pure $ inspect ins fa



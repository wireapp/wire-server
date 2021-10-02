{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.SAMLUserStore.Cassandra where

import Cassandra
import qualified Control.Monad.Catch as Catch
import Data.String.Conversions (cs)
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Final
import qualified SAML2.WebSSO.Error as SAML
import qualified Spar.Data as Data
import Spar.Error
import Spar.Sem.SAMLUserStore

samlUserStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (SAMLUserStore ': r) a ->
  Sem r a
samlUserStoreToCassandra =
  interpret $
    embed . \case
      Insert ur uid -> Data.insertSAMLUser ur uid
      Get ur -> Data.getSAMLUser ur
      GetAnyByIssuer is -> Data.getSAMLAnyUserByIssuer is
      GetSomeByIssuer is -> Data.getSAMLSomeUsersByIssuer is
      DeleteByIssuer is -> Data.deleteSAMLUsersByIssuer is
      Delete uid ur -> Data.deleteSAMLUser uid ur

interpretClientToIO ::
  Members '[Error SparError, Final IO] r =>
  ClientState ->
  Sem (Embed Client ': r) a ->
  Sem r a
interpretClientToIO ctx = interpret $ \case
  Embed action -> withStrategicToFinal @IO $ do
    action' <- liftS $ runClient ctx action
    st <- getInitialStateS
    handler' <- bindS $ throw @SparError . SAML.CustomError . SparCassandraError . cs . show @SomeException
    pure $ action' `Catch.catch` \e -> handler' $ e <$ st

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.DefaultSsoCode.Cassandra where

import Cassandra
import Imports
import Polysemy
import qualified Spar.Data as Data
import Spar.Sem.DefaultSsoCode

defaultSsoCodeToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (DefaultSsoCode ': r) a ->
  Sem r a
defaultSsoCodeToCassandra =
  interpret $ embed @m . \case
    Get -> Data.getDefaultSsoCode
    Store ip -> Data.storeDefaultSsoCode ip
    Delete -> Data.deleteDefaultSsoCode


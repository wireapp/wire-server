{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.BindCookie.Cassandra where

-- import Cassandra
-- import Imports
-- import Polysemy
-- import qualified Spar.Data as Data
-- import Spar.Sem.BindCookie

-- bindCookieToCassandra ::
--   forall m r a.
--   (MonadClient m, Member (Embed m) r) =>
--   Sem (BindCookie ': r) a ->
--   Sem r a
-- bindCookieToCassandra =
--   interpret $ embed @m . \case
--     Insert s itlu n -> Data.insertBindCookie s itlu n
--     Lookup b -> Data.lookupBindCookie b

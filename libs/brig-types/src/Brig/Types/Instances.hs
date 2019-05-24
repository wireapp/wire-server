{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Brig.Types.Instances () where

#ifdef WITH_CQL
import Imports
import Brig.Types.Team.LegalHold
import Cassandra.CQL

instance Cql LegalHoldStatus where
    ctype = Tagged IntColumn

    fromCql (CqlInt n) = case n of
        0 -> pure $ LegalHoldDisabled
        1 -> pure $ LegalHoldEnabled
        _ -> fail "fromCql: Invalid LegalHoldStatus"
    fromCql _           = fail "fromCql: LegalHoldStatus: CqlInt expected"

    toCql LegalHoldDisabled = CqlInt 0
    toCql LegalHoldEnabled = CqlInt 1
#endif

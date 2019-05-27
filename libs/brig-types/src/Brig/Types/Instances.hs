{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Brig.Types.Instances () where

#ifdef WITH_CQL
import Imports
import Brig.Types.Team.LegalHold
import Brig.Types.Client.Prekey
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

instance Cql UserLegalHoldStatus where
    ctype = Tagged IntColumn

    fromCql (CqlInt n) = case n of
        0 -> pure $ UserLegalHoldDisabled
        1 -> pure $ UserLegalHoldPending
        2 -> pure $ UserLegalHoldEnabled
        _ -> fail "fromCql: Invalid UserLegalHoldStatus"
    fromCql _           = fail "fromCql: UserLegalHoldStatus: CqlInt expected"

    toCql UserLegalHoldDisabled = CqlInt 0
    toCql UserLegalHoldPending = CqlInt 1
    toCql UserLegalHoldEnabled = CqlInt 2

instance Cql PrekeyId where
    ctype = Tagged IntColumn
    toCql = CqlInt . fromIntegral . keyId
    fromCql (CqlInt i) = return $ PrekeyId (fromIntegral i)
    fromCql _            = fail "PrekeyId: Int expected"

#endif

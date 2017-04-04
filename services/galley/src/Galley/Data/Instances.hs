{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Galley.Data.Instances where

import Cassandra.CQL
import Galley.Types
import Galley.Types.Bot

deriving instance Cql ServiceToken

instance Cql ConvType where
    ctype = Tagged IntColumn

    toCql RegularConv = CqlInt 0
    toCql SelfConv    = CqlInt 1
    toCql One2OneConv = CqlInt 2
    toCql ConnectConv = CqlInt 3

    fromCql (CqlInt i) = case i of
        0 -> return RegularConv
        1 -> return SelfConv
        2 -> return One2OneConv
        3 -> return ConnectConv
        n -> fail $ "unexpected conversation-type: " ++ show n
    fromCql _ = fail "conv-type: int expected"

instance Cql Access where
    ctype = Tagged IntColumn

    toCql PrivateAccess = CqlInt 1
    toCql InviteAccess  = CqlInt 2
    toCql LinkAccess    = CqlInt 3

    fromCql (CqlInt i) = case i of
        1 -> return PrivateAccess
        2 -> return InviteAccess
        3 -> return LinkAccess
        n -> fail $ "Unexpected Access value: " ++ show n
    fromCql _ = fail "Access value: int expected"

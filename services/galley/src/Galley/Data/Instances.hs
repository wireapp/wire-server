{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DataKinds                  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Galley.Data.Instances where

import Cassandra.CQL
import Control.Lens ((^.))
import Control.Error (note)
import Data.Int
import Galley.Types
import Galley.Types.Bot
import Galley.Types.Teams
import Galley.Types.Teams.Intra

import qualified Data.Set

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
    toCql CodeAccess    = CqlInt 4

    fromCql (CqlInt i) = case i of
        1 -> return PrivateAccess
        2 -> return InviteAccess
        3 -> return LinkAccess
        4 -> return CodeAccess
        n -> fail $ "Unexpected Access value: " ++ show n
    fromCql _ = fail "Access value: int expected"


instance Cql Permissions where
    ctype = Tagged $ UdtColumn "permissions" [("self", BigIntColumn), ("copy", BigIntColumn)]

    toCql p =
        let f = CqlBigInt . fromIntegral . permsToInt in
        CqlUdt [("self", f (p^.self)), ("copy", f (p^.copy))]

    fromCql (CqlUdt p) = do
        let f = intToPerms . fromIntegral :: Int64 -> Data.Set.Set Perm
        s <- note "missing 'self' permissions" ("self" `lookup` p) >>= fromCql
        d <- note "missing 'copy' permissions" ("copy" `lookup` p) >>= fromCql
        r <- note "invalid permissions" (newPermissions (f s) (f d))
        pure r
    fromCql _ = fail "permissions: udt expected"


instance Cql ConvTeamInfo where
    ctype = Tagged $ UdtColumn "teaminfo" [("teamid", UuidColumn), ("managed", BooleanColumn)]

    toCql t = CqlUdt [("teamid", toCql (cnvTeamId t)), ("managed", toCql (cnvManaged t))]

    fromCql (CqlUdt u) = do
        t <- note "missing 'teamid' in teaminfo" ("teamid" `lookup` u) >>= fromCql
        m <- note "missing 'managed' in teaminfo" ("managed" `lookup` u) >>= fromCql
        pure (ConvTeamInfo t m)

    fromCql _ = fail "teaminfo: udt expected"


instance Cql TeamBinding where
    ctype = Tagged BooleanColumn

    toCql Binding    = CqlBoolean True
    toCql NonBinding = CqlBoolean False

    fromCql (CqlBoolean True)  = pure Binding
    fromCql (CqlBoolean False) = pure NonBinding
    fromCql _                  = fail "teambinding: boolean expected"


instance Cql TeamStatus where
    ctype = Tagged IntColumn

    toCql Active          = CqlInt 0
    toCql PendingDelete   = CqlInt 1
    toCql Deleted         = CqlInt 2
    toCql Suspended       = CqlInt 3
    toCql PendingActive   = CqlInt 4

    fromCql (CqlInt i) = case i of
        0 -> return Active
        1 -> return PendingDelete
        2 -> return Deleted
        3 -> return Suspended
        4 -> return PendingActive
        n -> fail $ "unexpected team-status: " ++ show n
    fromCql _ = fail "team-status: int expected"

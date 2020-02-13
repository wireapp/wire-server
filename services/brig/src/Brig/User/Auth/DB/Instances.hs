{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Brig.User.Auth.DB.Instances
  (
  )
where

import Brig.Types.User.Auth
import Cassandra.CQL
import Data.Id ()
import Data.Misc ()
import Data.Range ()
import Data.Text.Ascii ()
import Imports

deriving instance Cql CookieLabel

deriving instance Cql LoginCode

instance Cql CookieId where
  ctype = Tagged BigIntColumn
  toCql = CqlBigInt . fromIntegral . cookieIdNum

  fromCql (CqlBigInt i) = return (CookieId (fromIntegral i))
  fromCql _ = fail "fromCql: invalid cookie id"

instance Cql CookieType where
  ctype = Tagged IntColumn

  toCql SessionCookie = CqlInt 0
  toCql PersistentCookie = CqlInt 1

  fromCql (CqlInt 0) = return SessionCookie
  fromCql (CqlInt 1) = return PersistentCookie
  fromCql _ = fail "fromCql: invalid cookie type"

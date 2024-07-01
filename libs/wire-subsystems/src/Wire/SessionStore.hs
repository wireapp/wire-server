{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Wire.SessionStore where

import Cassandra
import Data.Id
import Data.Time.Clock
import Imports
import Polysemy
import Test.QuickCheck
import Wire.API.User.Auth

newtype TTL = TTL {ttlSeconds :: Int32}
  deriving (Show, Eq)
  deriving newtype (Cql, Arbitrary)

data SessionStore m a where
  InsertCookie :: UserId -> Cookie () -> Maybe TTL -> SessionStore m ()
  LookupCookie :: UserId -> UTCTime -> CookieId -> SessionStore m (Maybe (Cookie ()))
  ListCookies :: UserId -> SessionStore m [Cookie ()]
  DeleteAllCookies :: UserId -> SessionStore m ()
  DeleteCookies :: UserId -> [Cookie ()] -> SessionStore m ()

makeSem ''SessionStore

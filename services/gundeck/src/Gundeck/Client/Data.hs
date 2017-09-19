{-# LANGUAGE OverloadedStrings #-}

module Gundeck.Client.Data
    ( select
    , insert
    , remove
    , erase
    ) where

import Cassandra
import Control.Applicative
import Data.Functor.Identity
import Data.Id
import Gundeck.Instances ()
import Gundeck.Types
import Prelude hiding (max)

-- | Note: Consistency: 'One'
select :: MonadClient m => UserId -> ClientId -> m (Maybe SignalingKeys)
select u c = fmap (uncurry SignalingKeys) <$> query1 q (params One (u, c))
  where
    q :: QueryString R (UserId, ClientId) (EncKey, MacKey)
    q = "select enckey, mackey from clients where user = ? and client = ?"

insert :: MonadClient m => UserId -> ClientId -> SignalingKeys -> m ()
insert u c k = retry x5 (write q (params Quorum (u, c, sigEncKey k, sigMacKey k)))
  where
    q :: PrepQuery W (UserId, ClientId, EncKey, MacKey) ()
    q = "insert into clients (user, client, enckey, mackey) values (?, ?, ?, ?)"

remove :: MonadClient m => UserId -> ClientId -> m ()
remove u c = retry x5 (write q (params Quorum (u, c)))
  where
    q :: PrepQuery W (UserId, ClientId) ()
    q = "delete from clients where user = ? and client = ?"

erase :: MonadClient m => UserId -> m ()
erase u = retry x5 (write q (params Quorum (Identity u)))
  where
    q :: PrepQuery W (Identity UserId) ()
    q = "delete from clients where user = ?"

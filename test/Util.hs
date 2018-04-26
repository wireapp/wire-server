{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators   #-}

module Util where


import           API.Group
import           Control.Monad.STM
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Text
import           Data.Time.Clock
import           Data.Time.Calendar
import           DB.User
import           ListT
import           Network.HTTP.Types
import           Network.Wai.Test (SResponse)
import qualified STMContainers.Map as Map
import           STMContainers.Map    (Map)
import           Schema.User
import           Schema.Meta
import           Schema.ListResponse
import qualified Schema.Common     as Common
import           Servant
import           Test.Hspec.Wai    hiding (post, put, patch)

import Prelude hiding (id)

type UserStorage = Map Text StoredUser
type GroupStorage = Map Text StoredGroup

data TestStorage = TestStorage
  { userDB :: UserStorage
  , groupDB :: GroupStorage
  }

-- in-memory implementation of the API for tests
type TestServer = ReaderT TestStorage Handler

liftAtomic :: STM a -> ReaderT TestStorage Handler a
liftAtomic = liftIO . atomically

instance UserDB TestServer where
  list = do
    m <- userDB <$> ask
    l <- liftIO . atomically $ ListT.toList $ Map.stream m
    return $ fromList $ snd <$> l
  get i = do
    m <- userDB <$> ask
    liftIO . atomically $ Map.lookup i m
  create user = do
    m <- userDB <$> ask
    met <- getMeta
    newUser <- liftIO . atomically $ insertUser user met m
    return newUser
  update uid user = do
    storage <- userDB <$> ask
    liftAtomic $ updateUser uid user storage
  delete uid = do
    m <- userDB <$> ask
    liftIO . atomically $ delUser uid m
  getMeta = createMeta UserResource
  patch = undefined

instance GroupDB TestServer where
  list = do
    m <- groupDB <$> ask
    l <- liftIO . atomically $ ListT.toList $ Map.stream m
    return $ snd <$> l
  get i = do
    m <- groupDB <$> ask
    liftIO . atomically $ Map.lookup i m 
  create = \grp -> do
    storage <- groupDB <$> ask
    met <- getGroupMeta
    newGroup <- liftIO . atomically $ insertGroup grp met storage
    pure newGroup
  update i g = do
    m <- groupDB <$> ask
    liftAtomic $ updateGroup i g m
  delete gid = do
    m <- groupDB <$> ask
    liftIO . atomically $ delGroup gid m 
  getGroupMeta = createMeta GroupResource

insertGroup :: Group -> Meta -> GroupStorage -> STM StoredGroup
insertGroup grp met storage = do
  size <- Map.size storage
  let gid = pack . show $ size
      newGroup = WithMeta met $ WithId gid grp
  Map.insert newGroup gid storage
  return newGroup

updateGroup :: GroupId -> Group -> GroupStorage  -> STM (Either ServantErr StoredGroup)
updateGroup gid grp storage = do
  existing <- Map.lookup gid storage
  case existing of
    Nothing -> pure $ Left err400
    Just stored -> do
      let newMeta = meta stored
          newGroup = WithMeta newMeta $ WithId gid grp
      Map.insert newGroup gid storage
      pure $ Right newGroup

updateUser :: UserId -> User -> UserStorage  -> STM (Either UpdateError StoredUser)
updateUser uid user storage = do
  existing <- Map.lookup uid storage
  case existing of
    Nothing -> pure $ Left NonExisting
    Just stored -> do
      let newMeta = meta stored
          newUser = WithMeta newMeta $ WithId uid user
      Map.insert newUser uid storage
      pure $ Right newUser

-- TODO: what needs checking here?
-- (there seems to be no readOnly fields in User)
assertMutability :: User -> StoredUser -> Bool
assertMutability _newUser _stored = True

delGroup :: GroupId -> GroupStorage -> STM Bool
delGroup gid storage = do
  g <- Map.lookup gid storage
  case g of
    Nothing -> return False
    Just _ -> Map.delete gid storage >> return True

delUser :: UserId -> UserStorage -> STM Bool
delUser uid storage = do
  u <- Map.lookup uid storage
  case u of
    Nothing -> return False
    Just _ -> Map.delete uid storage >> return True

-- 2018-01-01 00:00
testDate :: UTCTime
testDate = UTCTime
  { utctDay = ModifiedJulianDay 58119
  , utctDayTime = 0
  }

-- static meta for testing
createMeta :: ResourceType -> ReaderT TestStorage Handler Meta
createMeta rType = return $ Meta
  { resourceType = rType
  , created = testDate
  , lastModified = testDate
  , version = Weak "testVersion"
  , location = Common.URI $ URI "todo" Nothing "" "" ""
  }

-- insert with a simple incrementing integer id (good for testing)
insertUser :: User -> Meta -> UserStorage -> STM StoredUser
insertUser user met storage = do
  size <- Map.size storage
  let uid = pack . show $ size
      newUser = WithMeta met $ WithId uid user
  Map.insert newUser uid storage
  return newUser

-- Natural transformation from our transformer stack to the Servant
-- stack
-- this takes the initial environment and returns the transformation
nt :: r -> ReaderT r m a -> m a
nt = flip runReaderT

-- Redefine wai test helpers to include json content type
post :: ByteString -> L.ByteString -> WaiSession SResponse
post path = request methodPost path [(hContentType, "application/json")]

put :: ByteString -> L.ByteString -> WaiSession SResponse
put path = request methodPut path [(hContentType, "application/json")]

patch :: ByteString -> L.ByteString -> WaiSession SResponse
patch path = request methodPatch path [(hContentType, "application/json")]


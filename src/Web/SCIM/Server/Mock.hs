{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE LambdaCase      #-}

-- | A mock server for use in our testsuite, as well as for automated
-- compliance testing (e.g. with Runscope – see
-- https://developer.okta.com/standards/SCIM/#step-2-test-your-scim-server).

module Web.SCIM.Server.Mock where

import           Web.SCIM.Class.Group
import           Web.SCIM.Class.User
import           Web.SCIM.Class.Auth
import           Control.Monad.STM
import           Control.Monad.Reader
import           Data.Text
import           Data.Text.Encoding
import           Data.Time.Clock
import           Data.Time.Calendar
import           ListT
import qualified STMContainers.Map as STMMap
import           Web.SCIM.Schema.User
import           Web.SCIM.Schema.Meta
import           Web.SCIM.Schema.ListResponse
import           Web.SCIM.Schema.Common (WithId(WithId))
import qualified Web.SCIM.Schema.Common     as Common
import           Servant hiding (BadPassword, NoSuchUser)
import           Servant.Auth.Server

import Prelude hiding (id)

type UserStorage  = STMMap.Map Text StoredUser
type GroupStorage = STMMap.Map Text StoredGroup
type AdminStorage = STMMap.Map Text (Admin, Text) -- (Admin, Pass)

data TestStorage = TestStorage
  { userDB :: UserStorage
  , groupDB :: GroupStorage
  , authDB :: AdminStorage
  }

-- in-memory implementation of the API for tests
type TestServer = ReaderT TestStorage Handler

liftAtomic :: STM a -> ReaderT TestStorage Handler a
liftAtomic = liftIO . atomically

instance UserDB TestServer where
  list = do
    m <- userDB <$> ask
    l <- liftIO . atomically $ ListT.toList $ STMMap.stream m
    return $ fromList $ snd <$> l
  get i = do
    m <- userDB <$> ask
    liftIO . atomically $ STMMap.lookup i m
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
    l <- liftIO . atomically $ ListT.toList $ STMMap.stream m
    return $ snd <$> l
  get i = do
    m <- groupDB <$> ask
    liftIO . atomically $ STMMap.lookup i m
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

instance AuthDB TestServer where
  mkAuthChecker = do
    m <- authDB <$> ask
    pure $ \(BasicAuthData login pass) ->
      atomically (STMMap.lookup (decodeUtf8 login) m) >>= \case
        Just (admin, adminPass)
          | decodeUtf8 pass == adminPass -> pure (Authenticated admin)
          | otherwise -> pure BadPassword
        Nothing -> pure NoSuchUser

insertGroup :: Group -> Meta -> GroupStorage -> STM StoredGroup
insertGroup grp met storage = do
  size <- STMMap.size storage
  let gid = pack . show $ size
      newGroup = WithMeta met $ WithId gid grp
  STMMap.insert newGroup gid storage
  return newGroup

updateGroup :: GroupId -> Group -> GroupStorage  -> STM (Either ServantErr StoredGroup)
updateGroup gid grp storage = do
  existing <- STMMap.lookup gid storage
  case existing of
    Nothing -> pure $ Left err400
    Just stored -> do
      let newMeta = meta stored
          newGroup = WithMeta newMeta $ WithId gid grp
      STMMap.insert newGroup gid storage
      pure $ Right newGroup

updateUser :: UserId -> User -> UserStorage  -> STM (Either UpdateError StoredUser)
updateUser uid user storage = do
  existing <- STMMap.lookup uid storage
  case existing of
    Nothing -> pure $ Left NonExisting
    Just stored -> do
      let newMeta = meta stored
          newUser = WithMeta newMeta $ WithId uid user
      STMMap.insert newUser uid storage
      pure $ Right newUser

-- (there seems to be no readOnly fields in User)
assertMutability :: User -> StoredUser -> Bool
assertMutability _newUser _stored = True

delGroup :: GroupId -> GroupStorage -> STM Bool
delGroup gid storage = do
  g <- STMMap.lookup gid storage
  case g of
    Nothing -> return False
    Just _ -> STMMap.delete gid storage >> return True

delUser :: UserId -> UserStorage -> STM Bool
delUser uid storage = do
  u <- STMMap.lookup uid storage
  case u of
    Nothing -> return False
    Just _ -> STMMap.delete uid storage >> return True

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
  size <- STMMap.size storage
  let uid = pack . show $ size
      newUser = WithMeta met $ WithId uid user
  STMMap.insert newUser uid storage
  return newUser

-- Natural transformation from our transformer stack to the Servant
-- stack
-- this takes the initial environment and returns the transformation
nt :: r -> ReaderT r m a -> m a
nt = flip runReaderT

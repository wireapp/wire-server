{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

-- | A mock server for use in our testsuite, as well as for automated
-- compliance testing (e.g. with Runscope – see
-- https://developer.okta.com/standards/SCIM/#step-2-test-your-scim-server).

module Web.SCIM.Server.Mock where

import           Web.SCIM.Class.Group hiding (value)
import           Web.SCIM.Class.User
import           Web.SCIM.Class.Auth
import           Control.Monad.STM (STM, atomically)
import           Control.Monad.Reader
import           Control.Monad.Catch (throwM)
import           Data.Maybe
import           Data.Text (Text, pack)
import           Data.Text.Encoding
import           Data.Time.Clock
import           Data.Time.Calendar
import           GHC.Exts (sortWith)
import           ListT
import qualified STMContainers.Map as STMMap
import           Web.SCIM.Schema.User
import           Web.SCIM.Schema.Error
import           Web.SCIM.Schema.Meta
import           Web.SCIM.Schema.ListResponse
import           Web.SCIM.Schema.ResourceType
import           Web.SCIM.Schema.Common (WithId(WithId, value))
import qualified Web.SCIM.Schema.Common     as Common
import           Web.SCIM.Filter
import           Servant hiding (BadPassword, NoSuchUser)
import           Servant.Auth.Server
import           Data.UUID as UUID

import Prelude hiding (id)

type UserStorage  = STMMap.Map Text StoredUser
type GroupStorage = STMMap.Map Text StoredGroup
type AdminStorage = STMMap.Map UUID (Admin, Text) -- (Admin, Pass)

data TestStorage = TestStorage
  { userDB :: UserStorage
  , groupDB :: GroupStorage
  , authDB :: AdminStorage
  }

-- in-memory implementation of the API for tests
type TestServer = ReaderT TestStorage IO

liftSTM :: MonadIO m => STM a -> m a
liftSTM = liftIO . atomically

instance UserDB TestServer where
  list mbFilter = do
    -- Note: in production instances it would make sense to remove this code
    -- and let the implementor of the 'UserDB' instance do filtering (e.g.
    -- doing case-insensitive queries on common attributes can be done
    -- faster if the underlying database has indices). However, it might
    -- still be useful to provide a default implementation in @hscim@ and
    -- let users of the library decide whether they want to use it or not.
    m <- userDB <$> ask
    users <- liftSTM $ ListT.toList $ STMMap.stream m
    let check user = case mbFilter of
          Nothing -> pure True
          Just filter_ -> do
            let user' = value (thing user)      -- unwrap
            case filterUser filter_ user' of
              Right res -> pure res
              Left err  -> throwM (badRequest InvalidFilter (Just err))
    fromList . sortWith (Common.id . thing) <$>
      filterM check (snd <$> users)
  get i = do
    m <- userDB <$> ask
    liftSTM $ STMMap.lookup i m
  create user = do
    m <- userDB <$> ask
    met <- getMeta
    newUser <- liftSTM $ insertUser user met m
    return newUser
  update uid user = do
    storage <- userDB <$> ask
    liftSTM $ updateUser uid user storage
  delete uid = do
    m <- userDB <$> ask
    liftSTM $ delUser uid m
  getMeta = return (createMeta UserResource)
  patch = error "PATCH /Users: not implemented"

instance GroupDB TestServer where
  list = do
    m <- groupDB <$> ask
    groups <- liftSTM $ ListT.toList $ STMMap.stream m
    return $ sortWith (Common.id . thing) $ snd <$> groups
  get i = do
    m <- groupDB <$> ask
    liftSTM $ STMMap.lookup i m
  create = \grp -> do
    storage <- groupDB <$> ask
    met <- getGroupMeta
    newGroup <- liftSTM $ insertGroup grp met storage
    pure newGroup
  update i g = do
    m <- groupDB <$> ask
    liftSTM $ updateGroup i g m
  delete gid = do
    m <- groupDB <$> ask
    liftSTM $ delGroup gid m
  getGroupMeta = return (createMeta GroupResource)

instance AuthDB TestServer where
  mkAuthChecker = do
    m <- authDB <$> ask
    pure $ \(BasicAuthData login pass) ->
      case UUID.fromASCIIBytes login of
        Nothing -> pure NoSuchUser
        Just loginUuid -> atomically (STMMap.lookup loginUuid m) >>= \case
          Just (admin, adminPass)
            | decodeUtf8 pass == adminPass -> pure (Authenticated admin)
            -- Note: somebody can figure out if whether the given user
            -- exists, but since the admin users are represented with UUIDs
            -- (which are pretty hard to bruteforce), it's okay. It also
            -- makes debugging easier.
            | otherwise -> pure BadPassword
          Nothing -> pure NoSuchUser

insertGroup :: Group -> Meta -> GroupStorage -> STM StoredGroup
insertGroup grp met storage = do
  size <- STMMap.size storage
  let gid = pack . show $ size
      newGroup = WithMeta met $ WithId gid grp
  STMMap.insert newGroup gid storage
  return newGroup

updateGroup :: GroupId -> Group -> GroupStorage -> STM StoredGroup
updateGroup gid grp storage = do
  existing <- STMMap.lookup gid storage
  case existing of
    Nothing -> throwM (notFound "Group" gid)
    Just stored -> do
      let newMeta = meta stored
          newGroup = WithMeta newMeta $ WithId gid grp
      STMMap.insert newGroup gid storage
      pure newGroup

updateUser :: UserId -> User -> UserStorage -> STM StoredUser
updateUser uid user storage = do
  existing <- STMMap.lookup uid storage
  case existing of
    Nothing -> throwM (notFound "User" uid)
    Just stored -> do
      let newMeta = meta stored
          newUser = WithMeta newMeta $ WithId uid user
      STMMap.insert newUser uid storage
      pure newUser

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
createMeta :: ResourceType -> Meta
createMeta rType = Meta
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

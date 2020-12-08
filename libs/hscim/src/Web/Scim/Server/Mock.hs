{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- | A mock server for use in our testsuite, as well as for automated
-- compliance testing (e.g. with Runscope – see
-- <https://developer.okta.com/standards/SCIM/#step-2-test-your-scim-server>).
module Web.Scim.Server.Mock where

import Control.Monad.Except (ExceptT (ExceptT), MonadError (..), runExceptT)
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.STM (STM, atomically)
import Data.Aeson
import Data.Hashable
import Data.Text (Text, pack, toCaseFold)
import Data.Time.Calendar
import Data.Time.Clock
import GHC.Exts (sortWith)
import ListT
import qualified Network.URI as URI
import Servant
import qualified StmContainers.Map as STMMap
import Text.Read (readMaybe)
import Web.Scim.Class.Auth
import Web.Scim.Class.Group hiding (value)
import Web.Scim.Class.User
import Web.Scim.Filter (AttrPath (..), CompValue (..), Filter (..), compareStr)
import Web.Scim.Handler
import Web.Scim.Schema.Common (WithId (WithId, value))
import qualified Web.Scim.Schema.Common as Common
import Web.Scim.Schema.Error
import Web.Scim.Schema.ListResponse
import Web.Scim.Schema.Meta
import Web.Scim.Schema.ResourceType
import Web.Scim.Schema.Schema (Schema (User20))
import Web.Scim.Schema.User

-- | Tag used in the mock server.
data Mock

-- | A simple ID type.
--
-- >>> eitherDecode' @Id . encode $ (Id 3)
-- Right (Id {unId = 3})
--
-- WARNING: {doctests don't work in our
-- infrastructure](https://github.com/zinfra/backend-issues/issues/1549), so this is
-- duplicated in the unit tests.
newtype Id = Id {unId :: Int}
  deriving (Eq, Show, Ord, Hashable, ToHttpApiData, FromHttpApiData)

instance ToJSON Id where
  toJSON = toJSON . show . unId

instance FromJSON Id where
  parseJSON = maybe (fail "not a number") (pure . Id) . readMaybe <=< parseJSON

type UserStorage = STMMap.Map Id (StoredUser Mock)

type GroupStorage = STMMap.Map Id (StoredGroup Mock)

data TestStorage = TestStorage
  { userDB :: UserStorage,
    groupDB :: GroupStorage
  }

emptyTestStorage :: IO TestStorage
emptyTestStorage =
  TestStorage <$> STMMap.newIO <*> STMMap.newIO

-- in-memory implementation of the API for tests
type TestServer = ReaderT TestStorage Handler

liftSTM :: MonadIO m => STM a -> m a
liftSTM = liftIO . atomically

hoistSTM :: (MFunctor t, MonadIO m) => t STM a -> t m a
hoistSTM = hoist liftSTM

----------------------------------------------------------------------------

newtype UVerbT xs m a = UVerbT {unUVerbT :: ExceptT (Union xs) m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

-- | Deliberately hide 'ExceptT's 'MonadError' instance to be able to use
-- underlying monad's instance.
instance MonadError e m => MonadError e (UVerbT xs m) where
  throwError = lift . throwError
  catchError (UVerbT act) h =
    UVerbT $
      ExceptT $
        runExceptT act `catchError` (runExceptT . unUVerbT . h)

-- | This combinator runs 'UVerbT'. It applies 'respond' internally, so the handler
-- may use the usual 'return'.
runUVerbT :: (Monad m, HasStatus x, IsMember x xs) => UVerbT xs m x -> m (Union xs)
runUVerbT (UVerbT act) = either id id <$> runExceptT (act >>= respond)

-- | Short-circuit 'UVerbT' computation returning one of the response types.
throwUVerb :: (Monad m, HasStatus x, IsMember x xs) => x -> UVerbT xs m a
throwUVerb = UVerbT . ExceptT . fmap Left . respond

----------------------------------------------------------------------------
-- UserDB

instance UserTypes Mock where
  type UserId Mock = Id
  type UserExtra Mock = NoUserExtra
  supportedSchemas = [User20]

  getUsers () mbFilter = do
    m <- userDB <$> ask
    users <- liftSTM $ ListT.toList $ STMMap.listT m
    let check user = case mbFilter of
          Nothing -> pure True
          Just filter_ -> do
            let user' = value (thing user) -- unwrap
            case filterUser filter_ user' of
              Right res -> pure res
              Left err -> throwScim (badRequest InvalidFilter (Just err))
    fromList . sortWith (Common.id . thing) <$> filterM check (snd <$> users)
  getUsers () mbFilter = runUVerbT $
    do
      m <- userDB <$> lift ask
      users <- lift $ liftSTM $ ListT.toList $ STMMap.listT m
      let check user = case mbFilter of
            Nothing -> pure True
            Just filter_ -> do
              let user' = value (thing user) -- unwrap
              case filterUser filter_ user' of
                Right res -> pure res
                Left err -> throwUVerb (BadRequest InvalidFilter (Just err))
      WithStatus @200 . fromList . sortWith (Common.id . thing) <$> filterM check (snd <$> users)

  -- getUser () uid = do
  --   m <- userDB <$> ask
  --   liftSTM (STMMap.lookup uid m) >>= \case
  --     Nothing -> throwScim (notFound "User" (pack (show uid)))
  --     Just x -> pure x

  getUser () uid = runUVerbT $ do
    m <- userDB <$> lift ask
    lift (liftSTM (STMMap.lookup uid m)) >>= \case
      Nothing -> throwUVerb (NotFound "User" (pack (show uid)))
      Just (x :: (StoredUser Mock)) -> respond (WithStatus @200 x)

  postUser () user = do
    m <- userDB <$> ask
    uid <- Id <$> liftSTM (STMMap.size m)
    let newUser = WithMeta (createMeta UserResource) $ WithId uid user
    liftSTM $ STMMap.insert newUser uid m
    return newUser

  putUser () uid user = do
    m <- userDB <$> ask
    liftSTM (STMMap.lookup uid m) >>= \case
      Nothing -> throwScim (notFound "User" (pack (show uid)))
      Just stored -> do
        let newUser = WithMeta (meta stored) $ WithId uid user
        liftSTM $ STMMap.insert newUser uid m
        pure newUser

  deleteUser () uid = do
    m <- userDB <$> ask
    liftSTM (STMMap.lookup uid m) >>= \case
      Nothing -> throwScim (notFound "User" (pack (show uid)))
      Just _ -> liftSTM $ STMMap.delete uid m

-- (there seems to be no readOnly fields in User)
assertMutability :: User Mock -> StoredUser Mock -> Bool
assertMutability _newUser _stored = True

----------------------------------------------------------------------------
-- GroupDB

instance GroupTypes Mock where
  type GroupId Mock = Id

instance GroupDB Mock TestServer where
  getGroups () = do
    m <- groupDB <$> ask
    groups <- liftSTM $ ListT.toList $ STMMap.listT m
    return $ fromList . sortWith (Common.id . thing) $ snd <$> groups

  getGroup () gid = do
    m <- groupDB <$> ask
    liftSTM (STMMap.lookup gid m) >>= \case
      Nothing -> throwScim (notFound "Group" (pack (show gid)))
      Just grp -> pure grp

  postGroup () grp = do
    m <- groupDB <$> ask
    gid <- Id <$> liftSTM (STMMap.size m)
    let newGroup = WithMeta (createMeta GroupResource) $ WithId gid grp
    liftSTM $ STMMap.insert newGroup gid m
    return newGroup

  putGroup () gid grp = do
    m <- groupDB <$> ask
    liftSTM (STMMap.lookup gid m) >>= \case
      Nothing -> throwScim (notFound "Group" (pack (show gid)))
      Just stored -> do
        let newGroup = WithMeta (meta stored) $ WithId gid grp
        liftSTM $ STMMap.insert newGroup gid m
        pure newGroup

  patchGroup _ _ _ = throwScim (serverError "PATCH /Users not implemented")

  deleteGroup () gid = do
    m <- groupDB <$> ask
    liftSTM (STMMap.lookup gid m) >>= \case
      Nothing -> throwScim (notFound "Group" (pack (show gid)))
      Just _ -> liftSTM $ STMMap.delete gid m

----------------------------------------------------------------------------
-- AuthDB

instance AuthTypes Mock where
  type AuthData Mock = Text
  type AuthInfo Mock = ()

instance AuthDB Mock TestServer where
  authCheck = \case
    Just "authorized" -> pure ()
    _ -> throwScim (unauthorized "expected 'authorized'")

----------------------------------------------------------------------------
-- Misc

-- 2018-01-01 00:00
testDate :: UTCTime
testDate =
  UTCTime
    { utctDay = ModifiedJulianDay 58119,
      utctDayTime = 0
    }

-- static meta for testing
createMeta :: ResourceType -> Meta
createMeta rType =
  Meta
    { resourceType = rType,
      created = testDate,
      lastModified = testDate,
      version = Weak "testVersion",
      location =
        Common.URI $ -- FUTUREWORK: getting the actual schema, authority, and path here
        -- is a bit of work, but it may be required one day.
          URI "https:" (Just $ URI.URIAuth "" "example.com" "") "/Users/id" "" ""
    }

-- Natural transformation from our transformer stack to the Servant stack
-- this takes the initial environment and returns the transformation
nt :: TestStorage -> ScimHandler TestServer a -> Handler a
nt storage =
  flip runReaderT storage
    . fromScimHandler (lift . throwError . scimToServerError)

-- | Check whether a user satisfies the filter.
--
-- Returns 'Left' if the filter is constructed incorrectly (e.g. tries to
-- compare a username with a boolean).
--
-- TODO(arianvp): We need to generalise filtering at some point probably.
filterUser :: Filter -> User extra -> Either Text Bool
filterUser (FilterAttrCompare (AttrPath schema' attrib subAttr) op val) user
  | isUserSchema schema' =
    case (subAttr, val) of
      (Nothing, (ValString str))
        | attrib == "userName" ->
          Right (compareStr op (toCaseFold (userName user)) (toCaseFold str))
      (Nothing, _)
        | attrib == "userName" ->
          Left "usernames can only be compared with strings"
      (_, _) ->
        Left "Only search on usernames is currently supported"
  | otherwise = Left "Invalid schema. Only user schema is supported"

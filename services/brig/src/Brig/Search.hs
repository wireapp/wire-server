{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wwarn #-}

module Brig.Search where

import Brig.User.Search.Index
import Data.Domain
import Data.Id hiding (UserId)
import Data.Range
import Debug.Trace
import Imports
import Polysemy
import Polysemy.Internal
import Polysemy.Internal.Union
import Wire.API.User.Search

type UserId = String

-- Stores -> Wrappers over DB
-- Access -> Wrappers over HTTP
--
-- All business logic uses subset of (stores, access)
--
--
-- User -> Client
-- Client -> User
-- User1 -> Client -> User2

-- User, Client, Conversation, Search, Notification, Team, Connection, Federation, Asset
--
-- deleteUser ->
--    [Client] deleteAllClients ->
--    [Connection] disconnectWithEveryone -> notifications
--    [Conversation] leaveAllConvs -> notification
--    [Team] leaveTheTeam
--    [Search] unindex for search

data Search m a where
  Search ::
    UserId ->
    Text ->
    Maybe Domain ->
    Maybe (Range 1 500 Int32) ->
    Search m (SearchResult Contact)
  SearchTeamUser ::
    UserId ->
    TeamId ->
    Maybe Text ->
    Maybe RoleFilter ->
    Maybe TeamUserSearchSortBy ->
    Maybe TeamUserSearchSortOrder ->
    Maybe (Range 1 500 Int32) ->
    Maybe PagingState ->
    Search m (SearchResult TeamContact)
  RefreshIndex :: Search m ()
  ReindexUser :: IndexUser -> Search m ()

makeSem ''Search

data User m a where
  -- GetIndexUser :: UserId -> User m IndexUser
  UserFoo :: String -> User m ()
  UserBar :: String -> User m ()

makeSem ''User

-- runSearch :: Member User r => Sem (Search ': r) a -> Sem r a
-- runSearch = interpret $ \case
--   Search ->

-- Serach -> User
-- User -> Search
-- Client -> User
-- User -> Client

data Client m a where
  ClientFoo :: String -> Client m ()
  ClientBar :: String -> Client m ()

makeSem ''Client

-- interpretSearch :: Member User r => Sem (Search ': r) a -> Sem r a
-- interpretUser :: (Member Search r, Member Client r) => Sem (User ': r) a -> Sem r a
-- interpretClientFoo :: Member User r => Sem (Client ': r) a -> Sem r a
-- interpretClientFoo = interpret

-- interpretClientBar :: Sem (Client ': r) a -> Sem r a

-- interpretClientFoo = undefined

-- Client User

interpretClient :: Member User r => Sem (Client ': r) a -> Sem r a
interpretClient = interpret $ \case
  ClientFoo _ -> do
    traceM "ClientFoo"
    pure ()
  ClientBar uid -> do
    traceM "ClientBar"
    userBar uid

interpretUser :: (Member Client r) => Sem (User ': r) a -> Sem r a
interpretUser = interpret $ \case
  UserFoo uid -> do
    traceM "UserFoo"
    clientFoo uid
  UserBar uid -> do
    traceM "UserBar"
    clientFoo uid

-- >>> run $ interpretClientUser $ userFoo "fasdf"
interpretClientUser :: Sem (Client ': User ': r) a -> Sem r a
interpretClientUser action =
  loopIfRequired $
    -- Client
    interpretUser $
      -- User Client
      raiseUnder $
        -- User
        interpretClient action

loopIfRequired :: Sem (Client ': User ': r) a -> Sem r a
loopIfRequired = undefined

foo ::
  Sem (Client ': r) a ->
  Sem r a
foo action@(Sem m) = Sem $ \k -> m $ \u ->
  case decomp u of
    Left x -> k $ hoist (interpretH f) x
    Right _ -> interpretClientUser $ raiseUnder action

data Env = Env
  { envUserActions :: UserActions,
    envClientActions :: ClientActions
  }

type App = ReaderT Env IO

data UserActions = UserActions
  { _userActionFoo :: UserId -> IO (),
    _userActionBar :: UserId -> IO ()
  }

data ClientActions = ClientActions
  { _clientActionFoo :: UserId -> IO (),
    _clientActionBar :: UserId -> IO ()
  }

clientActionFoo :: UserId -> App ()
clientActionFoo uid = do
  f <- asks (_clientActionFoo . envClientActions)
  liftIO $ f uid

clientActionBar :: UserId -> App ()
clientActionBar uid = do
  f <- asks (_clientActionBar . envClientActions)
  liftIO $ f uid

userActionFoo :: UserId -> App ()
userActionFoo uid = do
  f <- asks (_userActionFoo . envUserActions)
  liftIO $ f uid

userActionFooImpl :: UserId -> App ()
userActionFooImpl uid =
  putStrLn uid

userActionBar :: UserId -> App ()
userActionBar uid = do
  f <- asks (_userActionBar . envUserActions)
  liftIO $ f uid

defUserActions :: ClientActions -> UserActions
defUserActions e =
  UserActions
    { _userActionFoo = \uid -> runApp e $ clientActionFoo uid,
      _userActionBar = \uid -> runApp e $ clientActionFoo uid
    }

defClientActions :: Env -> ClientActions
defClientActions e =
  ClientActions
    { _clientActionFoo = \uid -> runApp e $ pure (),
      _clientActionBar = \uid -> runApp e (userActionFoo uid)
    }

defEnv :: Env
defEnv =
  Env
    { envUserActions = defUserActions defEnv,
      envClientActions = defClientActions defEnv
    }

runApp :: b -> ReaderT b m a -> m a
runApp = flip runReaderT

test = runApp defEnv (clientActionBar "foo")

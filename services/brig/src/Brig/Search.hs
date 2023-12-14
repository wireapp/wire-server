{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wwarn #-}

module Brig.Search where

import Data.Domain
import Data.Id hiding (UserId)
import Data.Range
import Debug.Trace
import Imports
import Polysemy
import Wire.API.User (UserProfile)
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
--
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
  ReindexUser :: UserId -> Search m ()
  RefreshIndex :: Search m ()

makeSem ''Search

data User m a where
  -- GetIndexUser :: UserId -> User m IndexUser
  UserFoo :: String -> User m ()
  UserBar :: String -> User m ()
  GetUserById :: UserId -> User m UserProfile

makeSem ''User

esReindex :: UserProfile -> IO ()
esReindex = undefined

runSearch :: (Member User r, Member (Embed IO) r) => Sem (Search ': r) a -> Sem r a
runSearch = interpret $ \case
  ReindexUser uid -> embed . esReindex =<< getUserById uid
  _ -> undefined

runUser :: Member Search r => Sem (User ': r) a -> Sem r a
runUser = interpret $ \case
  UserFoo uid -> reindexUser uid
  _ -> undefined

-- runSearchAndUser ::

-- runBrig :: Sem (User ': Search ': r) a -> Sem r a
-- runBrig = runUser . runSearch

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

interpretClient :: InterpreterFor User r -> Sem (Client ': r) a -> Sem r a
interpretClient iu = interpret $ \case
  ClientFoo _ -> do
    traceM "ClientFoo"
    pure ()
  ClientBar uid -> do
    traceM "ClientBar"
    iu $ userBar uid

-- data InterpretterStack =

interpretUser :: InterpreterFor Client r -> Sem (User ': r) a -> Sem r a
interpretUser ic = interpret $ \case
  UserFoo uid -> do
    traceM "UserFoo"
    ic $ clientFoo uid
  UserBar uid -> do
    traceM "UserBar"
    ic $ clientFoo uid

userBarImpl :: Member (Client) r => UserId -> Sem r ()
userBarImpl = clientFoo

-- interpretCientUser' :: (forall r'. Member Client r' => Sem (User ': r') a -> Sem r' a) -> Sem (Client : r) a -> Sem r a
-- interpretCientUser' iu = interpret $ \case
--   Client

-- >>> run $ interpretClientUser $ userFoo "fasdf"
-- interpretClientUser :: Sem (Client ': User ': r) a -> Sem r a
-- interpretClientUser action =
--   interpretClientUser $
--     raiseUnder $
--       -- Client
--       interpretUser $
--         -- User, Client
--         raiseUnder $
--           -- User
--           interpretClient action

prodInterpretUser :: Sem (User ': r) a -> Sem r a
prodInterpretUser = interpretUser prodInterpretClient

prodInterpretClient :: Sem (Client ': r) a -> Sem r a
prodInterpretClient = interpretClient prodInterpretUser

runStack :: Sem '[Client, User, Embed IO] a -> IO a
runStack =
  runM . prodInterpretUser . prodInterpretClient

test :: IO ()
test = runStack $ do
  clientBar "fasdf"
  userFoo "foo"

loopIfRequired :: Sem (Client ': r) a -> Sem r a
loopIfRequired = undefined

-- foo ::
--   -- | A natural transformation from the handled effect to other effects
--   -- already in 'Sem'.
--   (forall rInitial x. e (Sem rInitial) x -> Tactical e (Sem rInitial) r x) ->
--   Sem (e ': r) a ->
--   Sem r a
-- foo action@(Sem m) = Sem $ \k -> m $ \u ->
--   case decomp u of
--     Left x -> k $ hoist (interpretH f) x
--     Right _ -> interpretClientUser $ raiseUnder action

-- foo :: Sem (Client ': r) a -> Sem r a
-- foo action@(Sem m) = Sem $ \k -> m $ \u ->
--   case decomp u of
--     -- There is no Client constructors left
--     Left _ -> undefined
--     -- There are Client constructors
--     Right _ -> _

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

defUserActions :: Env -> UserActions
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

data Free f a = Pure a | Free (f (Free f a))
  deriving (Functor)

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance Functor f => Monad (Free f) where
  return = pure
  Pure a >>= f = f a
  Free m >>= f = Free $ fmap (>>= f) m

-- instance Functor (Free f)

data F a = F1 a | F2 a
  deriving (Functor)

f1 :: Free F ()
f1 = Free $ F1 (Pure ())

f2 :: Free F ()
f2 = Free $ F2 $ Pure ()

data G a = G1 a
  deriving (Functor)

g1 :: Free G ()
g1 = Free $ G1 (Pure ())

intF :: F a -> Free G a
-- intF (F1 a) = Free $ fmap (\_ -> Pure a) (G1 (Pure ()))
intF (F1 a) = g1 >> pure a
intF (F2 a) = Pure a

intG :: G a -> Free F a
intG (G1 a) = f2 >> pure a

-- intG (G1 a) = Free $ fmap (\_ -> Pure a) (F2 (Pure ()))

runF :: Free F a -> a
runF (Pure x) = x
runF (Free m) = runF $ runG (intF m)

runG :: Free G a -> a
runG (Pure x) = x
runG (Free m) = runG $ runF (intG m)

example :: ()
example = runF f1

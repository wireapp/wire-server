{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Network.Wire.Bot.Monad
    ( -- * Environment
      BotNetEnv
    , newBotNetEnv
    , getBotSettings
    , localBotSettings
    , getGen

      -- * BotNet
    , BotNet
    , MonadBotNet
    , runBotNet

      -- * BotSession
    , BotSession
    , runBotSession
    , getBot

      -- * Bot
    , Bot (botUser)
    , botId
    , botName
    , botEmail
    , botSettings
    , newBot
    , cachedBot
    , drainBot
    , killBot
    , withNewBot
    , withCachedBot

      -- * BotClient
    , BotClient (..)
    , getBotClients
    , addBotClient
    , removeBotClient

      -- * Assertions
    , assertEvent
    , awaitEvent
    , assertTrue
    , assertEqual
    , assertFailure
    , awaitAssertions
    , require
    , requireMaybe
    , requireRight

      -- * Reports & Metrics
    , report
    , getMetrics
    , timed
    , module Metrics

      -- * Exceptions & Recovery
    , BotNetException (..)
    , BotNetFailure   (..)
    , try
    ) where

import Bilge (MonadHttp (..), Manager)
import Control.Applicative
import Control.Concurrent (threadDelay, myThreadId, forkIO)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (forever, when, unless, void, (>=>))
import Control.Monad.Base
import Control.Monad.Catch hiding (try)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Data.Foldable (for_, forM_, traverse_, sum)
import Data.HashMap.Strict (HashMap)
import Data.Id
import Data.IORef
import Data.List (foldl', partition)
import Data.Maybe (fromMaybe, isNothing)
import Data.Misc
import Data.Metrics (Metrics)
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Text (Text, pack, unpack)
import Data.Time.Clock
import Data.Time.Format (parseTimeOrError, formatTime, defaultTimeLocale)
import Data.Typeable
import Data.UUID (toString)
import Data.UUID.V4
import Data.Word (Word16)
import Network.HTTP.Client (HttpException)
import Network.Wire.Bot.Cache (Cache, CachedUser (..))
import Network.Wire.Bot.Clients as Clients
import Network.Wire.Bot.Crypto.Glue
import Network.Wire.Bot.Email
import Network.Wire.Bot.Report
import Network.Wire.Bot.Report.Text
import Network.Wire.Bot.Settings
import Network.Wire.Client
import Network.Wire.Client.API.Auth
import Network.Wire.Client.API.Client
import Network.Wire.Client.API.Push
import Network.Wire.Client.API.User
import Prelude hiding (log, rem, sum)
import System.CryptoBox (Box)
import System.Logger.Class hiding (new)
import System.FilePath.Posix ((</>))

import qualified Data.HashMap.Strict      as HashMap
import qualified Data.Metrics             as Metrics
import qualified Network.Wire.Bot.Cache   as Cache
import qualified Network.Wire.Bot.Metrics as Metrics
import qualified System.Logger            as Logger
import qualified System.Random.MWC        as MWC

-------------------------------------------------------------------------------
-- * BotNetEnv

data BotNetEnv = BotNetEnv
    { botNetGen       :: MWC.GenIO
    , botNetMailboxes :: [Mailbox]
    , botNetSender    :: Email
    , botNetUsers     :: Cache
    , botNetServer    :: Server
    , botNetLogger    :: Logger
    , botNetAssert    :: !Bool
    , botNetSettings  :: BotSettings
    , botNetMetrics   :: Metrics
    , botNetReportDir :: Maybe FilePath
    }

newBotNetEnv :: Manager -> Logger -> BotNetSettings -> IO BotNetEnv
newBotNetEnv m l o = do
    gen <- MWC.createSystemRandom
    usr <- maybe Cache.empty (Cache.new l gen) (setBotNetUsersFile o)
    mbx <- maybe (return []) loadMailboxConfig (setBotNetMailboxConfig o)
    met <- initMetrics
    let sdr = setBotNetSender o
    let srv = Server
            { serverHost    = setBotNetApiHost   o
            , serverPort    = setBotNetApiPort   o
            , serverWsHost  = setBotNetApiWsHost o
            , serverWsPort  = setBotNetApiWsPort o
            , serverSSL     = setBotNetApiSSL    o
            , serverManager = m
            }
    let asrt = setBotNetAssert o
    let sets = setBotNetBotSettings o
    let rprt = setBotNetReportDir o
    return $! BotNetEnv gen mbx sdr usr srv l asrt sets met rprt

-- Note: Initializing metrics to avoid race conditions on first access and thus
-- potentially losing some values.
initMetrics :: IO Metrics
initMetrics = do
    m <- Metrics.metrics
    forM_ counters $ \c -> Metrics.counterGet c m
    forM_ gauges   $ \g -> Metrics.gaugeGet g m
    return m
  where
    counters = Metrics.assertionsTotal
             : Metrics.assertionsFailed
             : Metrics.exceptionsTotal
             : Metrics.botsCreatedNew
             : Metrics.botsCreatedCached
             : Metrics.eventsTotalRcvd
             : Metrics.eventsTotalAckd
             : Metrics.eventsTotalIgnd
             : Metrics.eventsTotalMssd
             : concatMap etc [(minBound :: EventType)..]

    etc t = [ Metrics.eventTypeRcvd t
            , Metrics.eventTypeAckd t
            , Metrics.eventTypeIgnd t
            , Metrics.eventTypeMssd t
            ]

    gauges = [ Metrics.botsAlive
             ]

-------------------------------------------------------------------------------
-- * BotNet

newtype BotNet a = BotNet { unBotNet :: ReaderT BotNetEnv IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow,
              MonadCatch, MonadMask, MonadBase IO)

class MonadIO m => MonadBotNet m where
    liftBotNet :: BotNet a -> m a

instance MonadBotNet BotNet where
    liftBotNet = id

instance MonadBaseControl IO BotNet where
    type StM BotNet a = StM (ReaderT BotNetEnv IO) a
    liftBaseWith f = BotNet $ liftBaseWith (\run -> f (run . unBotNet))
    restoreM       = BotNet . restoreM

instance MonadHttp BotNet where
    getManager = serverManager <$> getServer

instance MonadClient BotNet where
    getServer = BotNet $ asks botNetServer
    getLogger = BotNet $ asks botNetLogger

instance MonadLogger BotNet where
    log l m = getLogger >>= \lg -> Logger.log lg l m

getBotSettings :: MonadBotNet m => m BotSettings
getBotSettings = liftBotNet . BotNet $ asks botNetSettings

getGen :: MonadBotNet m => m MWC.GenIO
getGen = liftBotNet . BotNet $ asks botNetGen

localBotSettings :: (BotSettings -> BotSettings) -> BotNet a -> BotNet a
localBotSettings f (BotNet m) = BotNet $ local g m
  where
    g e = e { botNetSettings = f (botNetSettings e) }

runBotNet :: MonadIO m => BotNetEnv -> BotNet a -> m a
runBotNet s (BotNet b) = liftIO $ runReaderT b s

-------------------------------------------------------------------------------
-- * BotSession

newtype BotSession a = BotSession { unBotSession :: ReaderT Bot BotNet a }
    deriving (Functor, Applicative, Monad, MonadIO,
              MonadThrow, MonadCatch, MonadMask, MonadBase IO)

instance MonadBotNet BotSession where
    liftBotNet = BotSession . lift

instance MonadBaseControl IO BotSession where
    type StM BotSession a = StM (ReaderT Bot BotNet) a
    liftBaseWith f = BotSession $ liftBaseWith (\run -> f (run . unBotSession))
    restoreM       = BotSession . restoreM

instance MonadHttp BotSession where
    getManager = serverManager <$> getServer

instance MonadClient BotSession where
    getServer = liftBotNet getServer
    getLogger = liftBotNet getLogger

instance MonadSession BotSession where
    getAuth   = BotSession $ liftIO . fmap fst . readIORef =<< asks botAuth
    setAuth a = BotSession $ do
        refresh <- nextAuthRefresh a
        authRef <- asks botAuth
        liftIO $ writeIORef authRef (a, refresh)

instance MonadLogger BotSession where
    log l m = do
        b  <- getBot
        lg <- getLogger
        botLog lg b l m

getBot :: BotSession Bot
getBot = BotSession ask

runBotSession :: MonadBotNet m => Bot -> BotSession a -> m a
runBotSession b (BotSession s) = liftBotNet $ runReaderT s b

-------------------------------------------------------------------------------
-- Bot

newtype BotTag = BotTag { unTag :: Text }
    deriving (Eq, Show, IsString)

data Bot = Bot
    { botTag          :: BotTag
    , botSettings     :: BotSettings
    , botUser         :: User
      -- TODO: Move into BotClient?
    , botAuth         :: IORef (Auth, UTCTime)
    , botEvents       :: TVar (Word16, [(UTCTime, Event)])
    , botAsserts      :: TQueue EventAssertion
    , botBacklog      :: TVar [EventAssertion]
    , botAssertCount  :: TVar Word16
    , botPushThread   :: IORef (Maybe (Async ()))
    , botHeartThread  :: IORef (Maybe (Async ()))
    , botAssertThread :: IORef (Maybe (Async ()))
    , botMetrics      :: BotMetrics
      -- END TODO
    , botClients      :: TVar [BotClient] -- TODO: IORef?
    , botPassphrase   :: PlainTextPassword
    }

instance Show Bot where
    showsPrec _ b = showString "Bot {"
                  . showString "  ID="    . shows (botId b)
                  . showString ", EMAIL=" . shows (botEmail b)
                  . showString ", TAG="   . shows (unTag (botTag b))
                  . showString "}"

instance Eq Bot where
    a == b = botId a == botId b

data BotClient = BotClient
    { botClientId       :: !ClientId
    , botClientLabel    :: !(Maybe Text)
    , botClientBox      :: !Box
    , botClientSigKeys  :: !SignalingKeys
    , botClientSessions :: !Clients -- TODO: Map UserId (Map ClientId Session)
    }

instance Eq BotClient where
    a == b = botClientId a == botClientId b

botId :: Bot -> UserId
botId = userId . botUser

botName :: Bot -> Text
botName = fromName . userName . botUser

botEmail :: Bot -> Maybe Text
botEmail = fmap fromEmail . userEmail . botUser

addBotClient :: MonadBotNet m => Bot -> ClientType -> Maybe Text -> m BotClient
addBotClient self cty label = do
    box <- liftIO $ openBox (userId $ botUser self) label
    pks <- liftIO $ genPrekeys box 100
    lk  <- liftIO $ genLastKey box
    sg  <- liftIO $ genSigKeys box
    let nc = NewClient
           { newClientPassword = Just (botPassphrase self)
           , newClientPrekeys  = pks
           , newClientLastKey  = lk
           , newClientSigKeys  = sg
           , newClientLabel    = label
           , newClientType     = cty
           , newClientClass    = Nothing
           , newClientCookie   = Nothing
           , newClientModel    = Nothing
           }
    cid <- clientId <$> runBotSession self (registerClient nc)
    clt <- BotClient cid label box sg <$> liftIO Clients.empty
    liftIO . atomically $ modifyTVar' (botClients self) (clt:)
    return clt

-- TODO: withBotClient :: MonadBotNet m => Bot -> ClientType -> Maybe Text -> (BotClient -> m a) -> m a

removeBotClient :: MonadBotNet m => Bot -> BotClient -> m ()
removeBotClient self bc = do
    let rm = RmClient (Just (botPassphrase self))
    runBotSession self (removeClient (botClientId bc) rm)
    liftIO $ deleteBox (botId self) (botClientLabel bc)
    liftIO $ atomically $ modifyTVar' (botClients self) (filter (/= bc))

getBotClients :: MonadIO m => Bot -> m [BotClient]
getBotClients = liftIO . readTVarIO . botClients

-------------------------------------------------------------------------------
-- Bot API

-- | Create a new 'Bot'. This is an expensive operation that involves:
--
--     * Registering a new user
--     * Checking the 'Mailbox' for the activation mail
--     * Activating the user
--     * Logging in and establishing a push connection
--
-- 'Bot's should usually be used for a longer period of time to run
-- 'BotSession's.
newBot :: MonadBotNet m => BotTag -> m Bot
newBot tag = liftBotNet $ do
    mbox <- randMailbox
    (new, pw) <- liftIO $ randUser (mailboxUser $ mailboxSettings mbox) tag
    let email = fromMaybe (error "No email!") (newUserEmail new)
    log Info $ msg $ "Register user: " <> fromEmail email
    user <- registerUser new
    log Info $ botLogFields (userId user) tag . msg (val "Await activation mail")
    sndr <- BotNet $ asks botNetSender
    keys <- liftIO $ awaitActivationMail mbox sndr email
    log Info $ botLogFields (userId user) tag . msg (val "Activate user")
    forM_ keys (uncurry activateKey >=> flip assertTrue "Activation failed.")
    bot <- mkBot tag user pw
    -- TODO: addBotClient?
    incrBotsCreatedNew
    return bot

-- | Obtain a "cached" 'Bot' based on an existing user identity.
-- TODO: Better name 'reuseBot'?
cachedBot :: MonadBotNet m => BotTag -> m Bot
cachedBot t = liftBotNet $ do
    CachedUser p u <- BotNet (asks botNetUsers) >>= Cache.get
    bot <- mkBot t (tagged t u) p
    incrBotsCreatedCached
    return bot

-- | Wait for the Bot's assertions to finish (through matching
-- an event or through timeout) before killing it (see 'killBot').
drainBot :: MonadBotNet m => Bot -> m ()
drainBot bot = awaitAssertions bot >> killBot bot

-- | Tear down a 'Bot':
--
--    * Stop all of a Bot's background activities / threads.
--    * Empty the Bot's event inbox.
--    * Transfer all of a Bot's collected metrics into the overall metrics
--      (see 'getMetrics').
--
-- It is recommended to ensure that all 'Bot's get killed properly at some
-- point to avoid leaking threads or producing incorrect metrics, even in the
-- presence of exceptions. See 'withRegisterBot' and 'withCachedBot'.
killBot :: MonadBotNet m => Bot -> m ()
killBot bot = liftBotNet $ do
    let future = parseTimeOrError True defaultTimeLocale "%Y" "2259"
    events <- liftIO . atomically $ gcEvents bot future
    runBotSession bot $ do
        llevel <- level <$> getLogger
        forM_ events $
            log (llevel `min` Info) . ignoredEventMsg llevel
    stopThread (botPushThread   bot)
    stopThread (botAssertThread bot)
    stopThread (botHeartThread  bot)
    transferBotMetrics bot
    decrBotsAlive
  where
    stopThread r = liftIO $ traverse_ cancel =<< atomicModifyIORef' r (Nothing,)

    ignoredEventMsg l e
        | l < Info  = msg (val "Event Ignored: " +++ show e)
        | otherwise = msg (val "Event Ignored: " +++ showEventType e)

withNewBot :: (MonadBotNet m, MonadMask m) => BotTag -> (Bot -> m a) -> m a
withNewBot t f = do
    bot <- newBot t
    f bot `finally` killBot bot

-- TODO: Better name: withReusedBot?
withCachedBot :: (MonadBotNet m, MonadMask m) => BotTag -> (Bot -> m a) -> m a
withCachedBot t f = do
    c <- liftBotNet . BotNet $ asks botNetUsers
    x@(CachedUser p u) <- Cache.get c
    b <- liftBotNet $ mkBot t (tagged t u) p
    f b `finally` killBot b `finally` Cache.put c x

-------------------------------------------------------------------------------
-- Assertions

data EventAssertion = EventAssertion
    { _assertType :: !EventType
    , _assertTime :: !UTCTime
    , _assertPred :: Event -> Bool
    , _assertOut  :: !(Maybe (TMVar (Maybe Event)))
    }

whenAsserts :: MonadBotNet m => BotNet () -> m ()
whenAsserts ma = liftBotNet $ do
    asserts <- BotNet $ asks botNetAssert
    when asserts $ ma >> incrAssertTotal

-- | Wait for the Bot's assertion queue to become empty.
awaitAssertions :: MonadBotNet m => Bot -> m ()
awaitAssertions bot = whenAsserts $ do
    n <- liftIO . atomically $ readTVar (botAssertCount bot)
    unless (n <= 0) $ do
        liftIO $ threadDelay 1000000
        awaitAssertions bot

-- | A requirement that must be 'True', regardless of whether assertions are
-- enabled or not. If the requirement fails, a 'RequirementFailed' exception
-- is thrown.
require :: MonadThrow m => Bool -> Text -> m ()
require True  _ = return ()
require False m = throwM $ RequirementFailed m

-- | Require a 'Maybe a' to be 'Just a', regardless of whether assertions are
-- enabled or not. If it is 'Nothing' a 'RequirementFailed' exception is thrown.
requireMaybe :: MonadThrow m => Maybe a -> Text -> m a
requireMaybe Nothing  m = throwM $ RequirementFailed m
requireMaybe (Just a) _ = return a

-- | Require a 'Either e a' to be 'Right a', regardless of whether assertions are
-- enabled or not. If it is 'Left e 'RequirementFailed' exception is thrown.
requireRight :: (Show e, MonadThrow m) => Either e a -> m a
requireRight (Left  e) = throwM $ RequirementFailed (pack $ show e)
requireRight (Right a) = return a

assertEqual :: (MonadBotNet m, Show a, Eq a) => a -> a -> Text -> m ()
assertEqual a b m = whenAsserts $
    unless (a == b) $ do
        incrAssertFailed
        log Error . msg $ val "Assertion failed: " +++ m +++ val ": "
                        +++ show a +++ val " /= " +++ show b

assertTrue :: MonadBotNet m => Bool -> Text -> m ()
assertTrue b m = whenAsserts $
    unless b $ do
        incrAssertFailed
        log Error . msg $ val "Assertion failed: " +++ m

assertFailure :: MonadBotNet m => Text -> m ()
assertFailure m = whenAsserts $ do
    incrAssertFailed
    log Error . msg $ val "Assertion failed: " +++ m

-- | Place an assertion on a 'Bot', expecting a matching 'Event' to arrive
-- in its inbox within a timeout window.
assertEvent :: MonadBotNet m => Bot -> EventType -> (Event -> Bool) -> m ()
assertEvent bot typ f = scheduleAssert bot typ f Nothing

-- | Like 'assertEvent' but blocks until the event arrives or the assertion
-- times out, returning 'Just' the matching event or 'Nothing', respectively.
awaitEvent :: MonadBotNet m => Bot -> EventType -> (Event -> Bool) -> m (Maybe Event)
awaitEvent bot typ f = liftBotNet $ do
    r <- liftIO newEmptyTMVarIO
    scheduleAssert bot typ f (Just r)
    liftIO . atomically $ takeTMVar r

scheduleAssert :: MonadBotNet m => Bot -> EventType -> (Event -> Bool) -> Maybe (TMVar (Maybe Event)) -> m ()
scheduleAssert bot typ f out = whenAsserts $ do
    t <- liftIO getCurrentTime
    r <- liftIO . atomically $ do
        n <- readTVar (botAssertCount bot)
        if n >= botMaxAsserts (botSettings bot)
            then return False
            else do
                writeTQueue (botAsserts bot) (EventAssertion typ t f out)
                writeTVar (botAssertCount bot) (n + 1)
                return True
    unless r $ liftBotNet $ do
        incrAssertFailed
        runBotSession bot . log Error . msg $
            "Too many event assertions. Dropped: " <> eventTypeText typ

-------------------------------------------------------------------------------
-- * Exceptions

data BotNetException
    = LoginFailed
    | RequirementFailed !Text
    deriving Typeable

instance Show BotNetException where
    show LoginFailed           = "BotNetException.LoginFailed"
    show (RequirementFailed m) = "BotNetException.RequirementFailed: " ++ unpack m

instance Exception BotNetException

-------------------------------------------------------------------------------
-- * Error Recovery

data BotNetFailure
    = BotNetFailure BotNetException
    | HttpFailure   HttpException
    | ClientFailure ClientException
    deriving Show

-- | A variant of 'Control.Exception.try' that recovers from common
-- 'BotNetFailure's. Every recovered exception is logged and counts
-- towards the 'exceptionsTotal' metric.
try :: (MonadBotNet m, MonadCatch m) => m a -> m (Either BotNetFailure a)
try ma = do
    r <- (Right <$> ma) `catches` handlers
    case r of
        Left  e -> do
            liftBotNet $ log Error . msg $ show e
            incrExceptionsTotal
            return $ Left e
        Right a -> return $ Right a
  where
    handlers = [ Handler $ \e -> return . Left $ BotNetFailure e
               , Handler $ \e -> return . Left $ HttpFailure   e
               , Handler $ \e -> return . Left $ ClientFailure e
               ]

-------------------------------------------------------------------------------
-- Internal Bot Lifecycle

mkBot :: BotTag -> User -> PlainTextPassword -> BotNet Bot
mkBot tag user pw = do
    log Info $ botLogFields (userId user) tag . msg (val "Login")
    let ident = fromMaybe (error "No email") (userEmail user)
    let cred  = PasswordLogin (LoginByEmail ident) pw Nothing
    auth <- login cred >>= maybe (throwM LoginFailed) return
    aref <- nextAuthRefresh auth
    env <- BotNet ask
    bot <- liftIO $ Bot tag (botNetSettings env) user
        <$> newIORef (auth, aref)
        <*> newTVarIO (0, []) -- event inbox
        <*> newTQueueIO       -- assert queue
        <*> newTVarIO []      -- assert backlog
        <*> newTVarIO 0       -- assert count
        <*> newIORef Nothing
        <*> newIORef Nothing
        <*> newIORef Nothing
        <*> newBotMetrics
        <*> newTVarIO []
        <*> pure pw
    liftIO $ do
        writeIORef (botHeartThread bot) . Just =<< async (heartbeat bot env)
        writeIORef (botPushThread bot)  . Just =<< connectPush bot env
        when (botNetAssert env) $
            writeIORef (botAssertThread bot) . Just =<< async (assert bot env)
    incrBotsAlive
    return bot

connectPush :: Bot -> BotNetEnv -> IO (Async ())
connectPush bot e = runBotNet e $ runBotSession bot $ do
    log Info $ msg (val "Establishing push channel")
    awaitNotifications (consume bot e)

consume :: Bot -> BotNetEnv -> Notification -> IO ()
consume bot e n = do
    let l = botNetLogger e
    tid <- myThreadId
    forM_ (notifEvents n) $ \evt ->
        Logger.log l Info $ botLogFields (botId bot) (botTag bot)
                          . field "Thread" (show tid)
                          . msg ("RCV: " <> showEventType evt)
    when (botNetAssert e) $ do
        now <- getCurrentTime
        atomically $ do
            addEvents bot now (notifEvents n)
            backlog <- swapTVar (botBacklog bot) []
            mapM_ (writeTQueue (botAsserts bot)) backlog

heartbeat :: Bot -> BotNetEnv -> IO a
heartbeat bot e = forever $ do
    threadDelay 10000000 -- 10s
    now <- getCurrentTime
    let l = botNetLogger e
    -- Refresh the auth token, if necessary
    (auth, expiry) <- readIORef $ botAuth bot
    when (now > expiry) $
        void . forkIO . runBotNet e . runBotSession bot $ do
            log Debug $ msg (val "Refreshing auth token")
            refreshAuth auth >>= maybe
                (log Error $ msg (val "Failed to refresh auth token"))
                setAuth
    -- Event & assertion maintenance
    when (botNetAssert e) $ do
        -- Remove old events from the inbox
        events <- atomically $ gcEvents bot now
        forM_ events $ \evt -> botLog l bot Warn
                     $ msg ("Event Timeout: " <> showEventType evt)
        -- Check if the event inbox is full and if so, log a warning
        size <- fst <$> readTVarIO (botEvents bot)
        when (size == botMaxEvents (botSettings bot)) $
            botLog l bot Warn $ msg (val "Event inbox full!")
        -- Remove old assertions from the backlog
        asserts <- atomically $ gcBacklog bot now
        forM_ asserts $ \(EventAssertion typ _ _ out) -> do
            for_ out $ liftIO . atomically . flip tryPutTMVar Nothing
            botLog l bot Warn $ msg ("Assertion Timeout: " <> eventTypeText typ)
    -- Re-establish the push connection, if it died
    push <- maybe (return Nothing) poll =<< readIORef (botPushThread bot)
    case push of
        Just  x -> do
            case x of
                Left  er -> botLog l bot Error $ msg $ val "Push channel error:" +++ show er
                Right _  -> botLog l bot Warn  $ msg $ val "Unexpected exit of push thread"
            a <- connectPush bot e
            writeIORef (botPushThread bot) (Just a)
        Nothing -> return ()

assert :: Bot -> BotNetEnv -> IO a
assert bot e = forever $ do
    assertion <- atomically $ readTQueue (botAsserts bot)
    let l = botNetLogger e
    thread <- myThreadId
    found  <- atomically $ matchAssertion bot assertion
    for_ found $ \evt ->
        botLog l bot Info $ field "Thread" (show thread)
                          . msg ("ACK: " <> showEventType evt)

matchAssertion :: Bot -> EventAssertion -> STM (Maybe Event)
matchAssertion bot a@(EventAssertion _ _ f out) = do
    (num, events) <- readTVar (botEvents bot)
    let (new, found) = foldl' go ([], Nothing) events
    case found of
        Just ev -> do
            for_ out $ flip tryPutTMVar (Just ev)
            writeTVar (botEvents bot) (num - 1, new)
            modifyTVar' (botAssertCount bot) (subtract 1)
            incrEventsAckd bot (eventType ev)
        Nothing -> modifyTVar' (botBacklog bot) (a:)
    return found
  where
    go (events, found) (et, ev)
        | isNothing found && f ev = (events, Just ev)
        | otherwise               = ((et, ev) : events, found)

addEvents :: Bot -> UTCTime -> [Event] -> STM ()
addEvents bot now new = do
    (num, old) <- readTVar (botEvents bot)
    let count = num + fromIntegral (length new)
    if count > botMaxEvents (botSettings bot)
        then retry
        else do
            let events = foldl' (\es e -> (now, e) : es) old new
            mapM_ (incrEventsRcvd bot . eventType) new
            writeTVar (botEvents bot) (count, events)

gcEvents :: Bot -> UTCTime -> STM [Event]
gcEvents bot now = do
    (num, old) <- readTVar (botEvents bot)
    let timeout = botEventTimeout (botSettings bot)
    let (keep, del) = partition (\(t, _) -> now `diffUTCTime` t <= timeout) old
    let numDel = fromIntegral $ length del
    when (numDel > 0) $ do
        writeTVar (botEvents bot) (num - numDel, keep)
        mapM_ (incrEventsIgnd bot . eventType . snd) del
    return $ fmap snd del

gcBacklog :: Bot -> UTCTime -> STM [EventAssertion]
gcBacklog bot now = do
    old <- readTVar (botBacklog bot)
    let timeout = botAssertTimeout (botSettings bot)
    let (keep, del) = partition (\(EventAssertion _ t _ _) -> now `diffUTCTime` t <= timeout) old
    let numDel = fromIntegral $ length del
    when (numDel > 0) $ do
        writeTVar (botBacklog bot) keep
        modifyTVar' (botAssertCount bot) (subtract numDel)
        forM_ del $ \(EventAssertion typ _ _ out) -> do
            for_ out $ flip tryPutTMVar Nothing
            incrEventsMssd bot typ

    return del

nextAuthRefresh :: MonadIO m => Auth -> m UTCTime
nextAuthRefresh (Auth _ tok) = liftIO $ do
    now <- getCurrentTime
    return $ (fromInteger (expiresIn tok) - 60) `addUTCTime` now

-------------------------------------------------------------------------------
-- * Reports & Metrics

report :: MonadBotNet m => Text -> SectionS -> m Report
report t s = do
    m <- getMetrics
    r <- createReport t m s
    o <- liftBotNet . BotNet $ asks botNetReportDir
    case o of
        Just dir ->
            let d = formatTime defaultTimeLocale "%Y-%m-%dT%H%M%S" (reportDate r)
                f = showString (unpack t) . showString "-" . showString d $ ".bot"
            in writeReport (dir </> f) r
        Nothing -> printReport r
    return r

getMetrics :: MonadBotNet m => m Metrics
getMetrics = liftBotNet . BotNet $ asks botNetMetrics

timed :: MonadBotNet m => Metrics.Path -> m a -> m a
timed p ma = do
    start <- liftIO getCurrentTime
    a <- ma
    stop <- liftIO getCurrentTime
    let d = round . (* 1000) $ stop `diffUTCTime` start
    m <- getMetrics
    liftIO $ Metrics.bucketsIncr 30 12 d p m
    return a

incrAssertTotal :: MonadBotNet m => m ()
incrAssertTotal = getMetrics >>= liftIO . Metrics.counterIncr Metrics.assertionsTotal

incrAssertFailed :: MonadBotNet m => m ()
incrAssertFailed = getMetrics >>= liftIO . Metrics.counterIncr Metrics.assertionsFailed

incrExceptionsTotal :: MonadBotNet m => m ()
incrExceptionsTotal = getMetrics >>= liftIO . Metrics.counterIncr Metrics.exceptionsTotal

incrBotsCreatedNew :: MonadBotNet m => m ()
incrBotsCreatedNew = getMetrics >>= liftIO . Metrics.counterIncr Metrics.botsCreatedNew

incrBotsCreatedCached :: MonadBotNet m => m ()
incrBotsCreatedCached = getMetrics >>= liftIO . Metrics.counterIncr Metrics.botsCreatedCached

incrBotsAlive :: MonadBotNet m => m ()
incrBotsAlive = getMetrics >>= liftIO . Metrics.gaugeIncr Metrics.botsAlive

decrBotsAlive :: MonadBotNet m => m ()
decrBotsAlive = getMetrics >>= liftIO . Metrics.gaugeDecr Metrics.botsAlive

-- Note: Separate TVars to avoid contention.
data BotMetrics = BotMetrics
    { botEventsRcvd :: TVar (HashMap Metrics.Path Word)
    , botEventsAckd :: TVar (HashMap Metrics.Path Word)
    , botEventsIgnd :: TVar (HashMap Metrics.Path Word)
    , botEventsMssd :: TVar (HashMap Metrics.Path Word)
    }

newBotMetrics :: IO BotMetrics
newBotMetrics = BotMetrics
    <$> newTVarIO HashMap.empty
    <*> newTVarIO HashMap.empty
    <*> newTVarIO HashMap.empty
    <*> newTVarIO HashMap.empty

incrEventsRcvd :: Bot -> EventType -> STM ()
incrEventsRcvd b e = modifyTVar' (botEventsRcvd (botMetrics b)) $
    HashMap.insertWith (+) (Metrics.eventTypeRcvd e) 1

incrEventsAckd :: Bot -> EventType -> STM ()
incrEventsAckd b e = modifyTVar' (botEventsAckd (botMetrics b)) $
    HashMap.insertWith (+) (Metrics.eventTypeAckd e) 1

incrEventsIgnd :: Bot -> EventType -> STM ()
incrEventsIgnd b e = modifyTVar' (botEventsIgnd (botMetrics b)) $
    HashMap.insertWith (+) (Metrics.eventTypeIgnd e) 1

incrEventsMssd :: Bot -> EventType -> STM ()
incrEventsMssd b e = modifyTVar' (botEventsMssd (botMetrics b)) $
    HashMap.insertWith (+) (Metrics.eventTypeMssd e) 1

transferBotMetrics :: MonadBotNet m => Bot -> m ()
transferBotMetrics b = getMetrics >>= \m -> liftIO $ do
    -- Obtain current values
    l@[rcvd, ackd, ignd, mssd] <- atomically $ do
        rcvd <- readTVar $ botEventsRcvd (botMetrics b)
        ackd <- readTVar $ botEventsAckd (botMetrics b)
        ignd <- readTVar $ botEventsIgnd (botMetrics b)
        mssd <- readTVar $ botEventsMssd (botMetrics b)
        return [rcvd, ackd, ignd, mssd]
    -- Update per event type counters
    let add (p,n) = Metrics.counterAdd n p m
    mapM_ add (concatMap HashMap.toList l)
    -- Update Totals
    add (Metrics.eventsTotalRcvd, sum rcvd)
    add (Metrics.eventsTotalAckd, sum ackd)
    add (Metrics.eventsTotalIgnd, sum ignd)
    let s = sum mssd
    add (Metrics.eventsTotalMssd, s)
    add (Metrics.assertionsFailed, s)

-------------------------------------------------------------------------------
-- Logging

botLog :: MonadIO m => Logger -> Bot -> Level -> (Msg -> Msg) -> m ()
botLog lg b l m = liftIO $ Logger.log lg l $ botLogFields (botId b) (botTag b) . m

botLogFields :: UserId -> BotTag -> Msg -> Msg
botLogFields u t = field "Bot" (show u) . field "Tag" (unTag t)

-------------------------------------------------------------------------------
-- Randomness

randUser :: Email -> BotTag -> IO (NewUser, PlainTextPassword)
randUser (Email loc dom) (BotTag tag) = do
    uuid    <- nextRandom
    pwdUuid <- nextRandom
    let email = Email (loc <> "+" <> tag <> "-" <> pack (toString uuid)) dom
    let passw = PlainTextPassword (pack (toString pwdUuid))
    return (NewUser
        { newUserName           = Name (tag <> "-Wirebot-" <> pack (toString uuid))
        , newUserIdentity       = Just (EmailIdentity email)
        , newUserPassword       = Just passw
        , newUserPict           = Nothing
        , newUserAssets         = []
        , newUserAccentId       = Nothing
        , newUserPhoneCode      = Nothing
        , newUserInvitationCode = Nothing
        , newUserLabel          = Nothing
        , newUserLocale         = Nothing
        , newUserTeam           = Nothing
        }, passw)

randMailbox :: BotNet Mailbox
randMailbox = do
    e <- BotNet ask
    i <- liftIO $ MWC.uniformR (0, length (botNetMailboxes e) - 1) (botNetGen e)
    return $ botNetMailboxes e !! i

tagged :: BotTag -> User -> User
tagged t u = u { userName = Name $ unTag t <> "-" <> fromName (userName u) }

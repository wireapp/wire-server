{-# LANGUAGE OverloadedStrings #-}

module Network.Wire.Bot.Cache
    ( CachedUser (..)
    , Cache
    , new
    , Network.Wire.Bot.Cache.empty
    , get
    , put
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Conversion
import Data.IORef
import Data.LanguageCodes
import Data.Maybe (fromMaybe)
import Data.Misc
import Data.Monoid
import Data.Text.Encoding
import Data.Text.Lazy hiding (length, map)
import Data.Text.Lazy.IO
import Network.Wire.Client.API.User
import Prelude hiding (lines, readFile)
import System.Logger hiding (new)
import System.Random.MWC

newtype Cache = Cache { cache :: IORef [CachedUser] }

data CachedUser = CachedUser !PlainTextPassword !User

-- TODO: Use GenIO to randomise the order or remove the argument.
new :: Logger -> GenIO -> FilePath -> IO Cache
new l _ p = do
    c <- newIORef =<< foldM (toUser l) [] =<< map (splitOn ",") . lines <$> readFile p
    return (Cache c)

empty :: IO Cache
empty = Cache <$> newIORef []

get :: MonadIO m => Cache -> m CachedUser
get c = liftIO $ atomicModifyIORef (cache c) $ \u ->
    case u of
        []     -> error "empty cache"
        (x:xs) -> (xs, x)

put :: MonadIO m => Cache -> CachedUser -> m ()
put c a = liftIO $ atomicModifyIORef (cache c) $ \u -> (a:u, ())

toUser :: Logger -> [CachedUser] -> [Text] -> IO [CachedUser]
toUser _ acc [i, e, p] = do
    let pw = PlainTextPassword . toStrict $ strip p
    let iu = error "Cache.toUser: invalid user"
    let ie = error "Cache.toUser: invalid email"
    let ui = fromMaybe iu . fromByteString . encodeUtf8 . toStrict . strip $ i
    let em = fromMaybe ie . parseEmail . toStrict . strip $ e
    return . (:acc) $ CachedUser pw User
        { userId       = ui
        , userName     = Name $ "Fakebot-" <> toStrict (strip i)
        , userPict     = Pict []
        , userAssets   = []
        , userIdentity = Just (EmailIdentity em)
        , userAccentId = ColourId 0
        , userDeleted  = False
        , userLocale   = Locale (Language EN) Nothing
        , userService  = Nothing
        , userHandle   = Nothing
        }
toUser g acc entry = do
    warn g $ msg (val "invalid entry: " +++ show entry)
    return acc

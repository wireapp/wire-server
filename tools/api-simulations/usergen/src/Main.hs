{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

-- | A small utility for registering users on the backend. Useful for testing.
module Main where

import BasePrelude

import Brig.Types.User
import Control.Monad.IO.Class
import Data.Misc (fromPlainTextPassword)
import Data.Text (Text)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wire.Bot
import Network.Wire.Bot.Metrics (assertionsFailed)
import Network.Wire.Bot.Report
import Options.Applicative

import qualified Data.Text     as T
import qualified Data.Text.IO  as T
import qualified System.Logger as Log

main :: IO ()
main = do
    o <- parseOptions
    m <- newManager tlsManagerSettings
    l <- Log.new Log.defSettings
    e <- newBotNetEnv m l (userGenBotNetSettings o)
    r <- runBotNet e $ do
        userGen (userGenUserCount o) (userGenStartIndex o) (userGenOutputFile o)
        Log.flush l >> Log.close l
        report "User Generator" defaultSections
    unless (reportCounter r assertionsFailed == 0)
        exitFailure

-------------------------------------------------------------------------------
-- Settings

data UserGenSettings = UserGenSettings
    { userGenUserCount      :: Int
    , userGenStartIndex     :: Int
    , userGenOutputFile     :: FilePath
    , userGenBotNetSettings :: BotNetSettings
    } deriving Show

parseOptions :: IO UserGenSettings
parseOptions = execParser (info (helper <*> userGenSettingsParser) desc)
  where
    desc = header "User Generator" <> fullDesc
    userGenSettingsParser = do
        userGenUserCount <- argument auto $
            help "Number of users to generate" <>
            metavar "USERCOUNT"
        userGenOutputFile <- strArgument $
            help "Where to write users' credentials" <>
            metavar "OUTFILE"
        userGenStartIndex <- option auto $
            help "Index from which to start" <>
            long "start" <>
            metavar "INT" <>
            value 1 <>
            showDefault
        userGenBotNetSettings <- botNetSettingsParser
        pure UserGenSettings{..}

-------------------------------------------------------------------------------
-- Logic

userGen
    :: Int              -- ^ How many users to generate
    -> Int              -- ^ Starting index
    -> FilePath         -- ^ Where to save the users
    -> BotNet ()
userGen n start path = do
    liftIO $ writeFile path ""
    -- NB. I tried to parallelize this with @async-pool@, but got "thread
    -- blocked indefinitely in an STM transaction and gave up :(
    for_ [start..start+n-1] $ \i -> do
        user <- newBot (fromString ("Fake" <> show i))
        liftIO $ T.appendFile path (userLine user <> "\n")
        drainBot user

-- | Show a user in format that is accepted by other utilities when passed
-- via @--users-file@:
--
-- @
-- user's UUID,email,password
-- @
userLine :: Bot -> Text
userLine bot = T.intercalate ","
    [ T.pack (show (userId user))
    , maybe noEmailErr (T.pack . show) (emailIdentity =<< userIdentity user)
    , fromPlainTextPassword (botPassphrase bot)
    ]
  where
    user = botUser bot
    noEmailErr = error ("Generated user has no email: " <> show bot)

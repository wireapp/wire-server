{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class
import Data.Foldable (for_)
import Data.Text (Text, pack)
import System.Environment
import System.Exit
import System.IO
import System.Metrics.Collectd.Config
import System.Metrics.Collectd.IO

main :: IO ()
main = do
    host <- getEnv "COLLECTD_HOSTNAME"
    intv <- round . double <$> getEnv "COLLECTD_INTERVAL"
    args <- getArgs
    if null args
        then error "missing path to configuration file"
        else start (pack host) intv (head args)
  where
    double :: String -> Double
    double = (1000.0 *) . read

start :: Text -> Int -> FilePath -> IO ()
start h i p = do
    c <- readConfig i h p
    case c of
        Left  err -> putStrLn err >> exitFailure
        Right cfg -> do
            ctx <- create
            runCollector ctx $ do
                collectAll cfg
                err <- wait
                for_ err $
                    liftIO . hPutStrLn stderr . show
                terminate

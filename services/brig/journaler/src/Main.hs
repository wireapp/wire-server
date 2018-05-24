{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Cassandra as C
import Cassandra.Settings as C
import Control.Lens hiding ((.=))
import Data.Monoid
import Journal
import Network.HTTP.Client
import Network.HTTP.Client.OpenSSL
import OpenSSL (withOpenSSL)
import OpenSSL.Session as Ssl
import Options
import Options.Applicative

import qualified OpenSSL.X509.SystemStore as Ssl
import qualified System.Logger as Log

main :: IO ()
main = withOpenSSL $ do
    s   <- execParser (info (helper <*> settingsParser) desc)
    lgr <- initLogger
    c   <- initCas (s^.rCasSettings) lgr
    mgr <- initHttpManager
    aws <- Journal.mkEnv lgr (s^.rJournalQueue) mgr
    runCommand lgr aws c (s^.rStart)
  where
    desc = header   "user-journaler"
        <> progDesc "User event journaler"
        <> fullDesc

    initLogger
        = Log.new
        . Log.setOutput Log.StdOut
        . Log.setFormat Nothing
        . Log.setBufSize 0
        $ Log.defSettings

    initHttpManager :: IO Manager
    initHttpManager = do
        ctx <- Ssl.context
        Ssl.contextSetVerificationMode ctx $ Ssl.VerifyPeer True True Nothing
        Ssl.contextAddOption ctx SSL_OP_NO_SSLv2
        Ssl.contextAddOption ctx SSL_OP_NO_SSLv3
        Ssl.contextAddOption ctx SSL_OP_NO_TLSv1
        Ssl.contextSetCiphers ctx rsaCiphers
        Ssl.contextLoadSystemCerts ctx
        newManager (opensslManagerSettings ctx)
            { managerResponseTimeout     = responseTimeoutMicro 10000000
            , managerConnCount           = 100
            , managerIdleConnectionCount = 300
            }

    initCas cas l
        = C.init l
        . C.setContacts        (cas^.cHosts) []
        . C.setPortNumber      (fromIntegral $ cas^.cPort)
        . C.setKeyspace        (cas^.cKeyspace)
        . C.setProtocolVersion C.V3
        $ C.defSettings

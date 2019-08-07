module Main (main) where

import Imports
import Cassandra as C
import Cassandra.Settings as C
import Control.Lens hiding ((.=))
import Galley.Options
import Journal
import Network.HTTP.Client
import Network.HTTP.Client.OpenSSL
import OpenSSL (withOpenSSL)
import OpenSSL.Session as Ssl
import Ssl.Util
import Options as O
import Options.Applicative

import qualified Galley.Aws as Aws
import qualified OpenSSL.X509.SystemStore as Ssl
import qualified System.Logger.Class as Log


main :: IO ()
main = withOpenSSL $ do
    s <- execParser (info (helper <*> settingsParser) desc)
    lgr <- initLogger
    c <- initCas (s^.rCasSettings) lgr
    aws <- mkAWSEnv (s^.rJournalSettings)
    runCommand lgr aws c (s^.rStart)
  where
    desc = header   "team-journaler"
        <> progDesc "Team event journaler"
        <> fullDesc

    initLogger
        = Log.new  -- TODO: use mkLogger'?
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
        newManager (opensslManagerSettings (pure ctx))  -- see Note [SSL context]
            { managerResponseTimeout     = responseTimeoutMicro 10000000
            , managerConnCount           = 100
            , managerIdleConnectionCount = 300
            }

    mkAWSEnv :: JournalOpts -> IO Aws.Env
    mkAWSEnv o = do
        l   <- Log.new $ Log.setOutput Log.StdOut . Log.setFormat Nothing $ Log.defSettings  -- TODO: use mkLogger'?
        mgr <- initHttpManager
        Aws.mkEnv l mgr o

    initCas cas l
        = C.init
        $ C.setLogger          (C.mkLogger l)
        . C.setContacts        (cas^.cHosts) []
        . C.setPortNumber      (fromIntegral $ cas^.cPort)
        . C.setKeyspace        (cas^.cKeyspace)
        . C.setProtocolVersion C.V4
        $ C.defSettings

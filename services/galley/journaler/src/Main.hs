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

module Main
  ( main,
  )
where

import Cassandra as C
import Cassandra.Settings as C
import Control.Lens hiding ((.=))
import qualified Galley.Aws as Aws
import Galley.Options
import Imports
import Journal
import Network.HTTP.Client
import Network.HTTP.Client.OpenSSL
import OpenSSL (withOpenSSL)
import OpenSSL.Session as Ssl
import qualified OpenSSL.X509.SystemStore as Ssl
import Options as O
import Options.Applicative
import Ssl.Util
import qualified System.Logger.Class as Log

main :: IO ()
main = withOpenSSL $ do
  s <- execParser (info (helper <*> settingsParser) desc)
  lgr <- initLogger
  c <- initCas (s ^. rCasSettings) lgr
  aws <- mkAWSEnv (s ^. rJournalSettings)
  runCommand lgr aws c (s ^. rStart)
  where
    desc =
      header "team-journaler"
        <> progDesc "Team event journaler"
        <> fullDesc
    initLogger =
      Log.new -- TODO: use mkLogger'?
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
      newManager
        (opensslManagerSettings (pure ctx)) -- see Note [SSL context]
          { managerResponseTimeout = responseTimeoutMicro 10000000,
            managerConnCount = 100,
            managerIdleConnectionCount = 300
          }
    mkAWSEnv :: JournalOpts -> IO Aws.Env
    mkAWSEnv o = do
      l <- Log.new $ Log.setOutput Log.StdOut . Log.setFormat Nothing $ Log.defSettings -- TODO: use mkLogger'?
      mgr <- initHttpManager
      Aws.mkEnv l mgr o
    initCas cas l =
      C.init
        $ C.setLogger (C.mkLogger l)
          . C.setContacts (cas ^. cHosts) []
          . C.setPortNumber (fromIntegral $ cas ^. cPort)
          . C.setKeyspace (cas ^. cKeyspace)
          . C.setProtocolVersion C.V4
        $ C.defSettings

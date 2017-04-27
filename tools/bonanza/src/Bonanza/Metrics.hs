{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bonanza.Metrics
    ( Stats (..)
    , debugCollectd
    , dumpStderr
    , emitCollectd
    , formatCollectd
    , formatStderr
    )
where

import Control.Exception              (bracket)
import Control.Monad                  (void)
import Data.Collectd.PlainText
import Data.Int                       (Int64)
import Data.List                      (foldl', intercalate)
import Data.Monoid
import Data.Text                      (Text, pack)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Encoding
import Data.Time
import Network.BSD                    (getHostName)
import Network.Socket                 hiding (recv)
import Network.Socket.ByteString.Lazy
import System.IO                      (hPutStrLn, stderr)

import qualified Data.Text.Lazy.IO as TIO


data Stats = Stats
    { sBytesIn  :: !Int64
    , sBytesOut :: !Int64
    , sCPUTime  :: !DiffTime
    , sWallTime :: !NominalDiffTime
    , sEventsIn :: !Int64
    } deriving (Eq, Show)


dumpStderr :: Stats -> IO ()
dumpStderr = hPutStrLn stderr . ('\n':) . formatStderr

formatStderr :: Stats -> String
formatStderr Stats{..} = unlines . map (intercalate "\t") $
    [ ["Events parsed:", show sEventsIn]
    , ["Bytes read:",    show sBytesIn ]
    , ["Bytes written:", show sBytesOut]
    , ["CPU time:",      show sCPUTime ]
    , ["Wall time:",     show sWallTime]
    ]

emitCollectd :: FilePath -> Maybe String -> Stats -> IO ()
emitCollectd p inst s = do
    host <- pack <$> getHostName
    withSocket $ \ sock -> do
        sendAll sock . encodeUtf8 . toLazyText $
            formatCollectd host (pack `fmap` inst) s
        void $ recv sock 1024
  where
    withSocket = bracket
        (do sock <- socket AF_UNIX Stream defaultProtocol
            connect sock (SockAddrUnix p)
            return sock)
        close

debugCollectd :: Maybe String -> Stats -> IO ()
debugCollectd inst s = do
    host <- pack <$> getHostName
    TIO.hPutStrLn stderr . toLazyText $
        formatCollectd host (pack `fmap` inst) s

formatCollectd :: Text -> Maybe Text -> Stats -> Builder
formatCollectd host inst Stats{..} =
    let ident = Identifier host "bonanza" inst "gauge"
        vlist = ValueList Now . (:[])
        vals  = [ ("events_in", Gauge $ fromIntegral sEventsIn)
                , ("bytes_in",  Gauge $ fromIntegral sBytesIn)
                , ("bytes_out", Gauge $ fromIntegral sBytesOut)
                , ("cpu_time",  Gauge $ realToFrac   sCPUTime)
                , ("wall_time", Gauge $ realToFrac   sWallTime)
                ]
     in foldl' (\ b (l,v) -> b
                          <> formatRequest (PutVal (ident (Just l)) [] (vlist v))
                          <> nl)
               mempty
               vals

nl :: Builder
nl = singleton '\n'

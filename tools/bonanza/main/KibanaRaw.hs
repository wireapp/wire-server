{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import           Crypto.Hash
import           Data.Aeson.Encoding      (encodingToLazyByteString, pair, pairs)
import qualified Data.Aeson.Encoding      as Encoding
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Lazy     as Lazy
import           Data.Conduit
import qualified Data.Conduit.Binary      as CB
import qualified Data.Conduit.List        as CL
import           Data.Monoid
import qualified Data.Text                as Text
import           Data.Text.Encoding       (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Version             (showVersion)
import           Options.Applicative      hiding (action)
import           Paths_bonanza            (version)
import           System.Clock
import           System.IO


data Opts = Opts
    { optKibanaType  :: String
    , optKibanaIndex :: String
    , optNull        :: Bool
    } deriving Show

parseOpts :: IO Opts
parseOpts = execParser $ info (helper <*> parser) desc
  where
    desc = fullDesc
        <> header   ("kibana-raw v" ++ showVersion version)
        <> progDesc "Read raw logs and output Elasticsearch bulk records"


    parser = Opts
        <$> option str
                ( long "kibana-type"
               <> help "Value of the _type field"
                )
        <*> option str
                ( long "kibana-index"
               <> help "Index prefix"
               <> value "raw-logs"
               <> showDefault
                )
        <*> switch
                ( short '0'
               <> long  "null"
               <> help  "Separate each action/document pair by a null character"
                )


main :: IO ()
main = do
    opts <- parseOpts
    runConduit
          $ CB.sourceHandle stdin
         .| CB.lines
         .| CL.mapM (fmtKibana opts)
         .| CB.sinkHandle stdout

fmtKibana :: Opts -> ByteString -> IO ByteString
fmtKibana Opts{..} line = do
    ts <- getTime Realtime
    pure . Lazy.toStrict $
        action ts <> "\n" <> document ts <> "\n" <> if optNull then "\0" else mempty
  where
    idx ts = Text.pack $ mconcat
        [ optKibanaIndex
        , "-"
        , show . utctDay . posixSecondsToUTCTime . fromIntegral . sec $ ts
        ]

    action ts = encodingToLazyByteString . pairs . pair "index" $
        pairs ( pair "_index" (Encoding.text   (idx ts))
             <> pair "_type"  (Encoding.string optKibanaType)
             <> pair "_id"    (Encoding.string (show (hash line :: Digest SHA1)))
              )

    document ts = encodingToLazyByteString $
        pairs ( pair "timestamp" (Encoding.integer $ toNanoSecs ts `div` 1000000)
             <> pair "message"   (Encoding.text (decodeUtf8With lenientDecode line))
              )

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- necessary because of missing 'Eq ZonedTime' instance
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- Network.BSD got deprecated in network-2.7; this line won't be needed once we
-- move to network-3.0 because then we can use the network-bsd package
{-# OPTIONS_GHC -Wno-deprecations #-}

module Bonanza.Streaming.Kibana
    ( KibanaEvent

    , fromLogEvent
    , jsonEncode
    )
where

import           Imports                    hiding (stripPrefix)
import           Bonanza.Types
import           Control.Lens               ((^.))
import           Data.Aeson
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as BL
import qualified Data.HashMap.Strict        as Map
import           Data.Text                  (pack)
import qualified Data.Text                  as T
import           Data.Time
import           Data.Time.Clock.POSIX      (utcTimeToPOSIXSeconds)
import           Network.BSD                (getHostName)
import           Network.Socket
import           System.IO.Unsafe           (unsafePerformIO)


data BulkAction
    = Index { _index :: !IndexName
            , _type  :: !Text
            , _id    :: !(Maybe Text)
            }
    deriving (Eq, Show, Generic)

instance ToJSON BulkAction where
    toJSON Index{..} =
        let fields = catMaybes [ Just ("_index" .=  _index)
                               , Just ("_type"  .=  _type)
                               , fmap ("_id"    .=) _id
                               ]
         in object [ "index" .= object fields ]

newtype IndexName = IndexName Text
    deriving (Eq, Show, Generic, IsString)

instance ToJSON IndexName

data KibanaEvent = KibanaEvent
    { esTimestamp :: !ZonedTime
    , esOrigin    :: !Host
    , esTags      :: !Tags
    , esMessage   :: !Text
    } deriving (Eq, Show, Generic)

deriving instance Eq ZonedTime

instance ToJSON KibanaEvent where
    toJSON = genericToJSON $ modLabels fmod
      where
        fmod "esTimestamp" = "@timestamp"
        fmod x             = stripPrefix "es" x

fromLogEvent :: LogEvent -> IO KibanaEvent
fromLogEvent evt = do
    ts <- utcToZonedTime utc <$> maybe getCurrentTime pure (evt^.logTime)
    return KibanaEvent
        { esTimestamp = ts
        , esOrigin    = fromMaybe thisHost (evt^.logOrigin)
        , esTags      = dedotKeys (evt^.logTags)
        , esMessage   = fromMaybe mempty   (evt^.logMessage)
        }
  where
    -- elasticsearch 2.x doesn't support dots in field names anymore, so we just
    -- replace them by underscore
    dedotKeys
        = Tags
        . Map.foldlWithKey'
              (\m k v -> Map.insert (T.replace "." "_" k) v m)
              Map.empty
        . fromTags

jsonEncode :: Text -> KibanaEvent -> BL.ByteString
jsonEncode idxpre kev@KibanaEvent{..} =
      BB.toLazyByteString
    . mconcat
    $ map BB.lazyByteString
          [ encode action
          , "\n"
          , encode kev
          , "\n"
          ]
  where
    action = Index
        { _index = idx
        , _type  = fromMaybe "generic" srv
        , _id    = mkDocId esTimestamp esTags
        }

    srv = tagTxt $ Map.lookup "srv" (fromTags esTags)
    idx = IndexName $ idxpre <> "-" <> ts
    ts  = pack . showGregorian . localDay . zonedTimeToLocalTime $ esTimestamp


mkDocId :: ZonedTime -> Tags -> Maybe Text
mkDocId ts tgs =
    if Map.member "srv" (fromTags tgs)
        then (<>) <$> requestId tgs <*> pure ("-" <> secs ts)
        else Nothing
  where
    secs = T.pack . show . utcTimeToPOSIXSeconds . zonedTimeToUTC

requestId :: Tags -> Maybe Text
requestId (Tags t) =
    let rid = Map.lookup "request" t
    in mfilter (/="N/A") (tagTxt rid)

tagTxt :: Maybe TagValue -> Maybe Text
tagTxt (Just (String t)) = Just t
tagTxt _                 = Nothing

thisHost :: Host
thisHost = unsafePerformIO $ do
    localhost <- getHostName
    addrinfo  <- getAddrInfo (Just defaultHints { addrFlags = [AI_CANONNAME] })
                             (Just localhost)
                             Nothing
    return . Host . pack
           . fromMaybe localhost
           . head' . mapMaybe addrCanonName
           $ addrinfo
  where
    head' []    = Nothing
    head' (x:_) = Just x
{-# NOINLINE thisHost #-}

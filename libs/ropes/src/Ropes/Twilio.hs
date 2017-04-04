{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Ropes.Twilio
    ( -- * Types
      SID (..)
    , AccessToken (..)
    , Credentials
    , Message (..)
    , MessageId
    , LookupDetail (..)
    , CarrierInfo (..)
    , PhoneType (..)
    , LookupResult (..)
    , ErrorResponse (..)
    , ParseError (..)

      -- * Functions
    , sendMessage
    , sendMessages
    , lookupPhone
    , tryTwilio
    ) where

import Control.Applicative
#if MIN_VERSION_errors(2,0,0)
import Control.Error (ExceptT (..))
#else
import Control.Error (EitherT (..))
#endif
import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ISO3166_CountryCodes (CountryCode)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Traversable (forM)
import Data.Typeable
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI
import Prelude hiding (head, length)

import qualified Data.ByteString.Char8 as C
import qualified Data.List.NonEmpty as N

-- * Types

newtype MessageId = MessageId ByteString
newtype SID = SID ByteString
newtype AccessToken = AccessToken ByteString

type Credentials = (SID, AccessToken)

data Message = Message
    { msgFrom :: !Text
    , msgTo   :: !Text
    , msgText :: !Text
    } deriving (Eq, Show)

data ErrorResponse = ErrorResponse
    { errStatus   :: !Int
    , errMessage  :: !Text
    , errCode     :: !(Maybe Int)
    , errMoreInfo :: !(Maybe Text)
    } deriving (Eq, Show, Typeable)

instance Exception ErrorResponse

instance FromJSON ErrorResponse where
    parseJSON = withObject "error-response" $ \o ->
        ErrorResponse <$> o .:  "status"
                      <*> o .:  "message"
                      <*> o .:? "code"
                      <*> o .:? "more_info"

newtype ParseError = ParseError String
    deriving (Eq, Show, Typeable)

instance Exception ParseError

data MessageResponse = MessageResponse
    { msgId :: !MessageId
    }

instance FromJSON MessageResponse where
    parseJSON = withObject "MessageResponse" $ \o ->
        MessageResponse . MessageId . encodeUtf8 <$> o .: "sid"

data LookupDetail
    = LookupNoDetail
    | LookupCarrier
    deriving (Eq, Show)

data LookupResult = LookupResult
    { lookupE164    :: !Text
    , lookupCarrier :: !(Maybe CarrierInfo)
    }

data CarrierInfo = CarrierInfo
    { carrierName :: !(Maybe Text)
    , carrierType :: !(Maybe PhoneType)
    }

data PhoneType
    = Landline
    | Mobile
    | VoIp
    deriving (Eq, Show)

instance FromJSON LookupResult where
    parseJSON = withObject "LookupResult" $ \o ->
        LookupResult <$> o .:  "phone_number"
                     <*> o .:? "carrier"

instance FromJSON CarrierInfo where
    parseJSON = withObject "CarrierInfo" $ \o ->
        CarrierInfo <$> o .:? "name"
                    <*> o .:? "type"

instance FromJSON PhoneType where
    parseJSON = withText "PhoneType" $ \t ->
        case t of
            "mobile"   -> return Mobile
            "landline" -> return Landline
            "voip"     -> return VoIp
            x          -> fail $ "Unexpected phone type: " ++ show x

-- * Functions

#if MIN_VERSION_errors(2,0,0)
tryTwilio :: MonadIO m => IO a -> ExceptT ErrorResponse m a
tryTwilio = ExceptT . liftIO . try
#else
tryTwilio :: MonadIO m => IO a -> EitherT ErrorResponse m a
tryTwilio = EitherT . liftIO . try
#endif

sendMessage :: Credentials -> Manager -> Message -> IO MessageId
sendMessage cr mgr msg = N.head <$> sendMessages cr mgr (msg :| [])

sendMessages :: Credentials -> Manager -> NonEmpty Message -> IO (NonEmpty MessageId)
sendMessages cr mgr msgs = forM msgs $ \m -> do
    let req = urlEncodedBody (form m) . applyBasicAuth sid token $ apiReq
    rsp <- httpLbs req mgr
    if responseStatus rsp == status201
        then case eitherDecode (responseBody rsp) of
            Right r -> return $ msgId r
            Left  e -> throwIO $ ParseError e
        else case eitherDecode (responseBody rsp) of
            Right e -> throwIO (e :: ErrorResponse)
            Left  e -> throwIO $ ParseError e
  where
    apiReq = defaultRequest
        { method = "POST"
        , host   = "api.twilio.com"
        , secure = True
        , port   = 443
        , path   = "/2010-04-01/Accounts/" <> sid <> "/Messages.json"
        }

    (SID sid, AccessToken token) = cr

    form m = [ ("From", encodeUtf8 . msgFrom $ m)
             , ("To"  , encodeUtf8 . msgTo   $ m)
             , ("Body", encodeUtf8 . msgText $ m)
             ]

lookupPhone :: Credentials
            -> Manager
            -> Text
            -> LookupDetail
            -> Maybe CountryCode
            -> IO LookupResult
lookupPhone cr mgr phone detail country = do
    let req = applyBasicAuth sid token apiReq
    rsp <- httpLbs req mgr
    if responseStatus rsp == status200
        then case eitherDecode (responseBody rsp) of
            Right r -> return r
            Left  e -> throwIO $ ParseError e
        else case eitherDecode (responseBody rsp) of
            Right e -> throwIO (e :: ErrorResponse)
            Left  e -> throwIO $ ParseError e
  where
    (SID sid, AccessToken token) = cr

    apiReq = defaultRequest
        { method      = "GET"
        , host        = "lookups.twilio.com"
        , secure      = True
        , port        = 443
        , path        = "/v1/PhoneNumbers/" <> encodeUtf8 phone
        , queryString = renderSimpleQuery False queryItems
        }

    queryItems = catMaybes [ countryCode <$> country
                           , lookupType detail
                           ]

    countryCode c = ("CountryCode", C.pack (show c))

    lookupType LookupNoDetail = Nothing
    lookupType LookupCarrier  = Just ("Type", "carrier")

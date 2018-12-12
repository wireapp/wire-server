{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP                #-}

module Network.Wai.Utilities.Error where

import Imports
import Control.Error
import Data.Aeson hiding (Error)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.HTTP.Types

data Error = Error
    { code    :: !Status
    , label   :: !LText
    , message :: !LText
    } deriving (Show, Typeable)

instance Exception Error

-- | Assumes UTF-8 encoding.
byteStringError :: Status -> LByteString -> LByteString -> Error
byteStringError s l m = Error s (decodeUtf8 l) (decodeUtf8 m)

instance ToJSON Error where
    toJSON (Error c l m) =
        object [ "code"    .= statusCode c
               , "label"   .= l
               , "message" .= m
               ]

instance FromJSON Error where
    parseJSON = withObject "Error" $ \o ->
        Error <$> (toEnum <$> o .: "code")
              <*> o .: "label"
              <*> o .: "message"

-- FIXME: This should not live here.
infixl 5 !>>
#if MIN_VERSION_errors(2,0,0)
(!>>) :: Monad m => ExceptT a m r -> (a -> b) -> ExceptT b m r
#else
(!>>) :: Monad m => EitherT a m r -> (a -> b) -> EitherT b m r
#endif
(!>>) = flip fmapLT

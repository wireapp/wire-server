{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Brig.Types.Activation
    ( module Brig.Types.Activation
    , module C
    ) where

import Brig.Types.Common as C
import Control.Applicative
import Data.Aeson
import Data.ByteString.Conversion
import Data.Json.Util ((#))
import Data.Text.Ascii

-- | An opaque identifier of a 'UserKey' awaiting activation.
newtype ActivationKey = ActivationKey
    { fromActivationKey :: AsciiBase64Url }
    deriving (Eq, Show, FromByteString, ToByteString, FromJSON, ToJSON)

-- | A random code for use with an 'ActivationKey' that is usually transmitted
-- out-of-band, e.g. via email or sms.
newtype ActivationCode = ActivationCode
    { fromActivationCode :: AsciiBase64Url }
    deriving (Eq, Show, FromByteString, ToByteString, FromJSON, ToJSON)

-- | A pair of 'ActivationKey' and 'ActivationCode' as required for activation.
type ActivationPair = (ActivationKey, ActivationCode)

-- | Data for an activation request.
data Activate = Activate
    { activateTarget :: !ActivationTarget
    , activateCode   :: !ActivationCode
    , activateDryrun :: !Bool
    }

-- | The target of an activation request.
data ActivationTarget
    = ActivateKey !ActivationKey
        -- ^ An opaque key for some email or phone number awaiting activation.
    | ActivatePhone !Phone
        -- ^ A known phone number awaiting activation.
    | ActivateEmail !Email
        -- ^ A known email address awaiting activation.

instance ToByteString ActivationTarget where
    builder (ActivateKey   k) = builder k
    builder (ActivateEmail e) = builder e
    builder (ActivatePhone p) = builder p

-- | Information returned as part of a successful activation.
data ActivationResponse = ActivationResponse
    { activatedIdentity :: !UserIdentity
        -- ^ The activated / verified user identity.
    , activatedFirst :: !Bool
        -- ^ Whether this is the first verified identity of the account.
    }

-- | Payload for a request to (re-)send an activation code
-- for a phone number or e-mail address. If a phone is used,
-- one can also request a call instead of SMS.
data SendActivationCode = SendActivationCode
    { saUserKey :: !(Either Email Phone)
    , saLocale  :: !(Maybe Locale)
    , saCall    :: !Bool
    }

-- * JSON Instances:

instance FromJSON SendActivationCode where
    parseJSON = withObject "SendActivationCode" $ \o -> do
        e <- o .:? "email"
        p <- o .:? "phone"
        SendActivationCode <$> key e p
                           <*> o .:? "locale"
                           <*> o .:? "voice_call" .!= False
      where
        key (Just _) (Just _) = fail "Only one of 'email' or 'phone' allowed."
        key Nothing  Nothing  = fail "One of 'email' or 'phone' required."
        key (Just e) Nothing  = return $ Left e
        key Nothing  (Just p) = return $ Right p

instance ToJSON ActivationResponse where
    toJSON (ActivationResponse ident first) = object
        $ "email"  .= emailIdentity ident
        # "phone"  .= phoneIdentity ident
        # "first"  .= first
        # []

instance FromJSON ActivationResponse where
    parseJSON = withObject "ActivationResponse" $ \o ->
        ActivationResponse <$> parseJSON (Object o)
                           <*> o .:? "first" .!= False

instance FromJSON Activate where
    parseJSON = withObject "Activation" $ \o ->
        Activate <$> key o
                 <*> o .:  "code"
                 <*> o .:? "dryrun" .!= False
      where
        key o =  (ActivateKey   <$> o .: "key")
             <|> (ActivateEmail <$> o .: "email")
             <|> (ActivatePhone <$> o .: "phone")

instance ToJSON Activate where
    toJSON (Activate k c d) = object
        [ key k, "code" .= c, "dryrun" .= d ]
      where
        key (ActivateKey  ak) = "key"   .= ak
        key (ActivateEmail e) = "email" .= e
        key (ActivatePhone p) = "phone" .= p

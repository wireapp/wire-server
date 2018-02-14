{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Galley.Data.Types
    ( Conversation (..)
    , isSelfConv
    , isO2OConv
    , isTeamConv
    , isConvDeleted
    , selfConv
    , Code (..)
    , Scope (..)
    , toCode
    , generate
    , mkKey
    ) where

import Brig.Types.Code
import Data.ByteString.Conversion
import Control.Monad.IO.Class
import Cassandra hiding (Value)
import Data.Int (Int32)
import Data.Id
import Data.Range
import Data.Text
import Data.Maybe (fromMaybe, isJust)
import Galley.Types (ConvType (..), Access, Member (..))
import OpenSSL.Random (randBytes)
import OpenSSL.EVP.Digest (getDigestByName, digestBS)

import qualified Data.Text.Ascii as Ascii
import qualified Data.ByteString as BS

data Conversation = Conversation
    { convId      :: ConvId
    , convType    :: ConvType
    , convCreator :: UserId
    , convName    :: Maybe Text
    , convAccess  :: [Access]
    , convMembers :: [Member]
    , convTeam    :: Maybe TeamId
    , convDeleted :: Maybe Bool
    } deriving (Eq, Show)

isSelfConv :: Conversation -> Bool
isSelfConv = (SelfConv ==) . convType

isO2OConv :: Conversation -> Bool
isO2OConv = (One2OneConv ==) . convType

isTeamConv :: Conversation -> Bool
isTeamConv = isJust . convTeam

isConvDeleted :: Conversation -> Bool
isConvDeleted = fromMaybe False . convDeleted

selfConv :: UserId -> ConvId
selfConv uid = Id (toUUID uid)

--------------------------------------------------------------------------------
-- Code

data Code = Code
    { codeKey           :: !Key
    , codeValue         :: !Value
    , codeTTL           :: !Timeout
    , codeConversation  :: !ConvId
    , codeScope         :: !Scope
    } deriving (Eq, Show)


data Scope = ReusableCode
    deriving (Eq, Show)

instance Cql Scope where
    ctype = Tagged IntColumn

    toCql ReusableCode = CqlInt 1

    fromCql (CqlInt 1) = return ReusableCode
    fromCql _          = fail "unknown Scope"

toCode :: Key -> Scope -> (Value, Int32, ConvId) -> Code
toCode k s (val, ttl, cnv) = Code
        { codeKey = k
        , codeValue = val
        , codeTTL = Timeout (fromIntegral ttl)
        , codeConversation = cnv
        , codeScope = s
        }

-- Note on key/value used for a conversation Code
--
-- For similar reasons to those given for Codes used for verification, Password reset, etc
-- (see services/brig/src/Brig/Code.hs Note [Unique keys])
-- The 'key' is a stable, truncated, base64 encoded sha256 hash of the conversation ID
-- The 'value' is a base64 encoded, 120-bit random value (changing on each generation)

generate :: MonadIO m => ConvId -> Scope -> Timeout -> m Code
generate cnv s t = do
    key <- mkKey cnv
    val <- liftIO $ Value . unsafeRange . Ascii.encodeBase64Url <$> randBytes 15
    return Code
        { codeKey = key
        , codeValue = val
        , codeConversation = cnv
        , codeTTL = t
        , codeScope = s
        }

mkKey :: MonadIO m => ConvId -> m Key
mkKey cnv = do
    Just sha256 <- liftIO $ getDigestByName "SHA256"
    return $ Key . unsafeRange. Ascii.encodeBase64Url . BS.take 15 $ digestBS sha256 (toByteString' cnv)

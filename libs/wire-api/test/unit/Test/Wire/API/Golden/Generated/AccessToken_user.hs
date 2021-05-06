{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.AccessToken_user where

import Codec.MIME.Type (Type(..))
import qualified Codec.MIME.Type as MIME
import Control.Lens ((.~))
import Data.Code
import Data.Coerce
import Data.Currency
import Data.Domain
import Data.Handle
import Data.Id
import Data.ISO3166_CountryCodes
import Data.Json.Util
import Data.List1
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import Data.Misc
import Data.PEM
import Data.Qualified
import Data.Range (unsafeRange)
import qualified Data.Set as Set
import Data.Text.Ascii
import Data.Time (secondsToNominalDiffTime)
import Imports hiding (LT, GT)
import qualified Data.LanguageCodes
import qualified Data.UUID as UUID
import Test.Tasty (testGroup, TestTree)
import URI.ByteString
import qualified Wire.API.Call.Config as CallConfig
import qualified Wire.API.User.Profile as User.Profile
import qualified Wire.API.Team.Conversation as Team.Conversation
import qualified Wire.API.Provider as Provider
import qualified Wire.API.Provider.Bot as Provider
import qualified Wire.API.Provider.External as Provider
import qualified Wire.API.Provider.Service as Provider
import qualified Wire.API.Provider.Service.Tag as Provider
import Data.Aeson
import GHC.Exts
import Wire.API.Asset
import Wire.API.Asset.V3.Resumable
import Wire.API.Call.Config
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Conversation.Bot
import Wire.API.Conversation.Code
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.CustomBackend
import Wire.API.Event.Conversation
import Wire.API.Message
import Wire.API.Notification (QueuedNotification, queuedNotification, QueuedNotificationList, queuedNotificationList)
import Wire.API.Properties
-- import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
-- import Wire.API.Provider.Service.Tag
import Wire.API.Push.Token hiding (Transport)
import qualified Wire.API.Push.Token as Push.Token
import Wire.API.Team
import Wire.API.Team.Role
-- import Wire.API.Team.SearchVisibility
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.Handle
import Wire.API.User.Identity
import Wire.API.User.Password
import Wire.API.User.Profile
import Wire.API.User.RichInfo
import Wire.API.User.Search
import Wire.API.Wrapped
testObject_AccessToken_user_1 :: AccessToken
testObject_AccessToken_user_1 = AccessToken {user = (Id (fromJust (UUID.fromString "0000257f-0000-384a-0000-64e1000062c8"))), access = "V\SUBVkhp\NULc\240\163\179\191%\229\168\129*A\228\187\149!_S", tokenType = Bearer, expiresIn = 20}
testObject_AccessToken_user_2 :: AccessToken
testObject_AccessToken_user_2 = AccessToken {user = (Id (fromJust (UUID.fromString "0000623f-0000-6767-0000-2dd400002e4d"))), access = "y\227\129\168\240\163\189\160\243\180\168\167~\a\\\240\165\138\168\240\164\167\131\234\158\174EJ] ~\243\189\190\173A\ETB\244\129\165\169R\EOT", tokenType = Bearer, expiresIn = 27}
testObject_AccessToken_user_3 :: AccessToken
testObject_AccessToken_user_3 = AccessToken {user = (Id (fromJust (UUID.fromString "00000791-0000-371f-0000-111c00005eda"))), access = "\SYNS\244\143\141\170\244\139\181\177QM\ETBAI{", tokenType = Bearer, expiresIn = 27}
testObject_AccessToken_user_4 :: AccessToken
testObject_AccessToken_user_4 = AccessToken {user = (Id (fromJust (UUID.fromString "000058db-0000-6854-0000-4c5800004130"))), access = "\244\129\164\179\SI\DC2x\SUB\240\172\187\178#KV\a$\r\NUL\SOH\243\182\164\134p\r*OTBf", tokenType = Bearer, expiresIn = -13}
testObject_AccessToken_user_5 :: AccessToken
testObject_AccessToken_user_5 = AccessToken {user = (Id (fromJust (UUID.fromString "00006001-0000-7727-0000-7f3400006154"))), access = "P\ENQH&\240\174\150\188", tokenType = Bearer, expiresIn = 14}
testObject_AccessToken_user_6 :: AccessToken
testObject_AccessToken_user_6 = AccessToken {user = (Id (fromJust (UUID.fromString "000037c8-0000-20c1-0000-19e600004d36"))), access = "%\244\132\139\170\&5\240\166\181\179\244\139\188\168^\nu\EOTn\243\183\184\151\&8\240\161\166\137\239\189\147\238\152\189\240\173\174\137\243\184\144\157#\244\129\165\138,S", tokenType = Bearer, expiresIn = -7}
testObject_AccessToken_user_7 :: AccessToken
testObject_AccessToken_user_7 = AccessToken {user = (Id (fromJust (UUID.fromString "00001722-0000-54fb-0000-76d5000004cd"))), access = "\244\131\131\155\243\183\134\146\ETB\t\240\157\134\179P\244\132\186\135\ACK\243\179\148\190\ETBR\234\128\175\244\140\159\155\n'\228\141\137J,`p", tokenType = Bearer, expiresIn = -24}
testObject_AccessToken_user_8 :: AccessToken
testObject_AccessToken_user_8 = AccessToken {user = (Id (fromJust (UUID.fromString "00000b35-0000-0dde-0000-59980000662b"))), access = "QB|\240\161\190\144>pj3jv\SYN')T\ACKk\"", tokenType = Bearer, expiresIn = -18}
testObject_AccessToken_user_9 :: AccessToken
testObject_AccessToken_user_9 = AccessToken {user = (Id (fromJust (UUID.fromString "00006a80-0000-0102-0000-544400006095"))), access = "0\ETB\tf\ENQ/+\ACK\NUL", tokenType = Bearer, expiresIn = 15}
testObject_AccessToken_user_10 :: AccessToken
testObject_AccessToken_user_10 = AccessToken {user = (Id (fromJust (UUID.fromString "00007712-0000-2e38-0000-7b090000112b"))), access = "\"9\SOp@t", tokenType = Bearer, expiresIn = 28}
testObject_AccessToken_user_11 :: AccessToken
testObject_AccessToken_user_11 = AccessToken {user = (Id (fromJust (UUID.fromString "00000239-0000-594b-0000-65ee0000766f"))), access = "\243\191\191\185\244\133\172\128", tokenType = Bearer, expiresIn = 26}
testObject_AccessToken_user_12 :: AccessToken
testObject_AccessToken_user_12 = AccessToken {user = (Id (fromJust (UUID.fromString "000035e0-0000-7ac8-0000-76e80000280d"))), access = "H`", tokenType = Bearer, expiresIn = 30}
testObject_AccessToken_user_13 :: AccessToken
testObject_AccessToken_user_13 = AccessToken {user = (Id (fromJust (UUID.fromString "0000305a-0000-258f-0000-7dd60000406d"))), access = "G0\244\140\188\185\240\159\158\158\240\151\142\189F\fa\243\185\184\160\SUB\240\173\139\183Fbi&RW\DELU\240\172\173\157E\EM\239\191\187\DC4\244\137\134\164", tokenType = Bearer, expiresIn = 6}
testObject_AccessToken_user_14 :: AccessToken
testObject_AccessToken_user_14 = AccessToken {user = (Id (fromJust (UUID.fromString "00004251-0000-2afb-0000-766f00003384"))), access = "H\ETBe\STX\FS\243\186\129\149\t+\240\162\150\186\"$\240\174\132\169\243\181\186\176\244\141\128\175I(\a#Yi~\240\160\137\130Y\243\176\148\187p\SUBs", tokenType = Bearer, expiresIn = 22}
testObject_AccessToken_user_15 :: AccessToken
testObject_AccessToken_user_15 = AccessToken {user = (Id (fromJust (UUID.fromString "000040b4-0000-60db-0000-155300004998"))), access = " \ENQ0\231\147\180\244\132\183\159\RST\240\173\189\139\GS\240\164\188\177Wy\SUB=\240\162\174\172", tokenType = Bearer, expiresIn = -13}
testObject_AccessToken_user_16 :: AccessToken
testObject_AccessToken_user_16 = AccessToken {user = (Id (fromJust (UUID.fromString "000054ab-0000-0bf8-0000-04e90000050e"))), access = "p\244\132\141\165\DC4>%\EOT\ENQ\SO\240\161\168\156\244\142\151\142D\EM~Ejl\240\167\130\150q", tokenType = Bearer, expiresIn = -19}
testObject_AccessToken_user_17 :: AccessToken
testObject_AccessToken_user_17 = AccessToken {user = (Id (fromJust (UUID.fromString "00007d74-0000-08df-0000-3dde000067ac"))), access = "K\GS\STX\rty<+(+s7\244\143\159\168nV\244\128\183\171I}/\SOHbo\240\162\162\168\240\168\140\172`6", tokenType = Bearer, expiresIn = 29}
testObject_AccessToken_user_18 :: AccessToken
testObject_AccessToken_user_18 = AccessToken {user = (Id (fromJust (UUID.fromString "000036e3-0000-3358-0000-2a7000006fc1"))), access = "?>\244\133\183\162K\243\189\157\153\ESCm5>QGEE\244\141\179\158R=F", tokenType = Bearer, expiresIn = 2}
testObject_AccessToken_user_19 :: AccessToken
testObject_AccessToken_user_19 = AccessToken {user = (Id (fromJust (UUID.fromString "00000694-0000-1c3d-0000-225d0000046d"))), access = "\nv\243\180\137\169l{=\243\182\129\171\235\175\176x\243\178\182\151\DLEr\232\175\144\240\162\169\154F\DELmo<_b\GS3\243\189\149\149\244\143\190\191", tokenType = Bearer, expiresIn = 21}
testObject_AccessToken_user_20 :: AccessToken
testObject_AccessToken_user_20 = AccessToken {user = (Id (fromJust (UUID.fromString "00002296-0000-301a-0000-5878000076f9"))), access = ")m\243\191\154\157\&4%\ESCKA,\ACK>f6#\EMnt]O\ESC5\ETXG6", tokenType = Bearer, expiresIn = -26}

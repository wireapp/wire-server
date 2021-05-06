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
testObject_AccessToken_user_1 = AccessToken {user = (Id (fromJust (UUID.fromString "00005318-0000-0da4-0000-22ed00005817"))), access = "o\SO\243\182\152\139\f\243\182\135\182\230\166\186J;?C\"S5\240\170\146\146\240\163\176\152iIV\FS\GS\231\178\174u\244\141\189\135p*\226\175\142D\240\158\162\169a\DEL", tokenType = Bearer, expiresIn = 25}
testObject_AccessToken_user_2 :: AccessToken
testObject_AccessToken_user_2 = AccessToken {user = (Id (fromJust (UUID.fromString "00007bd6-0000-24c9-0000-2e510000793b"))), access = "\US\229\175\136\240\163\144\128IJ+\\\ENQ\240\162\174\183\233\162\163\STX\244\141\183\189DnR", tokenType = Bearer, expiresIn = -8}
testObject_AccessToken_user_3 :: AccessToken
testObject_AccessToken_user_3 = AccessToken {user = (Id (fromJust (UUID.fromString "0000335a-0000-4867-0000-19aa00001d84"))), access = "\243\182\168\174\236\162\188\aG#yt", tokenType = Bearer, expiresIn = 24}
testObject_AccessToken_user_4 :: AccessToken
testObject_AccessToken_user_4 = AccessToken {user = (Id (fromJust (UUID.fromString "00003b13-0000-3354-0000-2315000056bb"))), access = "\244\129\136\173\243\188\163\131:,\229\128\170\227\160\157F\240\161\177\131\\\a\240\163\177\143\&0\SI\244\129\138\130N", tokenType = Bearer, expiresIn = -14}
testObject_AccessToken_user_5 :: AccessToken
testObject_AccessToken_user_5 = AccessToken {user = (Id (fromJust (UUID.fromString "00006322-0000-4fb4-0000-78700000630e"))), access = "O0\244\141\143\136\EOTl\243\185\140\182\ESCRAz\240\163\149\135\244\130\156\140b\243\184\150\187I\244\140\191\191", tokenType = Bearer, expiresIn = -7}
testObject_AccessToken_user_6 :: AccessToken
testObject_AccessToken_user_6 = AccessToken {user = (Id (fromJust (UUID.fromString "00004761-0000-7168-0000-3853000001e5"))), access = "\ENQt$B\243\184\160\142a\US\DC3\240\173\135\131$\229\142\187l7!\NAKz\a\"w4,2\240\172\152\185\240\173\131\147\236\135\146|x", tokenType = Bearer, expiresIn = 13}
testObject_AccessToken_user_7 :: AccessToken
testObject_AccessToken_user_7 = AccessToken {user = (Id (fromJust (UUID.fromString "00006b66-0000-0369-0000-7d5200000dbd"))), access = "u\240\171\175\151\NAKe\SUBL\GS7i@?9\CANQEwIW\DLE\227\129\171\NUL", tokenType = Bearer, expiresIn = 16}
testObject_AccessToken_user_8 :: AccessToken
testObject_AccessToken_user_8 = AccessToken {user = (Id (fromJust (UUID.fromString "00003b5a-0000-4bc0-0000-419200001860"))), access = "\SOH\DC2r\243\185\151\152f\243\176\177\185\\2\ACK\243\180\162\131\240\161\160\190\240\160\184\155\243\182\148\144\243\188\184\140\SUB\240\168\173\143\225\154\145l2", tokenType = Bearer, expiresIn = -6}
testObject_AccessToken_user_9 :: AccessToken
testObject_AccessToken_user_9 = AccessToken {user = (Id (fromJust (UUID.fromString "00000f83-0000-6863-0000-07f800000767"))), access = "\244\142\136\184Ni.e\EM\240\144\133\166r^7K\ETBfr\243\179\172\139\DELl\DC4,\244\128\156\162\230\186\175", tokenType = Bearer, expiresIn = -3}
testObject_AccessToken_user_10 :: AccessToken
testObject_AccessToken_user_10 = AccessToken {user = (Id (fromJust (UUID.fromString "00003811-0000-07da-0000-47fb0000660a"))), access = "", tokenType = Bearer, expiresIn = -4}
testObject_AccessToken_user_11 :: AccessToken
testObject_AccessToken_user_11 = AccessToken {user = (Id (fromJust (UUID.fromString "00003011-0000-52b3-0000-04a2000011f6"))), access = "\240\159\128\179\240\164\135\146'\240\172\191\165\\u<W\240\170\148\145==\SO2\239\174\185'\231\151\139", tokenType = Bearer, expiresIn = 23}
testObject_AccessToken_user_12 :: AccessToken
testObject_AccessToken_user_12 = AccessToken {user = (Id (fromJust (UUID.fromString "00002254-0000-580f-0000-120e000067f0"))), access = "/\240\172\159\183\240\166\143\191a3\240\166\153\182[x\CAN\232\180\143\230\174\160|\NUL\GS\243\180\168\174$\t\240\173\133\164Fqd*\243\186\168\180\240\172\137\132lE", tokenType = Bearer, expiresIn = 29}
testObject_AccessToken_user_13 :: AccessToken
testObject_AccessToken_user_13 = AccessToken {user = (Id (fromJust (UUID.fromString "000044c5-0000-6722-0000-600900001885"))), access = "Ya\DELi8,\243\178\163\130=C\240\163\155\156\&9I{\DEL\EMSeW'@T", tokenType = Bearer, expiresIn = 23}
testObject_AccessToken_user_14 :: AccessToken
testObject_AccessToken_user_14 = AccessToken {user = (Id (fromJust (UUID.fromString "0000292f-0000-7fbf-0000-487200002018"))), access = "7\231\133\158M\244\136\159\132\DLE\SO<,q}\RS\f\SOHi;z\ESClgPE(a\DC2:D", tokenType = Bearer, expiresIn = 13}
testObject_AccessToken_user_15 :: AccessToken
testObject_AccessToken_user_15 = AccessToken {user = (Id (fromJust (UUID.fromString "0000284c-0000-4c9a-0000-5e6000003d34"))), access = "$W\238\167\184_R\ENQ&)\228\168\157", tokenType = Bearer, expiresIn = 20}
testObject_AccessToken_user_16 :: AccessToken
testObject_AccessToken_user_16 = AccessToken {user = (Id (fromJust (UUID.fromString "00002ede-0000-2f1d-0000-26b500000291"))), access = "\DLE\244\131\170\177=\240\163\131\189", tokenType = Bearer, expiresIn = 13}
testObject_AccessToken_user_17 :: AccessToken
testObject_AccessToken_user_17 = AccessToken {user = (Id (fromJust (UUID.fromString "00006078-0000-0b65-0000-2b60000010fe"))), access = "\240\166\184\167\225\142\148XVD%:M\240\169\175\168V\229\152\129z\NULy*O\240\161\155\175\231\144\139\DC4\RSb\FS\243\178\173\171W\DC4\243\176\176\170F", tokenType = Bearer, expiresIn = 10}
testObject_AccessToken_user_18 :: AccessToken
testObject_AccessToken_user_18 = AccessToken {user = (Id (fromJust (UUID.fromString "000002b5-0000-116b-0000-67ab00001e17"))), access = "sMNZwx083\240\163\135\180", tokenType = Bearer, expiresIn = 4}
testObject_AccessToken_user_19 :: AccessToken
testObject_AccessToken_user_19 = AccessToken {user = (Id (fromJust (UUID.fromString "0000138c-0000-2602-0000-5bdc0000381c"))), access = "", tokenType = Bearer, expiresIn = 15}
testObject_AccessToken_user_20 :: AccessToken
testObject_AccessToken_user_20 = AccessToken {user = (Id (fromJust (UUID.fromString "00003b60-0000-06df-0000-00ad00002e0c"))), access = "\f\240\160\190\157qx\t\229\140\149h\244\137\189\147\240\174\165\188S /\240\174\168\176\244\132\129\147'\234\146\153{!\GS\DC4\240\157\170\175", tokenType = Bearer, expiresIn = 5}

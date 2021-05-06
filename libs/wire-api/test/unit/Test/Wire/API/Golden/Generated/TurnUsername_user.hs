{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.TurnUsername_user where

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
testObject_TurnUsername_user_1 :: TurnUsername
testObject_TurnUsername_user_1 = (turnUsername (secondsToNominalDiffTime (4863074.000000000000)) ("y5m4") & tuVersion .~ (1) & tuKeyindex .~ (9516) & tuT .~ ('\42151'))
testObject_TurnUsername_user_2 :: TurnUsername
testObject_TurnUsername_user_2 = (turnUsername (secondsToNominalDiffTime (10273286.000000000000)) ("c4q0qdtjee5v993ln7617g6") & tuVersion .~ (23) & tuKeyindex .~ (12977) & tuT .~ ('\26258'))
testObject_TurnUsername_user_3 :: TurnUsername
testObject_TurnUsername_user_3 = (turnUsername (secondsToNominalDiffTime (3040006.000000000000)) ("0tnivin96j9uww5s1po1") & tuVersion .~ (23) & tuKeyindex .~ (14535) & tuT .~ ('v'))
testObject_TurnUsername_user_4 :: TurnUsername
testObject_TurnUsername_user_4 = (turnUsername (secondsToNominalDiffTime (7599283.000000000000)) ("yq6vqeeinznpmt29hd16bymb076d") & tuVersion .~ (3) & tuKeyindex .~ (31643) & tuT .~ ('\1000786'))
testObject_TurnUsername_user_5 :: TurnUsername
testObject_TurnUsername_user_5 = (turnUsername (secondsToNominalDiffTime (1002651.000000000000)) ("7gttdhfy") & tuVersion .~ (15) & tuKeyindex .~ (21682) & tuT .~ ('\CAN'))
testObject_TurnUsername_user_6 :: TurnUsername
testObject_TurnUsername_user_6 = (turnUsername (secondsToNominalDiffTime (3010100.000000000000)) ("py7e03zanf78") & tuVersion .~ (6) & tuKeyindex .~ (30619) & tuT .~ ('\3299'))
testObject_TurnUsername_user_7 :: TurnUsername
testObject_TurnUsername_user_7 = (turnUsername (secondsToNominalDiffTime (4146354.000000000000)) ("oxk150rhsgo69t06398ue9s") & tuVersion .~ (27) & tuKeyindex .~ (31900) & tuT .~ ('K'))
testObject_TurnUsername_user_8 :: TurnUsername
testObject_TurnUsername_user_8 = (turnUsername (secondsToNominalDiffTime (10660316.000000000000)) ("oy7njw0foh7zewm7hq1ygl4") & tuVersion .~ (25) & tuKeyindex .~ (23597) & tuT .~ ('+'))
testObject_TurnUsername_user_9 :: TurnUsername
testObject_TurnUsername_user_9 = (turnUsername (secondsToNominalDiffTime (33058.000000000000)) ("m6qrcoav6ltfrp7e2hl9546zm1n") & tuVersion .~ (17) & tuKeyindex .~ (19376) & tuT .~ ('I'))
testObject_TurnUsername_user_10 :: TurnUsername
testObject_TurnUsername_user_10 = (turnUsername (secondsToNominalDiffTime (13092488.000000000000)) ("62h1hm5iab1zgo5k") & tuVersion .~ (4) & tuKeyindex .~ (1601) & tuT .~ ('\19095'))
testObject_TurnUsername_user_11 :: TurnUsername
testObject_TurnUsername_user_11 = (turnUsername (secondsToNominalDiffTime (16033933.000000000000)) ("07fomzs6u1men5zsil2ckk8j54p") & tuVersion .~ (19) & tuKeyindex .~ (14478) & tuT .~ ('\NAK'))
testObject_TurnUsername_user_12 :: TurnUsername
testObject_TurnUsername_user_12 = (turnUsername (secondsToNominalDiffTime (15181107.000000000000)) ("2h2chni2pdpjpj48t") & tuVersion .~ (25) & tuKeyindex .~ (20080) & tuT .~ ('G'))
testObject_TurnUsername_user_13 :: TurnUsername
testObject_TurnUsername_user_13 = (turnUsername (secondsToNominalDiffTime (11221417.000000000000)) ("2wkx6yjbtdtltrkihdjakxv6") & tuVersion .~ (2) & tuKeyindex .~ (27551) & tuT .~ ('\1008547'))
testObject_TurnUsername_user_14 :: TurnUsername
testObject_TurnUsername_user_14 = (turnUsername (secondsToNominalDiffTime (15779099.000000000000)) ("zw") & tuVersion .~ (17) & tuKeyindex .~ (13112) & tuT .~ ('\DEL'))
testObject_TurnUsername_user_15 :: TurnUsername
testObject_TurnUsername_user_15 = (turnUsername (secondsToNominalDiffTime (14900328.000000000000)) ("bn52g9") & tuVersion .~ (3) & tuKeyindex .~ (5731) & tuT .~ ('Z'))
testObject_TurnUsername_user_16 :: TurnUsername
testObject_TurnUsername_user_16 = (turnUsername (secondsToNominalDiffTime (15274995.000000000000)) ("uw654z40le") & tuVersion .~ (4) & tuKeyindex .~ (23817) & tuT .~ ('\1057777'))
testObject_TurnUsername_user_17 :: TurnUsername
testObject_TurnUsername_user_17 = (turnUsername (secondsToNominalDiffTime (8568769.000000000000)) ("2iqwv652e7vfmwbwhfs") & tuVersion .~ (9) & tuKeyindex .~ (5841) & tuT .~ ('@'))
testObject_TurnUsername_user_18 :: TurnUsername
testObject_TurnUsername_user_18 = (turnUsername (secondsToNominalDiffTime (11007613.000000000000)) ("6pcha6w32big8x5au") & tuVersion .~ (21) & tuKeyindex .~ (21654) & tuT .~ ('\DC4'))
testObject_TurnUsername_user_19 :: TurnUsername
testObject_TurnUsername_user_19 = (turnUsername (secondsToNominalDiffTime (2698363.000000000000)) ("j5juf8cund") & tuVersion .~ (18) & tuKeyindex .~ (18771) & tuT .~ ('c'))
testObject_TurnUsername_user_20 :: TurnUsername
testObject_TurnUsername_user_20 = (turnUsername (secondsToNominalDiffTime (6958298.000000000000)) ("2w7x") & tuVersion .~ (9) & tuKeyindex .~ (28153) & tuT .~ ('\SO'))

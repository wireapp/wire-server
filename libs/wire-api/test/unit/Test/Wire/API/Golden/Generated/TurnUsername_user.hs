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
testObject_TurnUsername_user_1 = (turnUsername (secondsToNominalDiffTime (13726182.000000000000)) ("natklrzjkfdj5iw") & tuVersion .~ (28) & tuKeyindex .~ (12666) & tuT .~ ('K'))
testObject_TurnUsername_user_2 :: TurnUsername
testObject_TurnUsername_user_2 = (turnUsername (secondsToNominalDiffTime (14758759.000000000000)) ("t3ubqtj") & tuVersion .~ (16) & tuKeyindex .~ (24248) & tuT .~ ('\1107698'))
testObject_TurnUsername_user_3 :: TurnUsername
testObject_TurnUsername_user_3 = (turnUsername (secondsToNominalDiffTime (7063470.000000000000)) ("73x5y2ygy4o7qaamtxd65") & tuVersion .~ (24) & tuKeyindex .~ (26949) & tuT .~ ('\1018234'))
testObject_TurnUsername_user_4 :: TurnUsername
testObject_TurnUsername_user_4 = (turnUsername (secondsToNominalDiffTime (11730111.000000000000)) ("d4fknghdf6ie6dp9") & tuVersion .~ (25) & tuKeyindex .~ (20076) & tuT .~ ('/'))
testObject_TurnUsername_user_5 :: TurnUsername
testObject_TurnUsername_user_5 = (turnUsername (secondsToNominalDiffTime (1043226.000000000000)) ("kijjeot3340vzhu6oqxobfpu5xu382") & tuVersion .~ (26) & tuKeyindex .~ (3568) & tuT .~ ('\1101505'))
testObject_TurnUsername_user_6 :: TurnUsername
testObject_TurnUsername_user_6 = (turnUsername (secondsToNominalDiffTime (4951484.000000000000)) ("rrwfr44jc327u49hfux85") & tuVersion .~ (21) & tuKeyindex .~ (30322) & tuT .~ ('8'))
testObject_TurnUsername_user_7 :: TurnUsername
testObject_TurnUsername_user_7 = (turnUsername (secondsToNominalDiffTime (12267345.000000000000)) ("0oadau1d2byvdj") & tuVersion .~ (8) & tuKeyindex .~ (8986) & tuT .~ ('"'))
testObject_TurnUsername_user_8 :: TurnUsername
testObject_TurnUsername_user_8 = (turnUsername (secondsToNominalDiffTime (15269085.000000000000)) ("mtzquu34") & tuVersion .~ (11) & tuKeyindex .~ (1729) & tuT .~ ('\48709'))
testObject_TurnUsername_user_9 :: TurnUsername
testObject_TurnUsername_user_9 = (turnUsername (secondsToNominalDiffTime (3829249.000000000000)) ("9y1lhb3y40e4lc3von8izwg") & tuVersion .~ (1) & tuKeyindex .~ (14431) & tuT .~ ('-'))
testObject_TurnUsername_user_10 :: TurnUsername
testObject_TurnUsername_user_10 = (turnUsername (secondsToNominalDiffTime (12668204.000000000000)) ("v9rl8m2slwzknm") & tuVersion .~ (24) & tuKeyindex .~ (28457) & tuT .~ ('N'))
testObject_TurnUsername_user_11 :: TurnUsername
testObject_TurnUsername_user_11 = (turnUsername (secondsToNominalDiffTime (7357509.000000000000)) ("2h") & tuVersion .~ (28) & tuKeyindex .~ (40) & tuT .~ ('\1086656'))
testObject_TurnUsername_user_12 :: TurnUsername
testObject_TurnUsername_user_12 = (turnUsername (secondsToNominalDiffTime (5507212.000000000000)) ("6ivkkyxdev") & tuVersion .~ (18) & tuKeyindex .~ (2016) & tuT .~ ('c'))
testObject_TurnUsername_user_13 :: TurnUsername
testObject_TurnUsername_user_13 = (turnUsername (secondsToNominalDiffTime (6376037.000000000000)) ("izsgbxi87sk2enrdsfq6o") & tuVersion .~ (23) & tuKeyindex .~ (30736) & tuT .~ ('\1071729'))
testObject_TurnUsername_user_14 :: TurnUsername
testObject_TurnUsername_user_14 = (turnUsername (secondsToNominalDiffTime (14800174.000000000000)) ("xi4kiqsp8enw") & tuVersion .~ (19) & tuKeyindex .~ (15085) & tuT .~ ('a'))
testObject_TurnUsername_user_15 :: TurnUsername
testObject_TurnUsername_user_15 = (turnUsername (secondsToNominalDiffTime (6158194.000000000000)) ("qpu85lt9w2se8fgq3ci") & tuVersion .~ (11) & tuKeyindex .~ (2697) & tuT .~ ('\183774'))
testObject_TurnUsername_user_16 :: TurnUsername
testObject_TurnUsername_user_16 = (turnUsername (secondsToNominalDiffTime (6956274.000000000000)) ("x9blkk8c7nv7a5pogaiv") & tuVersion .~ (9) & tuKeyindex .~ (32488) & tuT .~ ('6'))
testObject_TurnUsername_user_17 :: TurnUsername
testObject_TurnUsername_user_17 = (turnUsername (secondsToNominalDiffTime (1786985.000000000000)) ("06b5") & tuVersion .~ (0) & tuKeyindex .~ (4092) & tuT .~ ('='))
testObject_TurnUsername_user_18 :: TurnUsername
testObject_TurnUsername_user_18 = (turnUsername (secondsToNominalDiffTime (5444472.000000000000)) ("gzay90qebb7mz36pxt3dxzv4x") & tuVersion .~ (8) & tuKeyindex .~ (14980) & tuT .~ ('C'))
testObject_TurnUsername_user_19 :: TurnUsername
testObject_TurnUsername_user_19 = (turnUsername (secondsToNominalDiffTime (12571319.000000000000)) ("s7tm007piq5jnx") & tuVersion .~ (3) & tuKeyindex .~ (14747) & tuT .~ ('c'))
testObject_TurnUsername_user_20 :: TurnUsername
testObject_TurnUsername_user_20 = (turnUsername (secondsToNominalDiffTime (16007796.000000000000)) ("015ihx2m") & tuVersion .~ (30) & tuKeyindex .~ (18256) & tuT .~ ('9'))

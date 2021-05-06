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
testObject_TurnUsername_user_1 = (turnUsername (secondsToNominalDiffTime (4197303.000000000000)) ("1hl4sqlo8we80") & tuVersion .~ (1) & tuKeyindex .~ (21717) & tuT .~ ('1'))
testObject_TurnUsername_user_2 :: TurnUsername
testObject_TurnUsername_user_2 = (turnUsername (secondsToNominalDiffTime (16181750.000000000000)) ("bky") & tuVersion .~ (1) & tuKeyindex .~ (19659) & tuT .~ ('\DLE'))
testObject_TurnUsername_user_3 :: TurnUsername
testObject_TurnUsername_user_3 = (turnUsername (secondsToNominalDiffTime (367077.000000000000)) ("dtfjrr73gn8knf") & tuVersion .~ (13) & tuKeyindex .~ (19922) & tuT .~ ('\NAK'))
testObject_TurnUsername_user_4 :: TurnUsername
testObject_TurnUsername_user_4 = (turnUsername (secondsToNominalDiffTime (13290418.000000000000)) ("6y") & tuVersion .~ (16) & tuKeyindex .~ (11066) & tuT .~ ('\1049277'))
testObject_TurnUsername_user_5 :: TurnUsername
testObject_TurnUsername_user_5 = (turnUsername (secondsToNominalDiffTime (1119840.000000000000)) ("y9uydimku8") & tuVersion .~ (2) & tuKeyindex .~ (2877) & tuT .~ ('\120103'))
testObject_TurnUsername_user_6 :: TurnUsername
testObject_TurnUsername_user_6 = (turnUsername (secondsToNominalDiffTime (3261783.000000000000)) ("3j1hcu") & tuVersion .~ (7) & tuKeyindex .~ (7513) & tuT .~ ('\46086'))
testObject_TurnUsername_user_7 :: TurnUsername
testObject_TurnUsername_user_7 = (turnUsername (secondsToNominalDiffTime (5953142.000000000000)) ("xklqtcn021zze20pl5geoayggv") & tuVersion .~ (3) & tuKeyindex .~ (12806) & tuT .~ ('\167955'))
testObject_TurnUsername_user_8 :: TurnUsername
testObject_TurnUsername_user_8 = (turnUsername (secondsToNominalDiffTime (14397233.000000000000)) ("mvdxrmsmpr6h6") & tuVersion .~ (23) & tuKeyindex .~ (932) & tuT .~ ('q'))
testObject_TurnUsername_user_9 :: TurnUsername
testObject_TurnUsername_user_9 = (turnUsername (secondsToNominalDiffTime (4917166.000000000000)) ("4um9r1g3w2") & tuVersion .~ (29) & tuKeyindex .~ (20724) & tuT .~ ('Q'))
testObject_TurnUsername_user_10 :: TurnUsername
testObject_TurnUsername_user_10 = (turnUsername (secondsToNominalDiffTime (5900072.000000000000)) ("sf7ks1") & tuVersion .~ (28) & tuKeyindex .~ (10631) & tuT .~ ('p'))
testObject_TurnUsername_user_11 :: TurnUsername
testObject_TurnUsername_user_11 = (turnUsername (secondsToNominalDiffTime (1380531.000000000000)) ("lv4qn82m40jh5jgfb") & tuVersion .~ (18) & tuKeyindex .~ (27130) & tuT .~ ('\17550'))
testObject_TurnUsername_user_12 :: TurnUsername
testObject_TurnUsername_user_12 = (turnUsername (secondsToNominalDiffTime (4727967.000000000000)) ("jjshf2zc4vqy") & tuVersion .~ (23) & tuKeyindex .~ (22295) & tuT .~ ('&'))
testObject_TurnUsername_user_13 :: TurnUsername
testObject_TurnUsername_user_13 = (turnUsername (secondsToNominalDiffTime (3483088.000000000000)) ("jgmyn63") & tuVersion .~ (11) & tuKeyindex .~ (7971) & tuT .~ ('\n'))
testObject_TurnUsername_user_14 :: TurnUsername
testObject_TurnUsername_user_14 = (turnUsername (secondsToNominalDiffTime (6354326.000000000000)) ("rm2w1mnzqwldjxbzrq40h9eqv4knju") & tuVersion .~ (21) & tuKeyindex .~ (10374) & tuT .~ ('O'))
testObject_TurnUsername_user_15 :: TurnUsername
testObject_TurnUsername_user_15 = (turnUsername (secondsToNominalDiffTime (6874703.000000000000)) ("5bpyrc4mmzbuh96f5cy3hi63nem") & tuVersion .~ (22) & tuKeyindex .~ (1614) & tuT .~ ('\17051'))
testObject_TurnUsername_user_16 :: TurnUsername
testObject_TurnUsername_user_16 = (turnUsername (secondsToNominalDiffTime (3826512.000000000000)) ("iqtuf6785bypa2vq3crmoed") & tuVersion .~ (22) & tuKeyindex .~ (4928) & tuT .~ ('\ACK'))
testObject_TurnUsername_user_17 :: TurnUsername
testObject_TurnUsername_user_17 = (turnUsername (secondsToNominalDiffTime (4291721.000000000000)) ("9sd291eeibtmgxtyk") & tuVersion .~ (17) & tuKeyindex .~ (7561) & tuT .~ ('\1073388'))
testObject_TurnUsername_user_18 :: TurnUsername
testObject_TurnUsername_user_18 = (turnUsername (secondsToNominalDiffTime (13330159.000000000000)) ("yhy3h2yct7cwlapc1ono") & tuVersion .~ (18) & tuKeyindex .~ (1467) & tuT .~ ('\t'))
testObject_TurnUsername_user_19 :: TurnUsername
testObject_TurnUsername_user_19 = (turnUsername (secondsToNominalDiffTime (12583948.000000000000)) ("lt6e68y") & tuVersion .~ (3) & tuKeyindex .~ (6095) & tuT .~ ('y'))
testObject_TurnUsername_user_20 :: TurnUsername
testObject_TurnUsername_user_20 = (turnUsername (secondsToNominalDiffTime (219339.000000000000)) ("14anip6luaypejunym2w") & tuVersion .~ (5) & tuKeyindex .~ (21971) & tuT .~ ('\1077865'))

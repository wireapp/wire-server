{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.MemberUpdate_user where

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
testObject_MemberUpdate_user_1 :: MemberUpdate
testObject_MemberUpdate_user_1 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just ":\"]", mupOtrArchive = Just True, mupOtrArchiveRef = Just "a=", mupHidden = Just True, mupHiddenRef = Nothing, mupConvRoleName = Just (fromJust (parseRoleName "ta8jdd8rclxemm6f3_a7rxu1emg0xhfczvl2l51gjekc8dblqm6n_5h6cv106jkeb8tjr8kh0ksxu25ok"))}
testObject_MemberUpdate_user_2 :: MemberUpdate
testObject_MemberUpdate_user_2 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "c", mupOtrArchive = Just True, mupOtrArchiveRef = Just "", mupHidden = Nothing, mupHiddenRef = Nothing, mupConvRoleName = Just (fromJust (parseRoleName "cz2fc46m2jof441kmm5_nszesas"))}
testObject_MemberUpdate_user_3 :: MemberUpdate
testObject_MemberUpdate_user_3 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "", mupOtrArchive = Nothing, mupOtrArchiveRef = Just "\b#\1021362", mupHidden = Just False, mupHiddenRef = Just "\SO\1100831", mupConvRoleName = Just (fromJust (parseRoleName "32ti3lf13a1v6sky_7x3gzidudwq5"))}
testObject_MemberUpdate_user_4 :: MemberUpdate
testObject_MemberUpdate_user_4 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "\138085", mupOtrArchive = Just False, mupOtrArchiveRef = Nothing, mupHidden = Just False, mupHiddenRef = Just "at:", mupConvRoleName = Just (fromJust (parseRoleName "9ud4wuu2dk7fk2db11033btwog1o_rc4r__fnbfb0xwn5b4t4g_3e88hegwsakgu32v_mml6dfa33wcqgr2wed6t75p5zjgsdgycp4oabmwrthhcp"))}
testObject_MemberUpdate_user_5 :: MemberUpdate
testObject_MemberUpdate_user_5 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Nothing, mupOtrArchive = Just False, mupOtrArchiveRef = Just "\164737", mupHidden = Just True, mupHiddenRef = Just "\FS\DLE2", mupConvRoleName = Nothing}
testObject_MemberUpdate_user_6 :: MemberUpdate
testObject_MemberUpdate_user_6 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "\US\98443`", mupOtrArchive = Just False, mupOtrArchiveRef = Just "\STXX5", mupHidden = Just False, mupHiddenRef = Nothing, mupConvRoleName = Just (fromJust (parseRoleName "cdvuvdpywk0njzy9_p3b6r6"))}
testObject_MemberUpdate_user_7 :: MemberUpdate
testObject_MemberUpdate_user_7 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Nothing, mupOtrArchive = Just True, mupOtrArchiveRef = Nothing, mupHidden = Just False, mupHiddenRef = Just "M\f\NAK", mupConvRoleName = Just (fromJust (parseRoleName "38i6zkv296m5d1xrutek49sob8_37914jk7tjy8s6gn89139kyih3ko7d35w9uwzroehwzl3ao8cfd1fmhivy"))}
testObject_MemberUpdate_user_8 :: MemberUpdate
testObject_MemberUpdate_user_8 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Nothing, mupOtrArchive = Just True, mupOtrArchiveRef = Just "\1097083", mupHidden = Nothing, mupHiddenRef = Just "\1110311", mupConvRoleName = Just (fromJust (parseRoleName "pu5u12z5rb21ublnxlx0"))}
testObject_MemberUpdate_user_9 :: MemberUpdate
testObject_MemberUpdate_user_9 = MemberUpdate {mupOtrMute = Nothing, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Nothing, mupOtrArchive = Just False, mupOtrArchiveRef = Just "~", mupHidden = Just True, mupHiddenRef = Just "i", mupConvRoleName = Nothing}
testObject_MemberUpdate_user_10 :: MemberUpdate
testObject_MemberUpdate_user_10 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "", mupOtrArchive = Just True, mupOtrArchiveRef = Nothing, mupHidden = Just False, mupHiddenRef = Just "", mupConvRoleName = Just (fromJust (parseRoleName "5y4mbsmuonqc_ygw6ghjkfrnpqwp_3yt0zpg1z3m4yr2yspwx6giinj85mythm8bxoz_ef0qmuwbfc0ycg3xwth5fuwjc6tb2dengwid9ch"))}
testObject_MemberUpdate_user_11 :: MemberUpdate
testObject_MemberUpdate_user_11 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "", mupOtrArchive = Just False, mupOtrArchiveRef = Just "\EM\NUL", mupHidden = Just False, mupHiddenRef = Nothing, mupConvRoleName = Nothing}
testObject_MemberUpdate_user_12 :: MemberUpdate
testObject_MemberUpdate_user_12 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "A\1044881", mupOtrArchive = Just False, mupOtrArchiveRef = Nothing, mupHidden = Just True, mupHiddenRef = Just "\SUBC5", mupConvRoleName = Just (fromJust (parseRoleName "645ll841mpwd0hnnk2s5hjugfd0dee69hebudf33l4iy4fe8in60ohi"))}
testObject_MemberUpdate_user_13 :: MemberUpdate
testObject_MemberUpdate_user_13 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "", mupOtrArchive = Just False, mupOtrArchiveRef = Nothing, mupHidden = Nothing, mupHiddenRef = Nothing, mupConvRoleName = Nothing}
testObject_MemberUpdate_user_14 :: MemberUpdate
testObject_MemberUpdate_user_14 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Nothing, mupOtrArchive = Just False, mupOtrArchiveRef = Just ".hA", mupHidden = Just True, mupHiddenRef = Nothing, mupConvRoleName = Just (fromJust (parseRoleName "2avr9"))}
testObject_MemberUpdate_user_15 :: MemberUpdate
testObject_MemberUpdate_user_15 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "", mupOtrArchive = Nothing, mupOtrArchiveRef = Just "\159816\NUL\169051", mupHidden = Nothing, mupHiddenRef = Nothing, mupConvRoleName = Nothing}
testObject_MemberUpdate_user_16 :: MemberUpdate
testObject_MemberUpdate_user_16 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "]Z\RS", mupOtrArchive = Just False, mupOtrArchiveRef = Just "", mupHidden = Nothing, mupHiddenRef = Just "", mupConvRoleName = Just (fromJust (parseRoleName "yy8a83v40ovijjbs"))}
testObject_MemberUpdate_user_17 :: MemberUpdate
testObject_MemberUpdate_user_17 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "I-\1043867", mupOtrArchive = Just True, mupOtrArchiveRef = Just "2F", mupHidden = Just False, mupHiddenRef = Just "]\EM", mupConvRoleName = Just (fromJust (parseRoleName "iflqbjqpqzr0g6hx2qtynsm8xqz0nbr0m9x8uxoml49n1uk0_d9l1j2pxhqcpch0oy1"))}
testObject_MemberUpdate_user_18 :: MemberUpdate
testObject_MemberUpdate_user_18 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "\1068464\ETB", mupOtrArchive = Nothing, mupOtrArchiveRef = Just "\1067896*\DEL", mupHidden = Nothing, mupHiddenRef = Just "\fg", mupConvRoleName = Just (fromJust (parseRoleName "1emc3ef4rn_0r_jm860no3x7nlmjh700713wxapj"))}
testObject_MemberUpdate_user_19 :: MemberUpdate
testObject_MemberUpdate_user_19 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Nothing, mupOtrArchive = Just True, mupOtrArchiveRef = Nothing, mupHidden = Just False, mupHiddenRef = Just "0", mupConvRoleName = Just (fromJust (parseRoleName "iiec_k19plrvh_qy3g79p19i_recyhb856zdse5donsgz8q5ca06zgb3dwfnhhgy_26mxh"))}
testObject_MemberUpdate_user_20 :: MemberUpdate
testObject_MemberUpdate_user_20 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "", mupOtrArchive = Nothing, mupOtrArchiveRef = Nothing, mupHidden = Just False, mupHiddenRef = Just "\CAN", mupConvRoleName = Just (fromJust (parseRoleName "cry3ahb93ig753jddbgnkegszxqpcl6nzl5jmq69_ecffc4"))}

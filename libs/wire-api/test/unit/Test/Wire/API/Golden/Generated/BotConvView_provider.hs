{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.BotConvView_provider where

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
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
import Wire.API.Provider.Service.Tag
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
testObject_BotConvView_provider_1 :: BotConvView
testObject_BotConvView_provider_1 = (botConvView ((Id (fromJust (UUID.fromString "00000003-0000-0013-0000-000500000020")))) (Just "H*e\ENQ!\STX\999248") ([]))
testObject_BotConvView_provider_2 :: BotConvView
testObject_BotConvView_provider_2 = (botConvView ((Id (fromJust (UUID.fromString "0000001b-0000-000d-0000-001000000012")))) (Just "J5\1034078\&4>") ([]))
testObject_BotConvView_provider_3 :: BotConvView
testObject_BotConvView_provider_3 = (botConvView ((Id (fromJust (UUID.fromString "0000001d-0000-0001-0000-001f00000019")))) (Just "\DLE{\129094\39494\48845\f\1024546V") ([OtherMember {omId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}), omConvRoleName = (fromJust (parseRoleName "hr"))}]))
testObject_BotConvView_provider_4 :: BotConvView
testObject_BotConvView_provider_4 = (botConvView ((Id (fromJust (UUID.fromString "0000000e-0000-001a-0000-000200000004")))) (Just "\1001724|\990309vz\1008334\SO") ([OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "f2v_f5lzx0x1ehnz7wq8cmjdwb8k3j6zv7ehgcbx1_fd4lha9p74z_q1zbgbv0e9kv08yz9jgmqx5c8a3ki1mdu_0l"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "kbpvwp56_8jdb9sd8zon0lupxut2me119zl6urlomsx5w44e_agq_t9ppyf8seygfa_nmscx14ob"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "q8qcbbfleymkqligf5c2wc01"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))}), omConvRoleName = (fromJust (parseRoleName "nnvaxokmyge7_lw6xm6go1mjbz6d2xme8g1vs7nvpmq62d"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "1mhg4ccmyhzfxrsgn96f01acnfgj3z0ntlecqov65u2o"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}), omConvRoleName = (fromJust (parseRoleName "viqccu4al9si2vj6pue28pqu41hzbp0_mf1hlf3is76ew43efjizqthi5_vet8nh9nwznwws8p9do_x87rf7u8wk9mgan1y1u8zp64rgqq71x"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "_illamhl1eq2z34xwd6o30n32"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "j50je7rhz8puop9ypbt1cb8oo6k5stm1_2s1s_xlfq85jptk_ifx7clypcc2cems4co1q1fe27ggh91"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "aw3rf3q2hgddn3xid9voepsd74c58z1sh8liy33zm84brn72amk1zkh2szlgq"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "trs_n1rfgd4sj0pb9lmmnpr2t7jzv5p9z1_cbi"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "d5yt7f2tyz78p9ej5wg79z3j0n5t_kfc5wrz4m26yit9b4od1g0p6g8qs85fyvmxhgrppdk4an0ydmdch5zdf0k5shchc5011hkbx8"))}]))
testObject_BotConvView_provider_5 :: BotConvView
testObject_BotConvView_provider_5 = (botConvView ((Id (fromJust (UUID.fromString "00000005-0000-0013-0000-000100000005")))) (Just "f\US\1088278\179566&1\ESC") ([]))

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.OtherMember_user where

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
testObject_OtherMember_user_1 :: OtherMember
testObject_OtherMember_user_1 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000004-0000-001c-0000-000500000016"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "jvn06vb767oqq9polqecs"))}
testObject_OtherMember_user_2 :: OtherMember
testObject_OtherMember_user_2 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000001d-0000-0006-0000-000500000014"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "i12jr_85nd2cr1zbx6i2bc_vxj6vt8xknew6snhjt236uwh5pae2ktq9v7cfmh6imzjfb1pwhgi_dsh_i6v89pbmym"))}
testObject_OtherMember_user_3 :: OtherMember
testObject_OtherMember_user_3 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000001c-0000-0010-0000-00030000000c"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000200000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002")))}), omConvRoleName = (fromJust (parseRoleName "_wkuk7pc60bvq2f6tneng6ygv2f391i9tt"))}
testObject_OtherMember_user_4 :: OtherMember
testObject_OtherMember_user_4 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000001f-0000-0002-0000-00200000000c"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000400000002"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000")))}), omConvRoleName = (fromJust (parseRoleName "7mw93nnxtgyloyuz_xgrqot06jm69dvoioedbafx3zczyvkp9h_k87_98dvxluo9f8tfgljaoi4l3nwe7tk"))}
testObject_OtherMember_user_5 :: OtherMember
testObject_OtherMember_user_5 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000010-0000-0006-0000-000e0000000d"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000400000002")))}), omConvRoleName = (fromJust (parseRoleName "at5hyk97av80661nyfus9rnoun6ftop9okwhhr0hqhfw9a2xlltk75fhay"))}
testObject_OtherMember_user_6 :: OtherMember
testObject_OtherMember_user_6 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000000d-0000-0000-0000-001400000005"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "m5mdkbwycb8x40ndbihrsc48dv219i8ntndgdz4r7cwpufmic8uijkop3kt8ddxrf_294x3t76v8_mmyo4iailb"))}
testObject_OtherMember_user_7 :: OtherMember
testObject_OtherMember_user_7 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000005-0000-0000-0000-001100000001"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "24rpwtrpn2p9uyivr_j05k0od9brcwborg25pt3xd"))}
testObject_OtherMember_user_8 :: OtherMember
testObject_OtherMember_user_8 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000012-0000-001e-0000-00090000000f"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "k5dzt90"))}
testObject_OtherMember_user_9 :: OtherMember
testObject_OtherMember_user_9 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000000c-0000-0008-0000-000300000015"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000400000004"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}), omConvRoleName = (fromJust (parseRoleName "q1lkm_mb57wveperrcflyqanta3zef8ms6u4w8x7z"))}
testObject_OtherMember_user_10 :: OtherMember
testObject_OtherMember_user_10 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000008-0000-000d-0000-00100000001e"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000300000000")))}), omConvRoleName = (fromJust (parseRoleName "id_0zgxmghn52v4i3vn8tpi8ajfe"))}
testObject_OtherMember_user_11 :: OtherMember
testObject_OtherMember_user_11 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000020-0000-000f-0000-000b0000000d"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "f_yx15hkbhglg582f30oytsz6dv1wylrc3m_p84hw2wb8lyi4_fwvq6nulkvlh8beulq"))}
testObject_OtherMember_user_12 :: OtherMember
testObject_OtherMember_user_12 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000004-0000-001c-0000-000700000013"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000000000004"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000400000001")))}), omConvRoleName = (fromJust (parseRoleName "wjr7tahhezp3hr2tcvn3qm190ce_dfaqs2iyjlb17i"))}
testObject_OtherMember_user_13 :: OtherMember
testObject_OtherMember_user_13 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000001c-0000-0012-0000-00100000001d"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000400000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000000000001")))}), omConvRoleName = (fromJust (parseRoleName "qbliqdiydrigapsm8pb8abfufa6frhlnfc78w9j56_s7rz2u8wsf3g80wpzazrfj8blu1amgns5wrp1a"))}
testObject_OtherMember_user_14 :: OtherMember
testObject_OtherMember_user_14 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000700000008"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000400000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000400000002")))}), omConvRoleName = (fromJust (parseRoleName "merslrpbwxeznzwalz2z8cuzb9up4i62zjr8pr7bkyicxyitfi1mclrzeccvhjk0u28ji__t_a3pp38s7d3l3il61qu07j4o09z1dh4pg8fmnezd2n9h7ymcq"))}
testObject_OtherMember_user_15 :: OtherMember
testObject_OtherMember_user_15 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000001f-0000-000c-0000-00200000000c"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000400000003"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000300000000")))}), omConvRoleName = (fromJust (parseRoleName "bf4iu4qshxa7i91d88pczrgn6yob7tl7gme36a1zmdmc7ul32jsm87wnh17fpi1pmi1cbui3wocdqoguqvspftmmvozxykmdes"))}
testObject_OtherMember_user_16 :: OtherMember
testObject_OtherMember_user_16 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000019-0000-0017-0000-001700000016"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "9hd1xtrsmjq2h_j2i1l2q3c_3om09kvhuk4z_9nrcwy6u0ayld85p45o4gx57iyv_u6ta0qj3hnlj8mq8u_34ywqi"))}
testObject_OtherMember_user_17 :: OtherMember
testObject_OtherMember_user_17 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000000b-0000-000b-0000-001b00000018"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "fst5tvst96wiy0rvuulkc5f6ppaf998hez29yf24ggilujmgqtinv5t"))}
testObject_OtherMember_user_18 :: OtherMember
testObject_OtherMember_user_18 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000008-0000-0002-0000-00150000000b"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000400000002"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000400000004")))}), omConvRoleName = (fromJust (parseRoleName "32rfvw9841jx3ckrtrabzy64mqxxamh9"))}
testObject_OtherMember_user_19 :: OtherMember
testObject_OtherMember_user_19 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-001000000007"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000200000004")))}), omConvRoleName = (fromJust (parseRoleName "orm9bv9gefw5hod9s1ag4iif82unz0j60png132mbzxxz4z6hjeay5uvmszscydn8oinybkmrrbcp9xynyautr"))}
testObject_OtherMember_user_20 :: OtherMember
testObject_OtherMember_user_20 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000010-0000-0006-0000-001800000012"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000400000002"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000300000001")))}), omConvRoleName = (fromJust (parseRoleName "5qu5nuktti2pa29rqg4ehgu8bjmst9x21to53nk03sjgdz6q0gs9avjqgolyqxkruv6qrw0c30jkclis28io7rsz5s2b5b5fu8tlpwnkzktn3wfxt6eop"))}

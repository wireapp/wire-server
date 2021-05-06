{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UserIdList_user where

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
testObject_UserIdList_user_1 :: UserIdList
testObject_UserIdList_user_1 = UserIdList {mUsers = [(Id (fromJust (UUID.fromString "00003f3d-0000-7bed-0000-3fee00006661"))),(Id (fromJust (UUID.fromString "00005925-0000-2610-0000-00de00003cae"))),(Id (fromJust (UUID.fromString "000044cd-0000-29d5-0000-2c8e00003aef"))),(Id (fromJust (UUID.fromString "00006316-0000-6654-0000-18dc00002743"))),(Id (fromJust (UUID.fromString "00002e9f-0000-0454-0000-4cc2000060eb"))),(Id (fromJust (UUID.fromString "00000e42-0000-1cf0-0000-045a000011e7"))),(Id (fromJust (UUID.fromString "00004d19-0000-0736-0000-3e6700004018"))),(Id (fromJust (UUID.fromString "0000522d-0000-78f1-0000-7a4c00004f90"))),(Id (fromJust (UUID.fromString "00002dda-0000-5693-0000-56c600002b52"))),(Id (fromJust (UUID.fromString "000018e1-0000-4a80-0000-190000002cc1"))),(Id (fromJust (UUID.fromString "00000d5e-0000-3953-0000-4304000043e3"))),(Id (fromJust (UUID.fromString "00004b1d-0000-7018-0000-465b000068d5"))),(Id (fromJust (UUID.fromString "00002650-0000-41d0-0000-1ec400003046"))),(Id (fromJust (UUID.fromString "000048aa-0000-4833-0000-0430000079fc"))),(Id (fromJust (UUID.fromString "00006ed9-0000-44d9-0000-2e3b00003408"))),(Id (fromJust (UUID.fromString "000062f7-0000-6fcd-0000-49d700004f2f"))),(Id (fromJust (UUID.fromString "000028d3-0000-74b5-0000-6e9c000060e6"))),(Id (fromJust (UUID.fromString "00000160-0000-223c-0000-393e00004f0c"))),(Id (fromJust (UUID.fromString "0000286c-0000-6342-0000-492f0000429c"))),(Id (fromJust (UUID.fromString "00001875-0000-18c7-0000-4f7b00007eef"))),(Id (fromJust (UUID.fromString "00004d4b-0000-5226-0000-52b10000616a"))),(Id (fromJust (UUID.fromString "00006143-0000-46ac-0000-49de00000794"))),(Id (fromJust (UUID.fromString "00001ea3-0000-657a-0000-1d40000007e4")))]}
testObject_UserIdList_user_2 :: UserIdList
testObject_UserIdList_user_2 = UserIdList {mUsers = [(Id (fromJust (UUID.fromString "000022e3-0000-2bc3-0000-79e8000071a9"))),(Id (fromJust (UUID.fromString "00007be4-0000-22db-0000-180b0000324b"))),(Id (fromJust (UUID.fromString "00001498-0000-385e-0000-183b00004daa"))),(Id (fromJust (UUID.fromString "00000c9c-0000-62d1-0000-60210000793a"))),(Id (fromJust (UUID.fromString "00003b26-0000-259b-0000-2aac00003135"))),(Id (fromJust (UUID.fromString "00001180-0000-0e4d-0000-49d40000207d"))),(Id (fromJust (UUID.fromString "00005f90-0000-245f-0000-247e00007e7c"))),(Id (fromJust (UUID.fromString "00005c98-0000-75b5-0000-12fb00001666"))),(Id (fromJust (UUID.fromString "000020f0-0000-2132-0000-0ab200006399"))),(Id (fromJust (UUID.fromString "00003fca-0000-6b2a-0000-1b6e000028be"))),(Id (fromJust (UUID.fromString "000007e2-0000-3c23-0000-709500005df1"))),(Id (fromJust (UUID.fromString "00001ae8-0000-6ea1-0000-001a000028c8"))),(Id (fromJust (UUID.fromString "00003e3f-0000-7993-0000-6b210000156a"))),(Id (fromJust (UUID.fromString "00007144-0000-2cfd-0000-3bf900007018"))),(Id (fromJust (UUID.fromString "00005563-0000-463d-0000-28060000712e"))),(Id (fromJust (UUID.fromString "00006d85-0000-42aa-0000-1c9c000034d3"))),(Id (fromJust (UUID.fromString "000046e8-0000-47ca-0000-2efb00001ab1"))),(Id (fromJust (UUID.fromString "00001efb-0000-7290-0000-1e9f000064a7"))),(Id (fromJust (UUID.fromString "00002582-0000-5510-0000-132800002be3"))),(Id (fromJust (UUID.fromString "0000459b-0000-3594-0000-3a40000018fc"))),(Id (fromJust (UUID.fromString "000031b3-0000-500d-0000-387000003a0a")))]}
testObject_UserIdList_user_3 :: UserIdList
testObject_UserIdList_user_3 = UserIdList {mUsers = [(Id (fromJust (UUID.fromString "000062ba-0000-4494-0000-1c6b00007eee"))),(Id (fromJust (UUID.fromString "000069a4-0000-7988-0000-7fdc000048d9"))),(Id (fromJust (UUID.fromString "00001528-0000-6045-0000-24b100000bd0"))),(Id (fromJust (UUID.fromString "00007b65-0000-7eb9-0000-7a3d00001a6a"))),(Id (fromJust (UUID.fromString "00003239-0000-2ece-0000-5d2e000056c9"))),(Id (fromJust (UUID.fromString "00001292-0000-548e-0000-360b00002ca5"))),(Id (fromJust (UUID.fromString "00000038-0000-5e89-0000-0cc500002a22"))),(Id (fromJust (UUID.fromString "00000822-0000-534c-0000-04c60000150d"))),(Id (fromJust (UUID.fromString "000041cb-0000-1bc1-0000-6ba300005375"))),(Id (fromJust (UUID.fromString "000007b6-0000-01cd-0000-49e300005d67"))),(Id (fromJust (UUID.fromString "000064b9-0000-0f89-0000-1a8200007b4f"))),(Id (fromJust (UUID.fromString "000008a1-0000-62dd-0000-44d300003f95"))),(Id (fromJust (UUID.fromString "000038da-0000-24d9-0000-5b5f000068de"))),(Id (fromJust (UUID.fromString "00006a30-0000-5e21-0000-769600005027"))),(Id (fromJust (UUID.fromString "000077ea-0000-7b13-0000-7e3100007837"))),(Id (fromJust (UUID.fromString "000052f8-0000-7d5c-0000-445300002819"))),(Id (fromJust (UUID.fromString "0000060e-0000-5a73-0000-6895000016e2"))),(Id (fromJust (UUID.fromString "00005496-0000-6b40-0000-4c0700002de7"))),(Id (fromJust (UUID.fromString "0000120e-0000-328d-0000-332a0000746a"))),(Id (fromJust (UUID.fromString "00006d23-0000-7f98-0000-2b2000001044"))),(Id (fromJust (UUID.fromString "00001c8b-0000-4a29-0000-04a600002071"))),(Id (fromJust (UUID.fromString "00001c21-0000-5055-0000-2a6300005d7a"))),(Id (fromJust (UUID.fromString "00007171-0000-27c2-0000-65f2000050f4"))),(Id (fromJust (UUID.fromString "00007529-0000-385f-0000-648000002585"))),(Id (fromJust (UUID.fromString "00006524-0000-23be-0000-5ac300007c64"))),(Id (fromJust (UUID.fromString "0000425b-0000-11a3-0000-4d8a00001ba9")))]}
testObject_UserIdList_user_4 :: UserIdList
testObject_UserIdList_user_4 = UserIdList {mUsers = [(Id (fromJust (UUID.fromString "000045e8-0000-03d6-0000-368000005fce"))),(Id (fromJust (UUID.fromString "00001147-0000-1c00-0000-057e00001eda"))),(Id (fromJust (UUID.fromString "000043a3-0000-12f5-0000-3de900003d31"))),(Id (fromJust (UUID.fromString "0000739a-0000-5e7c-0000-233100000ba7"))),(Id (fromJust (UUID.fromString "000032cf-0000-4d0a-0000-0848000047e6"))),(Id (fromJust (UUID.fromString "00001471-0000-14c7-0000-014800007447")))]}
testObject_UserIdList_user_5 :: UserIdList
testObject_UserIdList_user_5 = UserIdList {mUsers = [(Id (fromJust (UUID.fromString "00002086-0000-0a7a-0000-156800000385"))),(Id (fromJust (UUID.fromString "00000f0c-0000-122a-0000-35e800003116"))),(Id (fromJust (UUID.fromString "00003a37-0000-6e18-0000-24e800003c77"))),(Id (fromJust (UUID.fromString "000037e6-0000-7e6f-0000-38dc00005ed7"))),(Id (fromJust (UUID.fromString "000059be-0000-60ac-0000-39c900004fb2"))),(Id (fromJust (UUID.fromString "000055c3-0000-535c-0000-48cc00003cfb"))),(Id (fromJust (UUID.fromString "000072c6-0000-6278-0000-2d4f00002abd"))),(Id (fromJust (UUID.fromString "00005708-0000-5ad7-0000-4bd40000453d"))),(Id (fromJust (UUID.fromString "00001440-0000-08b9-0000-118c00005489"))),(Id (fromJust (UUID.fromString "000050c6-0000-6bc2-0000-3e6600005522"))),(Id (fromJust (UUID.fromString "0000726f-0000-5a15-0000-5aae000050ea"))),(Id (fromJust (UUID.fromString "0000480f-0000-01ae-0000-1dae00002222"))),(Id (fromJust (UUID.fromString "00002c7e-0000-3a19-0000-60e900007782"))),(Id (fromJust (UUID.fromString "000008d4-0000-4cda-0000-6b25000077a3"))),(Id (fromJust (UUID.fromString "00002118-0000-19e0-0000-291d00004d69"))),(Id (fromJust (UUID.fromString "00007c94-0000-0ad3-0000-154000001981"))),(Id (fromJust (UUID.fromString "000047ac-0000-6a07-0000-75bc00001865"))),(Id (fromJust (UUID.fromString "00003a28-0000-3be5-0000-31040000461f"))),(Id (fromJust (UUID.fromString "000003b2-0000-1cab-0000-69ed0000615d"))),(Id (fromJust (UUID.fromString "00007bb6-0000-4662-0000-12b5000033f3"))),(Id (fromJust (UUID.fromString "00007d96-0000-434c-0000-56c8000034fd"))),(Id (fromJust (UUID.fromString "0000516c-0000-3ba1-0000-474b000046dc"))),(Id (fromJust (UUID.fromString "00001f95-0000-1c1d-0000-7bd20000022a"))),(Id (fromJust (UUID.fromString "00001a3c-0000-65db-0000-28640000121a")))]}

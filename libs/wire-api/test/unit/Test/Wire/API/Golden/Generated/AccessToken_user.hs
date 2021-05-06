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
testObject_AccessToken_user_1 = AccessToken {user = (Id (fromJust (UUID.fromString "00006ab2-0000-63f2-0000-5bfb00001683"))), access = "\rK\ETBa\243\176\179\172\225\142\175M\244\135\183\163\&4\NAK`\n;\244\134\188\180\ETX9\DC31e\240\158\178\150G\240\169\160\137\239\144\178@\ENQ7", tokenType = Bearer, expiresIn = -20}
testObject_AccessToken_user_2 :: AccessToken
testObject_AccessToken_user_2 = AccessToken {user = (Id (fromJust (UUID.fromString "00004a70-0000-77ba-0000-2d2200001c46"))), access = "\GS*PKr\SUBh~?\244\140\133\144#7\\L\226\171\149A&~\244\129\168\159", tokenType = Bearer, expiresIn = 10}
testObject_AccessToken_user_3 :: AccessToken
testObject_AccessToken_user_3 = AccessToken {user = (Id (fromJust (UUID.fromString "00000005-0000-1f41-0000-01e700004a84"))), access = "\228\148\166\243\176\137\173.\STX\DC34-e2\240\160\182\168N\v\232\137\146\243\160\132\176\DC3\243\186\129\142>.", tokenType = Bearer, expiresIn = 2}
testObject_AccessToken_user_4 :: AccessToken
testObject_AccessToken_user_4 = AccessToken {user = (Id (fromJust (UUID.fromString "00006dce-0000-08f7-0000-0c9d00000cd6"))), access = "QQ\240\151\149\187\244\130\167\190\232\147\188K\EOT", tokenType = Bearer, expiresIn = 29}
testObject_AccessToken_user_5 :: AccessToken
testObject_AccessToken_user_5 = AccessToken {user = (Id (fromJust (UUID.fromString "00002192-0000-691d-0000-5cf8000004ed"))), access = "SW\DELX/N\t]I\GS\243\187\177\148l\235\181\183Y\244\132\191\176\244\140\184\152\225\155\166|''r\240\159\145\132+D", tokenType = Bearer, expiresIn = 11}
testObject_AccessToken_user_6 :: AccessToken
testObject_AccessToken_user_6 = AccessToken {user = (Id (fromJust (UUID.fromString "0000253a-0000-5747-0000-5f0d00005e4e"))), access = "h", tokenType = Bearer, expiresIn = 25}
testObject_AccessToken_user_7 :: AccessToken
testObject_AccessToken_user_7 = AccessToken {user = (Id (fromJust (UUID.fromString "00000f11-0000-54fc-0000-69b100004433"))), access = "\\y)j\NUL,\ETB\240\145\133\158\240\172\140\135\244\135\169\162\228\130\131E~\233\184\163\&3(\240\173\161\172", tokenType = Bearer, expiresIn = -24}
testObject_AccessToken_user_8 :: AccessToken
testObject_AccessToken_user_8 = AccessToken {user = (Id (fromJust (UUID.fromString "00003790-0000-73ce-0000-3c8300000592"))), access = "<Z8\236\150\128\DC1\NAK*%\SII7R\ESC\233\163\154UJ~t(", tokenType = Bearer, expiresIn = -15}
testObject_AccessToken_user_9 :: AccessToken
testObject_AccessToken_user_9 = AccessToken {user = (Id (fromJust (UUID.fromString "00006020-0000-1bca-0000-1555000034e0"))), access = "B4", tokenType = Bearer, expiresIn = -30}
testObject_AccessToken_user_10 :: AccessToken
testObject_AccessToken_user_10 = AccessToken {user = (Id (fromJust (UUID.fromString "000073a1-0000-0008-0000-6cd6000000ea"))), access = "v\SO\vxw\EME", tokenType = Bearer, expiresIn = -17}
testObject_AccessToken_user_11 :: AccessToken
testObject_AccessToken_user_11 = AccessToken {user = (Id (fromJust (UUID.fromString "000027f2-0000-19b5-0000-6fa20000651e"))), access = ",c\234\189\166r`H5\ESCT\243\185\148\145\240\173\187\139\243\190\165\153\229\166\128v\224\166\131\CAN(*6O", tokenType = Bearer, expiresIn = -15}
testObject_AccessToken_user_12 :: AccessToken
testObject_AccessToken_user_12 = AccessToken {user = (Id (fromJust (UUID.fromString "00004ee5-0000-331d-0000-55a200001d08"))), access = "*\243\178\158\160).\226\158\148\244\128\142\179=r", tokenType = Bearer, expiresIn = 29}
testObject_AccessToken_user_13 :: AccessToken
testObject_AccessToken_user_13 = AccessToken {user = (Id (fromJust (UUID.fromString "00006853-0000-2aae-0000-0126000035ff"))), access = "uSf\240\159\167\162\DC3:4\240\152\139\183O3'\DC4!\240\169\182\131F/\v\240\158\161\133\ETX~\SIk\234\177\167", tokenType = Bearer, expiresIn = -28}
testObject_AccessToken_user_14 :: AccessToken
testObject_AccessToken_user_14 = AccessToken {user = (Id (fromJust (UUID.fromString "000021c3-0000-4cea-0000-2f850000599c"))), access = "\DC3\DLEkj\SYN@&\244\129\140\146\243\187\159\132\243\180\163\148|R-", tokenType = Bearer, expiresIn = 12}
testObject_AccessToken_user_15 :: AccessToken
testObject_AccessToken_user_15 = AccessToken {user = (Id (fromJust (UUID.fromString "00003469-0000-1303-0000-4b3d00005cb9"))), access = "", tokenType = Bearer, expiresIn = -19}
testObject_AccessToken_user_16 :: AccessToken
testObject_AccessToken_user_16 = AccessToken {user = (Id (fromJust (UUID.fromString "000017a8-0000-7f68-0000-114a00003538"))), access = "\f\ACKi\DLE", tokenType = Bearer, expiresIn = 15}
testObject_AccessToken_user_17 :: AccessToken
testObject_AccessToken_user_17 = AccessToken {user = (Id (fromJust (UUID.fromString "00003627-0000-7179-0000-08d100006484"))), access = "\EM\DC1gs(G\tM\243\191\138\177\243\160\135\142\244\141\154\175\ACKO b", tokenType = Bearer, expiresIn = 30}
testObject_AccessToken_user_18 :: AccessToken
testObject_AccessToken_user_18 = AccessToken {user = (Id (fromJust (UUID.fromString "00003875-0000-2049-0000-61ff0000778d"))), access = "\240\163\132\133]\240\160\182\146]\SYN\243\184\142\144J6AP\t\243\176\151\154y\244\142\165\137u.\232\180\175(\DC3;d\ACK2\244\132\130\153", tokenType = Bearer, expiresIn = -18}
testObject_AccessToken_user_19 :: AccessToken
testObject_AccessToken_user_19 = AccessToken {user = (Id (fromJust (UUID.fromString "00005acf-0000-6596-0000-1e8900005b93"))), access = "\224\184\176 0", tokenType = Bearer, expiresIn = -13}
testObject_AccessToken_user_20 :: AccessToken
testObject_AccessToken_user_20 = AccessToken {user = (Id (fromJust (UUID.fromString "0000445f-0000-643c-0000-384400001603"))), access = "\NAK\CAN3_Y*\239\135\181", tokenType = Bearer, expiresIn = 30}

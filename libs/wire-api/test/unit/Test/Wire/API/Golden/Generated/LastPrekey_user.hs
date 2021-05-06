{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.LastPrekey_user where

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
testObject_LastPrekey_user_1 :: LastPrekey
testObject_LastPrekey_user_1 = (lastPrekey ("\1097499H\ACK&%\ENQjm\1056636\164762Cwnp-N\168029 '\159789"))
testObject_LastPrekey_user_2 :: LastPrekey
testObject_LastPrekey_user_2 = (lastPrekey ("\ENQ\SO\48173o\CANF`\187437\NAK\1048287\&0"))
testObject_LastPrekey_user_3 :: LastPrekey
testObject_LastPrekey_user_3 = (lastPrekey ("\SUB\USA\1032703qsAKZ\1038435Sy"))
testObject_LastPrekey_user_4 :: LastPrekey
testObject_LastPrekey_user_4 = (lastPrekey ("o2"))
testObject_LastPrekey_user_5 :: LastPrekey
testObject_LastPrekey_user_5 = (lastPrekey ("\NULYv\ap@ESq\54262x}"))
testObject_LastPrekey_user_6 :: LastPrekey
testObject_LastPrekey_user_6 = (lastPrekey ("<\186723G!\99831"))
testObject_LastPrekey_user_7 :: LastPrekey
testObject_LastPrekey_user_7 = (lastPrekey ("Q\ETX\ETX\rFH#2A\fwcA\18301A\993385\1023183\r\41668MG?Sl "))
testObject_LastPrekey_user_8 :: LastPrekey
testObject_LastPrekey_user_8 = (lastPrekey ("\SUB"))
testObject_LastPrekey_user_9 :: LastPrekey
testObject_LastPrekey_user_9 = (lastPrekey ("\1028981!\1012443\49202_sQ\92678\14631Q\53991R~\EMt\985758\GSPaD \1039027$#\EOT\27656"))
testObject_LastPrekey_user_10 :: LastPrekey
testObject_LastPrekey_user_10 = (lastPrekey ("{YOu%\DEL>_\1052506*\DC1E,\RS\1039281L\32467g\1103175\1111324DD\1063368\STX("))
testObject_LastPrekey_user_11 :: LastPrekey
testObject_LastPrekey_user_11 = (lastPrekey ("\"$S\DLE\96438S\ACK(\141517\&8\v\USw\th\60895\&5%u\ENQ\1067563\&2\SOUV\EOT"))
testObject_LastPrekey_user_12 :: LastPrekey
testObject_LastPrekey_user_12 = (lastPrekey ("\NULG@\DC3Wx\SYN\1032613s\FS\47242\STX\GS4}A\1072590$`B\1066283\GS?\1094886\&2\146676"))
testObject_LastPrekey_user_13 :: LastPrekey
testObject_LastPrekey_user_13 = (lastPrekey (""))
testObject_LastPrekey_user_14 :: LastPrekey
testObject_LastPrekey_user_14 = (lastPrekey ("mS><\990422\n\1063785t"))
testObject_LastPrekey_user_15 :: LastPrekey
testObject_LastPrekey_user_15 = (lastPrekey ("\b\1074290\127401\b\DC2\41891_"))
testObject_LastPrekey_user_16 :: LastPrekey
testObject_LastPrekey_user_16 = (lastPrekey ("\1088916%\RS\ETB\DLE/;\NAK\1005119OY"))
testObject_LastPrekey_user_17 :: LastPrekey
testObject_LastPrekey_user_17 = (lastPrekey ("I\35315(0\1090218&\1102658\22762\990090\1095159>\1051251\30251\STXz\51117_Rf\96210"))
testObject_LastPrekey_user_18 :: LastPrekey
testObject_LastPrekey_user_18 = (lastPrekey (",\48106\EOT\59943\DC3\DEL.\1103153\&5n\US$\1060921\146440\&101\118926\41665:"))
testObject_LastPrekey_user_19 :: LastPrekey
testObject_LastPrekey_user_19 = (lastPrekey ("pCJf"))
testObject_LastPrekey_user_20 :: LastPrekey
testObject_LastPrekey_user_20 = (lastPrekey ("\985396\&8W\154968%\NAKjzY\147520M2'(\1018818"))

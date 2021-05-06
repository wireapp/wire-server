{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Name_user where

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
testObject_Name_user_1 :: Name
testObject_Name_user_1 = Name {fromName = " 8K=F?qW_\1076784V\1043363\CAN\1039121\&4\DC3L &@j$D\SO\1075581\f\1113955\62353T\EOT\1043182\NULa"}
testObject_Name_user_2 :: Name
testObject_Name_user_2 = Name {fromName = "\159213\nVk\RS\rgwyf\DC2d\997777\158401\8578\1053665\97720NIl\n\142150/\9119\132378r9hrKQ{\DC4:\991972c\60314u\1040377\1104902vMf\\9Q`\SI/\1045256\DC4\ENQ\1009798\157224\1045631r\137081qE\n,^\SOH'|,7\28961hru\CANqm+\DC2\1027930cL;fX:\EOT\NAKm\SO9\14610vd\58337\t\1008396v\ETB&\GSUE{:t\1028161\24486Icx\95818Xu47b\1048808\DLE&"}
testObject_Name_user_3 :: Name
testObject_Name_user_3 = Name {fromName = "p\DELs\184392\18393\r\154426s\NUL\EOT\148646\DELP\f\27885\DELF_\100357\33238\1009511\t"}
testObject_Name_user_4 :: Name
testObject_Name_user_4 = Name {fromName = "\SUB{d\f\6648\30502\1022878\US7tB\SUB^\182033\189320\6839lr\CANiN\53702\1075802\151993;\1008857D\997429\&8\1029779\141242F}\CANpg\988973\40443\1065763l\ACK\1049860\GS\ETBmpCLab\r2\100868\60097\155771\&3'V\fKL\1094815\126487|>k\DC1s(\132781\1049318\"\STX0i\12840\137338L\DLE'\22747\171094kv +6W\1045119\1010344\1032824\r\DC4T\44579&\DC2\1042862C]\DC2'-L\RSU$\FS~_\DC2"}
testObject_Name_user_5 :: Name
testObject_Name_user_5 = Name {fromName = "\1075421\1072588\fo\1079922y\a\DC4P\1007846/\1037611\t\58893\45567<\1102134%iO}h\DLE\147173.`\995769\20060\1074407\1101825\SYN\155521qL7m\133249\178970\1059952\&9x^\bq3\ACK)\183931F\131520b\15769rn\38664l\163842^n\SOpLgEb\DEL\1060058\1016661"}

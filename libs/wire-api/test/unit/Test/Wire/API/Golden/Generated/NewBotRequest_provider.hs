{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.NewBotRequest_provider where

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
testObject_NewBotRequest_1 :: NewBotRequest
testObject_NewBotRequest_1 = NewBotRequest {newBotId = ((BotId . Id) (fromJust (UUID.fromString "00000002-0000-0002-0000-000300000003"))), newBotClient = ClientId {client = "3"}, newBotOrigin = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), botUserViewName = Name {fromName = "\1010670/\EMs\1111114\FS5\DC1O\185163\EOT\ETX$\161713@\SO\RS\CAN\fL\ESCYvJ\r\18568qoG\ACK\67850\ESC\FS/(#m\62819\&7y"}, botUserViewColour = ColourId {fromColourId = 1}, botUserViewHandle = Just (Handle {fromHandle = "oy7gek8vfekh87xwhq6hpf32ozqnkc7hbm9gqma9v_jra.5dqmt52_osoy6bqf30xob5_swiukkxu5fzy2n5z5-5st3laa9olvi3yqj8bbyxubv-zuxaqoqnzemveizyp5hzs-cf64vf17khezmhu"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))}, newBotConv = (botConvView ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) (Just "") ([])), newBotToken = "_\1060155\1005893`", newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.ZH, lCountry = Nothing}}
testObject_NewBotRequest_2 :: NewBotRequest
testObject_NewBotRequest_2 = NewBotRequest {newBotId = ((BotId . Id) (fromJust (UUID.fromString "00000004-0000-0004-0000-000100000004"))), newBotClient = ClientId {client = "6"}, newBotOrigin = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), botUserViewName = Name {fromName = "O\1046215\173563\SOH\1110873Z*A.-\ETX#\1082575I$\ESCe \14496wy]F%'\1028917\GSE\b92\12963P*@e'"}, botUserViewColour = ColourId {fromColourId = 1}, botUserViewHandle = Just (Handle {fromHandle = "hazvdq"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))}, newBotConv = (botConvView ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) (Just "\1045106") ([OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}), omConvRoleName = (fromJust (parseRoleName "7tuq5mgleton8meybctml7n_qcli1gnbnr0uv9vyx52jpgsp_e1ce_bc04yjnefc_oafyg2w"))}])), newBotToken = "\r\1047414Y\12343\DC1", newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.LA, lCountry = Just (Country {fromCountry = PF})}}
testObject_NewBotRequest_3 :: NewBotRequest
testObject_NewBotRequest_3 = NewBotRequest {newBotId = ((BotId . Id) (fromJust (UUID.fromString "00000004-0000-0000-0000-000400000002"))), newBotClient = ClientId {client = "7"}, newBotOrigin = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), botUserViewName = Name {fromName = "\1037844o\ETB\bJ8\137994=\155378}\6389\EM"}, botUserViewColour = ColourId {fromColourId = 1}, botUserViewHandle = Just (Handle {fromHandle = "oj.mm6b"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))}, newBotConv = (botConvView ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) (Just "") ([])), newBotToken = "\NUL;", newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.DZ, lCountry = Just (Country {fromCountry = LV})}}
testObject_NewBotRequest_4 :: NewBotRequest
testObject_NewBotRequest_4 = NewBotRequest {newBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0000-0000-000200000001"))), newBotClient = ClientId {client = "4"}, newBotOrigin = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), botUserViewName = Name {fromName = "Yb\ACK\a':LF!5\1107144\1029642\&4YC\1068385m\159048\1038216\SII\1042434\185965~d\FS\\\59509q\ETB#\3392LY\1038978\t\1083132<\SO\1112636\ESC\57732\GS\993263\EM\173982s\SYN+z\1061802C\FSM8\984444"}, botUserViewColour = ColourId {fromColourId = 1}, botUserViewHandle = Just (Handle {fromHandle = "3ykv40dlkb"}), botUserViewTeam = Nothing}, newBotConv = (botConvView ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) (Just "") ([OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "glzhvp00j29xtiex0"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}), omConvRoleName = (fromJust (parseRoleName "grt6ilg5p46yazp6r28pwr5k8x7waoq1gnq14ba72yxsm_t_pimjt7zyp9gm2057pmvt2tq0i571gnxjfbb71i"))}])), newBotToken = "", newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.SI, lCountry = Just (Country {fromCountry = YE})}}
testObject_NewBotRequest_5 :: NewBotRequest
testObject_NewBotRequest_5 = NewBotRequest {newBotId = ((BotId . Id) (fromJust (UUID.fromString "00000000-0000-0003-0000-000300000001"))), newBotClient = ClientId {client = "a"}, newBotOrigin = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), botUserViewName = Name {fromName = "3\155459QR\ETB!e\NUL*\b\135564\1058542\FS_B<\73864S>i\170299\&0xH\172345\aBU\DEL\161628X\25329\DC2\175996\53364[i\39225t\ETX:0\US<\SYN\100081R>PH7\55016'\STX\1011841\166419\ay\1113922yNf\\.R\7149\137692\1078177\1002996>\996229\1051044wxE\10415rXqF\1058785\DC2x*\NUL\FS"}, botUserViewColour = ColourId {fromColourId = 0}, botUserViewHandle = Just (Handle {fromHandle = "yck731m"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}, newBotConv = (botConvView ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) (Just "\39752") ([OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "g8n6vot7n21a7p5uh6ut6gkzdyk0eze05gy2ud2ekxi2bbqtnnky3yaldly8b5kt1jsrvr9huah62gfuh26gl96fr1grl4ovvxug3plzd0_cpvsg9er077ek2s6rb"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "qxcpdggip2f8rf7qwxbv4t752iu1hpip0f7lcqnync_vr8cxfr8q91hmrz9f2pnj3zpz51sqvufubk3c4e907hwuvxo6lx_bbyacjhgpzeykb8_1ve4jm"))}])), newBotToken = "m\30186", newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.LU, lCountry = Just (Country {fromCountry = BT})}}

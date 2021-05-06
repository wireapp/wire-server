{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.CookieLabel_user where

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
testObject_CookieLabel_user_1 :: CookieLabel
testObject_CookieLabel_user_1 = CookieLabel {cookieLabelText = ">\EOT\RS\1028791Z\SYN\151970\&7"}
testObject_CookieLabel_user_2 :: CookieLabel
testObject_CookieLabel_user_2 = CookieLabel {cookieLabelText = "\161496\142982&Z\DC1)*\DLE%\1007457n\ESC\"\ESC9M\ENQ\1061197a* 2\r5"}
testObject_CookieLabel_user_3 :: CookieLabel
testObject_CookieLabel_user_3 = CookieLabel {cookieLabelText = " Mo_\1091159:1\n\1056110*tx\144331\73026\DLE\ETB\1008068\SI\984367"}
testObject_CookieLabel_user_4 :: CookieLabel
testObject_CookieLabel_user_4 = CookieLabel {cookieLabelText = "ssi\94437\45204\bp$\FSeO<8"}
testObject_CookieLabel_user_5 :: CookieLabel
testObject_CookieLabel_user_5 = CookieLabel {cookieLabelText = "\1060629 \DC3\ENQ\6091"}
testObject_CookieLabel_user_6 :: CookieLabel
testObject_CookieLabel_user_6 = CookieLabel {cookieLabelText = "\1053931#40\a\NUL\1088538\153300\1023345\SOH"}
testObject_CookieLabel_user_7 :: CookieLabel
testObject_CookieLabel_user_7 = CookieLabel {cookieLabelText = "a0E\1033899\ESC\ETBBCPk/m\73856\1047461\27929O\1047268\SUBM\1089638\1093748@!,\57824\ETBgq\30486"}
testObject_CookieLabel_user_8 :: CookieLabel
testObject_CookieLabel_user_8 = CookieLabel {cookieLabelText = "e\1085231Xb_\164762\EOT\US\25334,\CAN\SUB\GS>SF\127804"}
testObject_CookieLabel_user_9 :: CookieLabel
testObject_CookieLabel_user_9 = CookieLabel {cookieLabelText = "U\54922A\6280u-3[q0\CAN\SOH\1016297^\190526#\16696\147990\1077642J-\14591\USJ\t|Q\DEL"}
testObject_CookieLabel_user_10 :: CookieLabel
testObject_CookieLabel_user_10 = CookieLabel {cookieLabelText = "6\35987\1092876-5x\13283\50972=.N1Z\133760\&1P\97167\1108005\990613C\\\51642\NUL\1005042J\48088#"}
testObject_CookieLabel_user_11 :: CookieLabel
testObject_CookieLabel_user_11 = CookieLabel {cookieLabelText = "\EOT\1067315\&15E\72338\184968\RSWG\USE"}
testObject_CookieLabel_user_12 :: CookieLabel
testObject_CookieLabel_user_12 = CookieLabel {cookieLabelText = "+\40096w3$t:]\STXq\1112283\&9\NUL%\SYN"}
testObject_CookieLabel_user_13 :: CookieLabel
testObject_CookieLabel_user_13 = CookieLabel {cookieLabelText = "\SOHeN={\2214I\45610V"}
testObject_CookieLabel_user_14 :: CookieLabel
testObject_CookieLabel_user_14 = CookieLabel {cookieLabelText = "GB%P\1054796\1111354\&7"}
testObject_CookieLabel_user_15 :: CookieLabel
testObject_CookieLabel_user_15 = CookieLabel {cookieLabelText = "+)\STXd<I[\1036610\166420/{"}
testObject_CookieLabel_user_16 :: CookieLabel
testObject_CookieLabel_user_16 = CookieLabel {cookieLabelText = "\1018459k\52133\DC2\\\149901\t7\ESC7Z\EMSc%\14206\1000936|e\1070471\984119\SO\63203"}
testObject_CookieLabel_user_17 :: CookieLabel
testObject_CookieLabel_user_17 = CookieLabel {cookieLabelText = "\15816\154945x!\SYNi\a\133647\48473\SYN`\29510\48004\12349pg*l[\1017807\186664\SYN\ESCO"}
testObject_CookieLabel_user_18 :: CookieLabel
testObject_CookieLabel_user_18 = CookieLabel {cookieLabelText = "Xu\3226z<u\USwC\1037138nn\vf\DC4"}
testObject_CookieLabel_user_19 :: CookieLabel
testObject_CookieLabel_user_19 = CookieLabel {cookieLabelText = "Og\rg\DC3L\995837\1035137kRn(\121377\1004656\1051422\&3\CAN+S\GS\186604\49160?$\t1"}
testObject_CookieLabel_user_20 :: CookieLabel
testObject_CookieLabel_user_20 = CookieLabel {cookieLabelText = "HI\1064246x\t H+\DC4\991362O\GS\t\vg"}

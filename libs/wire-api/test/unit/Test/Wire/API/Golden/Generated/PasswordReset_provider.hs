{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.PasswordReset_provider where

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
testObject_PasswordReset_provider_1 :: PasswordReset
testObject_PasswordReset_provider_1 = PasswordReset {nprEmail = Email {emailLocal = "#", emailDomain = "z<s{\14474\183044h(\SI\NAK)\189025p5\v\r\168158\28489\35468\&0K$N\"\SUB"}}
testObject_PasswordReset_provider_2 :: PasswordReset
testObject_PasswordReset_provider_2 = PasswordReset {nprEmail = Email {emailLocal = "1s\SOH{=\aI\SYN.3\1074050z\n\1010432\1084181\US\15384t\135516", emailDomain = "\28454\92968\68428\ACK\993404\ESC\166907yV\19230\&8(-\158212\46234&+)k"}}
testObject_PasswordReset_provider_3 :: PasswordReset
testObject_PasswordReset_provider_3 = PasswordReset {nprEmail = Email {emailLocal = "UI\1053283\1007493|\991281\157076\59944\SYNMN=\1005132Vk\t\SIf\1047612E\DC1\1063063MN#7,S", emailDomain = "Up9G\1076064\1100523RlovB\1068307\n\1055637\11667\FS\78187\\8"}}
testObject_PasswordReset_provider_4 :: PasswordReset
testObject_PasswordReset_provider_4 = PasswordReset {nprEmail = Email {emailLocal = "AEZA|\EOT", emailDomain = "\1057893\66899r\RSB\ETB\986471Nf`\NAKE\164984GVH~\1056801S4"}}
testObject_PasswordReset_provider_5 :: PasswordReset
testObject_PasswordReset_provider_5 = PasswordReset {nprEmail = Email {emailLocal = "\2386[j^\1077179\b\98237e<Y\nXk", emailDomain = "\"0\1047618br}\38108\n"}}

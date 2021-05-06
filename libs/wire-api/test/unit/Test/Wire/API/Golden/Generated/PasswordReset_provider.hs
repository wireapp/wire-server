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
testObject_PasswordReset_provider_1 = PasswordReset {nprEmail = Email {emailLocal = ",\26218\DC2q\am\993028\1112056o\49322\25253'\1005607=A\992863rq(\STXJ|Z\5620\ENQy\1090347y", emailDomain = ""}}
testObject_PasswordReset_provider_2 :: PasswordReset
testObject_PasswordReset_provider_2 = PasswordReset {nprEmail = Email {emailLocal = "\CAN\\>r\USP\SUB\ESC\156751\1089884)\1081856\3981", emailDomain = "|L\163126\15555\DC1[bR%\1055599\DC1\17421q"}}
testObject_PasswordReset_provider_3 :: PasswordReset
testObject_PasswordReset_provider_3 = PasswordReset {nprEmail = Email {emailLocal = "\1070854\&4\\\v.\1059269!%17`mZ\CAN_A\25513\128308E\179292{\1030918\a$3w\DC4", emailDomain = "^"}}
testObject_PasswordReset_provider_4 :: PasswordReset
testObject_PasswordReset_provider_4 = PasswordReset {nprEmail = Email {emailLocal = "+\SOFchuD\140524\&4[", emailDomain = "\992214_\SI+\1061277W^\DC4\1053609g`z6P\1088973TsBn1\993151\34409\ACK"}}
testObject_PasswordReset_provider_5 :: PasswordReset
testObject_PasswordReset_provider_5 = PasswordReset {nprEmail = Email {emailLocal = "\990001\&8xs?\178039\US\GSP\USf\96551\DC4[\DC1", emailDomain = "e9/\DC2Uj\EOTw\45496u"}}

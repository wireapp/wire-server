{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.EmailUpdate_provider where

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
testObject_EmailUpdate_provider_1 :: EmailUpdate
testObject_EmailUpdate_provider_1 = EmailUpdate {euEmail = Email {emailLocal = "\vmzn", emailDomain = "\1063998\STXn \ENQ |\61203Fyv\72307\63147>"}}
testObject_EmailUpdate_provider_2 :: EmailUpdate
testObject_EmailUpdate_provider_2 = EmailUpdate {euEmail = Email {emailLocal = "\1090379.?\v)R\DC1I\33145\138169?Mw\1069056\1053492\RS\74914C\\\1015832\GS\DEL", emailDomain = ""}}
testObject_EmailUpdate_provider_3 :: EmailUpdate
testObject_EmailUpdate_provider_3 = EmailUpdate {euEmail = Email {emailLocal = "\995612\1011750lhb\ETByN_s\SOH\DC3\1062455F", emailDomain = ":a#'6#?+\179507NN)L0`\14699\ETX,"}}
testObject_EmailUpdate_provider_4 :: EmailUpdate
testObject_EmailUpdate_provider_4 = EmailUpdate {euEmail = Email {emailLocal = "=jaIp\1009561\181768uj\133775p\NUL\96625", emailDomain = "\\SS\1050375\1036920 \"\DC2\1010977\6529\1090616\985532\66829\28661Vt\1091383\1018863r\96905\&2"}}
testObject_EmailUpdate_provider_5 :: EmailUpdate
testObject_EmailUpdate_provider_5 = EmailUpdate {euEmail = Email {emailLocal = "\184804\vs(\73980_6\"", emailDomain = "\988235`>\GS\ESC]\DC4\ETB\DLE`"}}

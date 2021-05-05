{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ProviderActivationResponse_provider where

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
testObject_ProviderActivationResponse_1 :: ProviderActivationResponse
testObject_ProviderActivationResponse_1 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "J;\ACK\63487\ENQ\127293\1030252*\1007342\14433:f", emailDomain = "?V|\158527aw[\140236"}}
testObject_ProviderActivationResponse_2 :: ProviderActivationResponse
testObject_ProviderActivationResponse_2 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\br\141236\1092311y|h\1074300+\72158:o|\59689I\53438e7\18021\SOH\ESCL\150076<", emailDomain = "^+}A\NAK\1047432\NULIK"}}
testObject_ProviderActivationResponse_3 :: ProviderActivationResponse
testObject_ProviderActivationResponse_3 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "WZ;a5c\144668R\1031532\ENQ\DC44\ta_(\ENQAC\143154D\ESCYV\1080942\ETBS\b", emailDomain = "\168692\95956\&2V\DELML\DEL\NAKNlj\1108081\&7\aG)&\f"}}
testObject_ProviderActivationResponse_4 :: ProviderActivationResponse
testObject_ProviderActivationResponse_4 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "*dJ", emailDomain = "Z\FS^-"}}
testObject_ProviderActivationResponse_5 :: ProviderActivationResponse
testObject_ProviderActivationResponse_5 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\1064141\RS\\Vq0\SOLq,\1052800", emailDomain = "M\98468Z\144424\&1uP\DC1\SO\DC1G\EM\NUL\1014585x\DC3#\47520\1013181Q"}}

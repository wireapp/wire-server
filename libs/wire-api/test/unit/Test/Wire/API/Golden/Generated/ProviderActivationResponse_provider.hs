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
testObject_ProviderActivationResponse_provider_1 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_1 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\b\1107162\10815\9144H\n\1030495\EMHgcva2\DELk\1099390<jE", emailDomain = "\992278:]\DC4,\999079\SUBo\1081502*"}}
testObject_ProviderActivationResponse_provider_2 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_2 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\1021412\EM'#\1033766!o\FS", emailDomain = "h"}}
testObject_ProviderActivationResponse_provider_3 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_3 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\NUL\\", emailDomain = "\156059s\n\1086373?\ACK\1003622;\SO\154137Aj\95070\DC3\95335l\DC2X\178674~+r7"}}
testObject_ProviderActivationResponse_provider_4 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_4 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "#\182304\1095911i5zHL\EM", emailDomain = "\144024J\n_\STX,\1105852\&3\1039280+f\ESC\RS\140762M^n3$\1085215,\n"}}
testObject_ProviderActivationResponse_provider_5 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_5 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\SO\"4\140287{z", emailDomain = "\rp\1036276\FS`\145629\38991m(W?\1059447\bAk_;G\983180\23728Y"}}

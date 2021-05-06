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
testObject_EmailUpdate_provider_1 = EmailUpdate {euEmail = Email {emailLocal = "K\1073448\1041903\1040711(\164962Q\171416[\SO<\DC1\RS~g\1096773U\1072019sX\64900", emailDomain = "_\69438(\GS\EMO\1105619\1055477\3255y3\13524M_|\1100486R^d\1047593EG6\DC2"}}
testObject_EmailUpdate_provider_2 :: EmailUpdate
testObject_EmailUpdate_provider_2 = EmailUpdate {euEmail = Email {emailLocal = "Cde\34468\50990\163501\f!L]Bq\ESC1\DC3\1027169D\1015710B\bn\v34n", emailDomain = "}\1077893^"}}
testObject_EmailUpdate_provider_3 :: EmailUpdate
testObject_EmailUpdate_provider_3 = EmailUpdate {euEmail = Email {emailLocal = "k\46983t\DC2%I\1108473\186465O\179973\985929:!\DLEN\DC3k\134147Pb~", emailDomain = "r"}}
testObject_EmailUpdate_provider_4 :: EmailUpdate
testObject_EmailUpdate_provider_4 = EmailUpdate {euEmail = Email {emailLocal = "O'\47822\SUB\tN\1055908V\94574R\DC4\998503\ETB^Hl3", emailDomain = "u\US4\txe\DC2\RS*H\74321\DC2&N\179568\f\DLEV\42031\1109234O#\29117s|\ETB0y?"}}
testObject_EmailUpdate_provider_5 :: EmailUpdate
testObject_EmailUpdate_provider_5 = EmailUpdate {euEmail = Email {emailLocal = "4U\f\ACKU!4\990331S\64935r\1073949", emailDomain = "8=\1011316\DC3_b\1050314\aQ\1088066r\155163/*\f&:\1006232*\RSZJ"}}

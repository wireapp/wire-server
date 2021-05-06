{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.NewBotResponse_provider where

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
testObject_NewBotResponse_provider_1 :: NewBotResponse
testObject_NewBotResponse_provider_1 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\986094"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("\SOH")), rsNewBotName = Just (Name {fromName = "\1011706n\135409\163620\1032841\11310mp\ETB\1101870\&6DE\1104365*\29489lR|R2C\1098261\DC1\137884\&9L\US\1026228oW\58803r@\CANvQ\161423Xq}\984589S\\%Z\ESCK\FSH7\USL\1034973\GS\DC4]5\DC2p\t\987721\1036737\58605t\1068044\985269!F\60671t\STX\DC3K_ym\1108765\41737+\67738-5\ESC\EOT\SOH'\1075796\1040198"}), rsNewBotColour = Nothing, rsNewBotAssets = Nothing}
testObject_NewBotResponse_provider_2 :: NewBotResponse
testObject_NewBotResponse_provider_2 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("F")), rsNewBotName = Just (Name {fromName = "\51979\DC1t\DEL*p\166295\32792'\1041731\1035440aR\DC4\ETB\1024638\138771\57507V\DC2kf\b\ENQ-\186564\42818|\DLEm5\\\SI<\b)u}\EOTM\45312\EOT\189657\1105277\&5\1042198\30953\GS\v\17663\1056222+s\1043821\157955\36118gO\1030540\FSZ\25569_\\C)\33200\DLE\ETBOq-]\DC1L\b\"$_\22568\DEL\rLf;oe\1094740iH8\997797\a~-`y\SI\\\1032146?1\1031248\DLE\180516\1043788\DC4\17579"}), rsNewBotColour = Just (ColourId {fromColourId = -3}), rsNewBotAssets = Just []}
testObject_NewBotResponse_provider_3 :: NewBotResponse
testObject_NewBotResponse_provider_3 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("6\1039935I\rKL")), rsNewBotName = Just (Name {fromName = ".\167730\61460\166795[\v\1023502\68838\1054788\10690\7391\rae=i"}), rsNewBotColour = Nothing, rsNewBotAssets = Just [(ImageAsset ",*\1044968" (Just AssetComplete)),(ImageAsset "\26589\15916" (Just AssetPreview)),(ImageAsset "Zw/" (Just AssetComplete)),(ImageAsset "\DC2GX" (Just AssetPreview)),(ImageAsset "," (Just AssetComplete))]}
testObject_NewBotResponse_provider_4 :: NewBotResponse
testObject_NewBotResponse_provider_4 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("z\1066501&\RS")), rsNewBotName = Just (Name {fromName = "\29361\44934\991708\&6#\38965(\DC4A\SOH\1011211E\174630\\\SYNO\1105362\&7\187646W\1040495Izj\ETX\ENQ\ENQ\1011988\1016244\NAK\4023\111137\SOH4\61960; qM\180804\19060^\DC2\1045693f\1045068v\1071719{C\DC4\1081430\ENQ["}), rsNewBotColour = Nothing, rsNewBotAssets = Nothing}
testObject_NewBotResponse_provider_5 :: NewBotResponse
testObject_NewBotResponse_provider_5 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\1088715"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("v0E?]")), rsNewBotName = Nothing, rsNewBotColour = Nothing, rsNewBotAssets = Just [(ImageAsset "\181011" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing)),(ImageAsset "\158821V;" (Just AssetComplete))]}

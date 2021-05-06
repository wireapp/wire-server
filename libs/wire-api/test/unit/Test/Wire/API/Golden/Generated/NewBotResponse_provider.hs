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
testObject_NewBotResponse_provider_1 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("")), rsNewBotName = Nothing, rsNewBotColour = Just (ColourId {fromColourId = -7}), rsNewBotAssets = Just [(ImageAsset "" (Just AssetComplete)),(ImageAsset "\152401\f" (Just AssetPreview)),(ImageAsset "V\1018085" (Just AssetPreview)),(ImageAsset "V@" (Nothing)),(ImageAsset "z\1051289\&6" (Just AssetComplete)),(ImageAsset ":\1061364" (Just AssetPreview))]}
testObject_NewBotResponse_provider_2 :: NewBotResponse
testObject_NewBotResponse_provider_2 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("k\"")), rsNewBotName = Just (Name {fromName = "\1098394\160207\1273N\160324\DC1\186528t\25834cv^\DC2\ETB\52377\1026418\EM:|\SUBs\988370;\99065\1027497\141192\1028131`@l\USX8s=\1004660\1109604D\DELym7q6\DLE\SOHx=G\132323\EM\62063u*<A\NUL"}), rsNewBotColour = Just (ColourId {fromColourId = -1}), rsNewBotAssets = Nothing}
testObject_NewBotResponse_provider_3 :: NewBotResponse
testObject_NewBotResponse_provider_3 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "W"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "0"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\12865"}], rsNewBotLastPrekey = (lastPrekey ("\NUL$R\3357\DEL\f")), rsNewBotName = Just (Name {fromName = "5\1103771[C\nq\118975'\171674\169112e\FSp@<v\188789&\NULr\1092112x\1093238\4855pR/pw\"\999411R\FS\SO.z\ETX\ETXvS\1086461z\10860\&6\1045058\&9\1081589\ETX\1085436\175156\1030845\1018011\DC2FBM\141240\985419; X%\191151\125006N<\f%3d\1093503\UST\168739X\1042655o\b\1049010\ENQE\ENQ\1017686G\DEL\ESC\1027723(!+#V\1097470\ETX\1035438g^ "}), rsNewBotColour = Just (ColourId {fromColourId = -7}), rsNewBotAssets = Just [(ImageAsset "\145585" (Nothing)),(ImageAsset "o:\131578" (Nothing)),(ImageAsset "\ETX" (Just AssetPreview)),(ImageAsset "V\1025756U" (Nothing)),(ImageAsset "0w\1073790" (Nothing)),(ImageAsset "\994385!" (Just AssetPreview))]}
testObject_NewBotResponse_provider_4 :: NewBotResponse
testObject_NewBotResponse_provider_4 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("Tz")), rsNewBotName = Just (Name {fromName = "\FS\1099171\f.0k\nR3<\DEL\ACK\CANW(&\NAK\14844\1006032.\1091330\r\154561\DC3\ETXx\1011904ZV0\1008029;t\t^y\12071\3235\&82A\143013n\1051560V\1081814l\v\EMe>\96584@\1000979K%Z{\\'\55123VR-[\121253JV\CAN(8N\145464\fHU"}), rsNewBotColour = Just (ColourId {fromColourId = 6}), rsNewBotAssets = Just [(ImageAsset "\b/\12480" (Just AssetComplete)),(ImageAsset "/" (Just AssetComplete)),(ImageAsset "B+0" (Nothing))]}
testObject_NewBotResponse_provider_5 :: NewBotResponse
testObject_NewBotResponse_provider_5 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("\1098386\US")), rsNewBotName = Nothing, rsNewBotColour = Just (ColourId {fromColourId = 6}), rsNewBotAssets = Just [(ImageAsset "" (Nothing))]}

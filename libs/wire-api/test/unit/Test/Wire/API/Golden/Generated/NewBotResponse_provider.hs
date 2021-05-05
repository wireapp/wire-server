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
testObject_NewBotResponse_1 :: NewBotResponse
testObject_NewBotResponse_1 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("/\52309\DELe\ACK\17120")), rsNewBotName = Just (Name {fromName = "\153214\&7Bzb\25773;\1095509<\DLE\NAKy\34018\45947{\EOT{=P\FS\SUB\NUL\146795a\DC4o%'O\58788=jV\1043136o\f\1078472V\RSo\78040\994617\48966\178226\\f<#\"/\1053756>@qI?}"}), rsNewBotColour = Just (ColourId {fromColourId = -7}), rsNewBotAssets = Just []}
testObject_NewBotResponse_2 :: NewBotResponse
testObject_NewBotResponse_2 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("C\FSX")), rsNewBotName = Nothing, rsNewBotColour = Just (ColourId {fromColourId = 6}), rsNewBotAssets = Nothing}
testObject_NewBotResponse_3 :: NewBotResponse
testObject_NewBotResponse_3 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\STX"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\n"}], rsNewBotLastPrekey = (lastPrekey ("-")), rsNewBotName = Just (Name {fromName = "$DGa\145052!\994863\at\128232gl)f0"}), rsNewBotColour = Just (ColourId {fromColourId = 3}), rsNewBotAssets = Nothing}
testObject_NewBotResponse_4 :: NewBotResponse
testObject_NewBotResponse_4 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\EOT"}], rsNewBotLastPrekey = (lastPrekey ("\DEL")), rsNewBotName = Just (Name {fromName = "\134802"}), rsNewBotColour = Just (ColourId {fromColourId = -3}), rsNewBotAssets = Just [(ImageAsset "\1053803q" (Nothing)),(ImageAsset "\ETB\1080327" (Just AssetComplete)),(ImageAsset "\177632" (Nothing)),(ImageAsset "v\1026394" (Just AssetPreview))]}
testObject_NewBotResponse_5 :: NewBotResponse
testObject_NewBotResponse_5 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("h\1015841\167684\167684\RSX")), rsNewBotName = Just (Name {fromName = "\59246\NAKea=u\DLElSE!\vD OX\173371\4438\989662\53996\1023840\95190\tqX\1071327YA\n\EM\1073649\1006559\DC4P\150088\1018854\EM\STX"}), rsNewBotColour = Just (ColourId {fromColourId = -8}), rsNewBotAssets = Just [(ImageAsset "\STX " (Nothing)),(ImageAsset "w\1054489" (Just AssetComplete)),(ImageAsset ">P" (Just AssetComplete))]}

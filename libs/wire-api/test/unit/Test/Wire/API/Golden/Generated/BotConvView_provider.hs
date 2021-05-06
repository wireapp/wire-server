{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.BotConvView_provider where

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
testObject_BotConvView_provider_1 :: BotConvView
testObject_BotConvView_provider_1 = (botConvView ((Id (fromJust (UUID.fromString "00000010-0000-0002-0000-000700000012")))) (Just "%\138012\DC3lrFLJT\DC1") ([]))
testObject_BotConvView_provider_2 :: BotConvView
testObject_BotConvView_provider_2 = (botConvView ((Id (fromJust (UUID.fromString "0000000b-0000-000c-0000-000800000004")))) (Nothing) ([OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "6_gqu7f22qetpb9gczbybpy4o3k5lwr6hg5u1c66z34by6m2p7rxzfupevtwz364b2egffxk75eyp36a6xn0oemeng1i60s6yvozmow1e3ie3wa_7"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))}), omConvRoleName = (fromJust (parseRoleName "l0qbrqxu4g9h1u7jk796c31jb5oszx8wjb28qg8ab2dbsfluxu8y3eiqaikufiswirg82uyfvynztovb550bpu0lw5"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))}), omConvRoleName = (fromJust (parseRoleName "v3c61to"))}]))
testObject_BotConvView_provider_3 :: BotConvView
testObject_BotConvView_provider_3 = (botConvView ((Id (fromJust (UUID.fromString "0000001b-0000-0018-0000-000100000014")))) (Just "GU") ([OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}), omConvRoleName = (fromJust (parseRoleName "q6x1pw5cvvi3w89vuvwvxxy0xbntvs7e38000q9fkt1djjj59w_on0x0qkmo59_ue3kyxdqkoyy3rcdy3jgh896hru1jv9e_dhjty6_f89tavw34h0"))}]))
testObject_BotConvView_provider_4 :: BotConvView
testObject_BotConvView_provider_4 = (botConvView ((Id (fromJust (UUID.fromString "00000011-0000-001d-0000-001e00000017")))) (Just "l") ([OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))}), omConvRoleName = (fromJust (parseRoleName "otnldsvrdrhazaa19e7jy4x7178jo4boixi_vxdqo8uutywzvkjizxuak1r3o6xnbzl9955g1iywopouj5r0w1ovhwalzvkx330cbrmxt5mb"))}]))
testObject_BotConvView_provider_5 :: BotConvView
testObject_BotConvView_provider_5 = (botConvView ((Id (fromJust (UUID.fromString "0000000f-0000-0010-0000-001500000007")))) (Just ")E") ([OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "e11bcimastnt7sk9aizwe1qth50hxmnta4h9s1p6k5uqft2h5d6ps8ulhnm6ka9ggu2ncmntym8baoy2je79t6o_nglwso113ooffx6wed9xvqugp5mbpk68e2e"))}]))

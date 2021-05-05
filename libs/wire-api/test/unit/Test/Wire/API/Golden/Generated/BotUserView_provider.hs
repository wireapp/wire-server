{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.BotUserView_provider where

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
testObject_BotUserView_1 :: BotUserView
testObject_BotUserView_1 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000400000006"))), botUserViewName = Name {fromName = "\ESC\142017\ETX\1006881>.C\1038959=\f\v\vNY\ACKM\9444?m\v\SYN%ko\US\DC4\EOTK\1014051\&9f91htz_D{\RS\DLE}\RS;\986371/Y\184695\27540\fh;\27438&`\170652r\v^\1087529\v\GS1\DELe"}, botUserViewColour = ColourId {fromColourId = -1}, botUserViewHandle = Nothing, botUserViewTeam = Nothing}
testObject_BotUserView_2 :: BotUserView
testObject_BotUserView_2 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000008-0000-0003-0000-000000000008"))), botUserViewName = Name {fromName = "\1056125\50125V+\ETBq<\ETX\983359$\65283I\RS6\NUL\65885c]\f9t\1007804\DC4T#\1067745\1049663ZD\150905\37686B\1060446B\1069623\&0\DLE"}, botUserViewColour = ColourId {fromColourId = -2}, botUserViewHandle = Just (Handle {fromHandle = "1tmnlbt05-g-zbi79m29.qxn3xb.e3kzu1wp4hed"}), botUserViewTeam = Nothing}
testObject_BotUserView_3 :: BotUserView
testObject_BotUserView_3 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000500000003"))), botUserViewName = Name {fromName = "`X?\ETB\bj\1087793\1091305;<\DC2\17045\120715T\13849\NUL\172812\DC2+*\34277\DC2\NUL\DC1f\173182#\31684VV\SYNTg\1091201\1003013\v\DLE4Y[F%S~B;\US ;\US,7\997847\1078145\1088302\DC3\73020/\1013589Y|\"f>I1\8461LS}\1021279H\1081088A\STXPIg7@h\72301\11957o\134200"}, botUserViewColour = ColourId {fromColourId = -3}, botUserViewHandle = Nothing, botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000008-0000-0003-0000-000800000005")))}
testObject_BotUserView_4 :: BotUserView
testObject_BotUserView_4 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000002-0000-0006-0000-000400000002"))), botUserViewName = Name {fromName = "\USwFR9\ACK:vrb\180843\t\46712l\4972\SYNm\FS-\138724>Z\RS"}, botUserViewColour = ColourId {fromColourId = 1}, botUserViewHandle = Nothing, botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000400000000")))}
testObject_BotUserView_5 :: BotUserView
testObject_BotUserView_5 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000007-0000-0005-0000-000300000002"))), botUserViewName = Name {fromName = "R\NAK\1102327t\1103257=8V\CAN\10552+9o\74907\7818\STX\995734~B\\z>\1038450\1096061`h\f@\DC2:\1045282\1047633\&0\167762ea\1091449\DC2id`[\62693pNqK\8068\ETX\1053528c[\1053195\DC1}\1097680Zl"}, botUserViewColour = ColourId {fromColourId = -6}, botUserViewHandle = Just (Handle {fromHandle = "77mwo.uw3"}), botUserViewTeam = Nothing}

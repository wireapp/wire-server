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
testObject_BotUserView_provider_1 :: BotUserView
testObject_BotUserView_provider_1 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000007-0000-0006-0000-000500000003"))), botUserViewName = Name {fromName = "\tJ\1110236,\986653\140246n\v\997921\1042405|\52662h\155805\1037150RE6#\1016322o\fc*+8hc\ETX\95228tOM\DELoY\SOH\1075640T|\44455\&0"}, botUserViewColour = ColourId {fromColourId = 0}, botUserViewHandle = Just (Handle {fromHandle = "k4lnw_t8ns"}), botUserViewTeam = Nothing}
testObject_BotUserView_provider_2 :: BotUserView
testObject_BotUserView_provider_2 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000008-0000-0001-0000-000500000002"))), botUserViewName = Name {fromName = "9X\"\1089357.ko\1080104\150753\ETX!2\"\1095006\SO\1113957j/ld\1067084\1061563\NUL\DC16X_\994066<\1104269\&7\996851b\1110254WP]\994589\ETX\DC4x\DC1\1017812"}, botUserViewColour = ColourId {fromColourId = 2}, botUserViewHandle = Just (Handle {fromHandle = "px"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000008-0000-0003-0000-000100000008")))}
testObject_BotUserView_provider_3 :: BotUserView
testObject_BotUserView_provider_3 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000600000007"))), botUserViewName = Name {fromName = "]\GS\DC4z\28584y\DC4!H\DC4a\131268G7u\73708iK\a\GS\141451\1080444K\ETX\1059432\v\1096535E\roD4>-4]2\72704G\EM\1019279\ETB\1016694U\ACK\135063\NUL\1056673z\t%\1021332\DC1\EOTIEY,9\t\SO\DC4f\RSd0;'\145959\63763\1112731\60440N8\50922\993679\177677\ACKjm+L\1042548\35192\47176\ESCm\fY\1087537x0\1020838T\140970f\995704\DELD!\ENQL\v\984240X\NAK{`\154724p\78522Xz\CAN\1047479b&B'\SYN\GS(\a\SOHgh\r"}, botUserViewColour = ColourId {fromColourId = -5}, botUserViewHandle = Just (Handle {fromHandle = "5s"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0008-0000-000700000007")))}
testObject_BotUserView_provider_4 :: BotUserView
testObject_BotUserView_provider_4 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000700000002"))), botUserViewName = Name {fromName = "=7\1044909\CAN\1040512x\b\1022278@^\GS7Q\28764\128675\1058888v\162589\NAK5\GS\EOT\ETBAL\ETX\ETX\1011311\&0{\191033:qo\"U*\1096979,\134864SKPu\98228oo\"\29670\143181\1017591\21938\94229\1909\1100171V\GS\t|"}, botUserViewColour = ColourId {fromColourId = -6}, botUserViewHandle = Just (Handle {fromHandle = "f-8_7_oc0jy_qazov4xdde8l1qg.4178s80v2g25_by25hc9sbmj-1t0tm6ndrpjw7rvxyiytmu_1qgdw.2p_7h92d9m9hm4w75oih4li3-m7fs6at7x0srt0qz.u"}), botUserViewTeam = Nothing}
testObject_BotUserView_provider_5 :: BotUserView
testObject_BotUserView_provider_5 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000400000004"))), botUserViewName = Name {fromName = "*\1083030\nw\71302\1105652aO\1088116\f\46314\RS h\n-dab\991312\&5sZL\62936\150418V\fS?\t\1104597\&5\NAK6\f"}, botUserViewColour = ColourId {fromColourId = -8}, botUserViewHandle = Nothing, botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0006-0000-000800000004")))}
testObject_BotUserView_provider_6 :: BotUserView
testObject_BotUserView_provider_6 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000400000007"))), botUserViewName = Name {fromName = "]Y\1028493\146041\94784\&5bQP\182111\EOT\ETB\166234(Fa\1063063{\SYNW\1081373\48338b^B\179060\STXQn\ETX\a[,e\1105579f)5LF)\DLE\ENQ>\EOTf{?\b\ENQ\STX\STX\985569u*\1049650\&0xaT \DC2\156152Z\\\48488\1039212m&S\22768X\142275\SUB\DLEg\EMBj_s\169597\ETBw\ACK\1075946Ae\1049734r\120736\1019807\FSu+\rB\EM\1039321\1002591\\"}, botUserViewColour = ColourId {fromColourId = 1}, botUserViewHandle = Just (Handle {fromHandle = "5wxxlr9ift2vjdrt12jv92wqe6egyhl57m4j5xs8a03"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0006-0000-000000000007")))}
testObject_BotUserView_provider_7 :: BotUserView
testObject_BotUserView_provider_7 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000800000006"))), botUserViewName = Name {fromName = "3\1019517(\ETB\4560\ACKb\180238^\1045925\FS\1029483\34832~deO;&a\ACK\1100600T\1099720/|\b\a@\67270\147380hty@\n\1086881v\FS\1024949\25350\146888\&7\183473/Z\ACK\1056026}o)\"4hp\RSk\US\ACKc\1078673V\1044755\1045671*J*c!\984670\129035*C`\SUB\DLE\23873\ACKuLH<Y\GSA\DEL\1007941\1101032\DC4"}, botUserViewColour = ColourId {fromColourId = 6}, botUserViewHandle = Just (Handle {fromHandle = "3ev0v6xpwx5b779qogax19w..bq-pukwg79h_d2255zyt10fwc1imn5t_5h89a_y.wv32qzbr2_br6f4_8qs9v5nimsjfb9fsukvwvnawqowln4n3x-mg3m6sa.4p0u_g48twkas4yq0tbsu2myang9hurwhojjkjmhltpih7-gjirelcx1dn88my375i4abca93skfx-u-f7v1-zuywef1.-3-pxhf9.fgje.ijv4_-o._nplwzph4og7.2t"}), botUserViewTeam = Nothing}
testObject_BotUserView_provider_8 :: BotUserView
testObject_BotUserView_provider_8 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000006-0000-0007-0000-000600000005"))), botUserViewName = Name {fromName = "B<C\42523vev=.man\165715\SYN\188666$\n\185673\SOH\174874\1027149\&93B\131753\140594\990285;^F_&B7$\1089098a\1015416A\ENQHg\DC19\SUB\DLE\1044890\1005524&W\132423!\62629\SO\1028597@@y{9\USS+;VW\72147\STXu\100364V\138092vrQkX\147880\DELZ\SO\SUB\1014701M#\ETX{\STXc;\SOJF\189888\18934\tJ\STX\157874\8606\138291_\DC3\997897]\99345\&19\1083367]\DC1/\172191[3\a\991692\a\39707(S<\30835\\}L"}, botUserViewColour = ColourId {fromColourId = -2}, botUserViewHandle = Nothing, botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000800000007")))}
testObject_BotUserView_provider_9 :: BotUserView
testObject_BotUserView_provider_9 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000004-0000-0006-0000-000400000005"))), botUserViewName = Name {fromName = "a\DC3CT\SI\ACK\1104155 \DLEGQ!E-\r H\183710bL\1108280H\136657\1020705ULf\47205\23839\191330\&1\1054753\DC3\30297\FS\STX.pXt\EOTA\7793\DLE8ZE\t_\20339\1047214S?\100951\1003718^<\NUL\4908G4\1035635\176466HtwB[q`]\97808\1019713E\987588]\1053670\999350B\1094469U\CAN\ESC\ETBpy\1087541\\\94087\&7\1059525\1063888D\999920wi\1080779v\147374"}, botUserViewColour = ColourId {fromColourId = 5}, botUserViewHandle = Just (Handle {fromHandle = "ytsx-anqq9rwyt7u1zyox7rb5_p26t41s6ih6g-14uqaghvflc3n64liajkdcj2u-vey2ippsy82fulof67erpq.kpk1oo618jjgo4t.phq84tnv.naarqedqb-yfn-s70c6vms.hgrmz.g7h460i_a3wiuj64boyp"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0008-0000-000200000005")))}
testObject_BotUserView_provider_10 :: BotUserView
testObject_BotUserView_provider_10 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000200000000"))), botUserViewName = Name {fromName = "\1055122$\a5W\vm_\ENQ?Fcm\1075521vmM\1099233bm?|\DLE'\NAKmq\1028312\&8'\148693\1066395\b\EOT\bE1\1049340\SO\DC1@\US\62106S$\f`\49787"}, botUserViewColour = ColourId {fromColourId = -7}, botUserViewHandle = Just (Handle {fromHandle = "wwrg7xt"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0005-0000-000300000006")))}
testObject_BotUserView_provider_11 :: BotUserView
testObject_BotUserView_provider_11 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000006-0000-0000-0000-000000000002"))), botUserViewName = Name {fromName = "\t\150071C+R\1012472\f[$%\US1\1090765\ESC"}, botUserViewColour = ColourId {fromColourId = 4}, botUserViewHandle = Just (Handle {fromHandle = "ym.bs7q6iwwwixj9-mj89cbabmvigpkz5bkgo2f6r0x_ku2m2dgx01ltb7-9-gbo2y7_dbjmvxkbr1pu-lb0a1orz2bocmup2s_636"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0004-0000-000100000004")))}
testObject_BotUserView_provider_12 :: BotUserView
testObject_BotUserView_provider_12 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000007-0000-0002-0000-000200000002"))), botUserViewName = Name {fromName = "\RS\1050237\DC4\9678@\b\ESCE\a\CANCD\SOH/\138708\1055189\br9&I\983974g\EOT\43837X\42274}Non\156954F<GX=w\183099\&3\NUL\10767\1068729\DLE\1070358x0=w\FS\1097083\141593`'}\NAK\170018\&0<Zj\b\1096836\&4\aX\2318<s\141395!\1048511ax2\1040244\189273]\t\NULv\1086770|7{\DC2\v9T\DEL\SOH\1015707\47657\SI0\1020097)SzB\1023326\1005555j\v/.\ESC\999034?!F\1024245A\NULA\ACK\153134-'b"}, botUserViewColour = ColourId {fromColourId = -4}, botUserViewHandle = Just (Handle {fromHandle = "2pskhies48v-8_5hockmyf9c6x50tlqg_j7-l6_18tkqax20zdedej1-r4yz"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000200000004")))}
testObject_BotUserView_provider_13 :: BotUserView
testObject_BotUserView_provider_13 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000003-0000-0006-0000-000600000004"))), botUserViewName = Name {fromName = "%\63254ac\b\"uA\EM\"\SUBgZ\1065376\SOH@\DC3a\US\1073181D*\STX6Ie\US\1050054\98696w\ESC\SUBv\1053162!\ENQ\1051054O\1092967\ENQ\1006688\ENQ,\ETX\144001h3;&\179032\1103212e\126530F`G\n \1068548\138634\1010382RE$!v\ACKIN\135683k#I\v0T\DELN\190654\fq\111355\ETX_9\tL\US\30693\&2\996182\US\v\1087533\US<5LuZ\153864\1019979<\174752\DC1"}, botUserViewColour = ColourId {fromColourId = -4}, botUserViewHandle = Just (Handle {fromHandle = "wn-zt-"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000007-0000-0008-0000-000200000005")))}
testObject_BotUserView_provider_14 :: BotUserView
testObject_BotUserView_provider_14 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000000-0000-0008-0000-000200000004"))), botUserViewName = Name {fromName = "i\SOH\162921\&9-{w]\133808\59847>-/GF3A\DLE\113766\1092122dI\f\11916\NAKQ\DC1\1028719\12120E`FN\NUL\58646p\3369v(\23408L}5d}p\SOH\DC3\EOT\1044314\GSj\1039872\US$*\96246UX-0I\1048772\&4\DC1\SIX\984932*\145178[\SO\DC4pv \1026826?eD\3343\1101656NRKH\178771-\121328\1031609e}sa"}, botUserViewColour = ColourId {fromColourId = 4}, botUserViewHandle = Just (Handle {fromHandle = "jw1zx1j5s"}), botUserViewTeam = Nothing}
testObject_BotUserView_provider_15 :: BotUserView
testObject_BotUserView_provider_15 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000008-0000-0007-0000-000400000006"))), botUserViewName = Name {fromName = "4 2:\141488\1027715\ETBB\FS\fRqhJ\NUL 0\FSA\FS\1023141\1089291\11238.\ETX\EM#eyBR\1048757\EOT\vj&T\1099863\&3C#=D[~L\147604I\988479}\1030508E\ETX@/\46325Fu$U\183120\990707\1049145z/\1026377\&07\"\155551J\SYN\CANR\ETX\ENQz\173588\f<[S"}, botUserViewColour = ColourId {fromColourId = -4}, botUserViewHandle = Just (Handle {fromHandle = "dnbbkryme-.dgz8c450r.x3yecmp60myqhnisk4yt4__2ev8yc_o6xz8.7.y7ngds9w6_-.q7gur327dkn4_.5q0ipyi66"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000000000003")))}
testObject_BotUserView_provider_16 :: BotUserView
testObject_BotUserView_provider_16 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000200000001"))), botUserViewName = Name {fromName = "\1050008\RS$\FS0\SYN\bK\ETB\1014182]\ACK\1049716\nu\194816r\165968\64380M4\165850gQ\f\ACK\SOr\137725\SO)7\b\DLE\NAK\az\36087 \1006581r8\DC1\1075590*\47390Xp\58812\1066783\302\FS \1034596\173334\SUB\1059306\133951z\177374\1097057\1105389t?3)\32586\1071454 \tz!\n\37872C\1032794\SOHP>\STX\147017`B\168362q=p3%~J\ESC\SI\rr?+Dmm\NUL\147788`H\1076529\vP(\STXI"}, botUserViewColour = ColourId {fromColourId = 4}, botUserViewHandle = Just (Handle {fromHandle = "bmp2fnha__uke-hm8imt79mn-nr2gvr2z1yp5rwa_8msul-43mo0.gkm2lnq7lhi3hwb7wandyeubkll9lsj9wltrruz8a3r4qi_2ybm_b4c89tr3-wv6swchrnxstutwln"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0008-0000-000400000007")))}
testObject_BotUserView_provider_17 :: BotUserView
testObject_BotUserView_provider_17 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000007-0000-0007-0000-000000000008"))), botUserViewName = Name {fromName = "\176613Or\97218h\1025472\EMD\EMW\SOB\51746\f\SOr\128076/7\FS\32463\1036898\1083146)\15240\SOHKr>\120674z;tv\13886\997206$\DC2\te]b*M\FS<K\1039070Lo#j\66891V\DC4\1102935\&6\1041755\&62& \1103138\1036953Y\DEL\ESC\1080928\ENQ\ENQB\161234i0<L\1091194\61114\1032872\31521\1007705T\DC26\142241\1084857\1007426p1L\986157u\DC4}W\994351+)"}, botUserViewColour = ColourId {fromColourId = 0}, botUserViewHandle = Nothing, botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002")))}
testObject_BotUserView_provider_18 :: BotUserView
testObject_BotUserView_provider_18 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000000000000"))), botUserViewName = Name {fromName = "\DC4d\99386"}, botUserViewColour = ColourId {fromColourId = 1}, botUserViewHandle = Just (Handle {fromHandle = "fgskuwoz_blsbc6e9-dyfp-hi5oatf1ygf.e89f81.rjmf_ag8mt0fprpund1arj31pged5xq7p5oxjc9o6iczeyj_eabd_acr_4fa1dm0cp-fwker-8kk0y-b_dhtdqt5k72v6t81kec_n0c7i7xfqcjttke1-g4d5k1qv5xiglqwmcesjjx1to0-pv-6pcmbnj84xp5v94z"}), botUserViewTeam = Nothing}
testObject_BotUserView_provider_19 :: BotUserView
testObject_BotUserView_provider_19 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000007-0000-0001-0000-000600000000"))), botUserViewName = Name {fromName = "\FS5a\NUL\fO\92949\&27s*:b\SOHMY\"Bj8\1071624GT\DLE\3792P\SYN\998598\1015038\NUL\"("}, botUserViewColour = ColourId {fromColourId = 8}, botUserViewHandle = Nothing, botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000800000003")))}
testObject_BotUserView_provider_20 :: BotUserView
testObject_BotUserView_provider_20 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000800000007"))), botUserViewName = Name {fromName = "\20160\25273m\1081898\&2-}n@l\3142\SYNs<z\USx*\42607+pR\GS,UJp\993562HH\159735\&3^A\f9\1075626$;a\145703\&4\29743bOX\US\1090705\1077768p|MO\DC4\SI3\22964\&6,\1064658\NUL\1111340{\SOH"}, botUserViewColour = ColourId {fromColourId = 5}, botUserViewHandle = Just (Handle {fromHandle = "44-__lipwo"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000003-0000-0006-0000-000800000004")))}

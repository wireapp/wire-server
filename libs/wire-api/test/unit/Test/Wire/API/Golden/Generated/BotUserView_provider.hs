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
testObject_BotUserView_provider_1 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000006-0000-0003-0000-000800000003"))), botUserViewName = Name {fromName = "H\183652\1040373y\DEL\58995]>/\n\tHmDu\RS\49329ot\138869\&5\EOT6O\vm+&f\bQ\RS\NAK\1027927\&5\1072209e8\CANn\SOH\GS\CANnD\1037064\&9Z\1078389]\39999`V\1059700Q/"}, botUserViewColour = ColourId {fromColourId = -2}, botUserViewHandle = Just (Handle {fromHandle = "uc1r"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000003-0000-0008-0000-000300000007")))}
testObject_BotUserView_provider_2 :: BotUserView
testObject_BotUserView_provider_2 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000001-0000-0007-0000-000600000004"))), botUserViewName = Name {fromName = "iS\RSD\1032336S\70829D.W\1050999p-1yr\131488~\1043556`\DEL\1005397\1063700!Z\1029568\rGcS7K}\162646\DC2X\ESC_m>5\"@\SI8I\16954\1087650\999250t\121180MH-N^\1000159&;\SYN\STX\ETB\139259\f6\38733\1075356\92356xRc\183036\&0{6*"}, botUserViewColour = ColourId {fromColourId = -6}, botUserViewHandle = Nothing, botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000700000008")))}
testObject_BotUserView_provider_3 :: BotUserView
testObject_BotUserView_provider_3 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000007-0000-0006-0000-000300000005"))), botUserViewName = Name {fromName = "t\\'[\t\STX\NUL\DLEz\1104371;AH\1020447\1041957$Pb\b\EOT\94360\DC1M4?-yYD=\1024225+\1089830\1043160\ETX;LM\1040239\&7\1017430\ETX\178982\SO3\1062269y\186644\US"}, botUserViewColour = ColourId {fromColourId = -4}, botUserViewHandle = Just (Handle {fromHandle = "y_kd394u51cxhavivtoxz77950yw-.l6yz_8cg-acmcrz5aorfo8wy1g1m7p2_x2771w10vfa1f7t_ktfwf5ge1bi5xv9q3v0u_y7rza3dnysgn.6wx2imos0cyi5hi8ix3f5e_dx-nl5j_4vs0478_bgtva6tvjv760u1m5wx_6a76zzy35co.94gqa3-hpwqyp"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000800000006")))}
testObject_BotUserView_provider_4 :: BotUserView
testObject_BotUserView_provider_4 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000003-0000-0007-0000-000800000008"))), botUserViewName = Name {fromName = "\1009150s\47844\DC3-\1009058\1059285\SYN\SI\SI\23303\"\986202K\GSz]F\48934bk\EMZKZ,\t!\RSCd\1018077*yN\DC1\SO\ETX\SOH\ETBa0\SOY\b&M\188142\99611XH\128477I"}, botUserViewColour = ColourId {fromColourId = 4}, botUserViewHandle = Just (Handle {fromHandle = "2kv_h94zxw_nh33hdww9t1im.fcxutd0ig80b9n9711i.sph3t.ka_c-tz5u2vvwi1_42fv.b9yj7256a64jw.a3dm.4er3_vghibh6vkysm3gtj2k8h07m6cta4v9gnqljo95af3_a2owhe_6xz6_gfn4hh6ii_cd3o.fk8qqms54j90-.vawb7o451k593dn3ezi_5cyg69s90j0hb-53pm0g52g-e7.syxtp4d_mzkmj.9y6fg"}), botUserViewTeam = Nothing}
testObject_BotUserView_provider_5 :: BotUserView
testObject_BotUserView_provider_5 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000004"))), botUserViewName = Name {fromName = "\EMr\1047544\SUBP\DEL3O4\EOT\1094393\ENQ\1020336H\1090673!\1037775p\SUB\1049433\&02c\v*\28110h<\1075122#7Cc\129046;\b\t\ETBv\1051150/M\SUB3V\CAN\ESC\44542@\1101711\&00\1110897\168542\"\1072429\1007075\44725JTprj?+\SUBw%'8aH\1092146\DEL\EOT"}, botUserViewColour = ColourId {fromColourId = 5}, botUserViewHandle = Just (Handle {fromHandle = "aeb_9.17chqc_xu4b6ucd9wtkxxn2cds2mrkzze1bm7uejw4p1x0m0rj2rj9xiq-kahh8qi_nb4ol.7614xatq_lvl2o.-ymc1y7r.d3zyri-e65r-1zheie8.pm0c9djjk3x_nxl6j94untd7s63oj631esb8"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000003-0000-0005-0000-000600000008")))}
testObject_BotUserView_provider_6 :: BotUserView
testObject_BotUserView_provider_6 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000004-0000-0006-0000-000200000008"))), botUserViewName = Name {fromName = "\137540<-\1096071$\b&\94911\f\DC4\98794.\">cL\51385)\181424~:k~\153486\95141\ETBW\b\1030888{\EOT0t\"\RSb\DLEn\152501\128499\1019043ft\38357\175494\991889\1011050g\DC22\n\994585>\SI\"*\1106226}\SYN\1078487Al]D\r.\986197.b?\992934\ACKqK8\1073652%"}, botUserViewColour = ColourId {fromColourId = 2}, botUserViewHandle = Nothing, botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000005-0000-0003-0000-000400000004")))}
testObject_BotUserView_provider_7 :: BotUserView
testObject_BotUserView_provider_7 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000100000006"))), botUserViewName = Name {fromName = "H{\1073755\44524K_\1067223[\1090377:o^ZM\DLE\DELj&#\\e\1009265\23617\38601T8\NULD\1022314|L\DC1\DC1r\337\1099553\1050771R\NUL\21815\140762K|2;\STX=y\SOHI7:\100640\r|;@MSk\NAK\24861R\fgn\1023298\ETX\DLE -\SUB'\fcae\DLE\1053547+\SO\ETB"}, botUserViewColour = ColourId {fromColourId = -4}, botUserViewHandle = Just (Handle {fromHandle = "b_3_ft-.xnn9x-fhcuw0hr3.4nmw63jzwok332nzzbu5q4.nbpm.wki5-kfk0r6f4yob1sspi8ct0quc4ms11h8l9mk5edjang78.cjfb_8u7lde0p_4.5kg695sra9cl2ab_fxrc0u38omvrislhs4sg3311d_f66rfami2qho4ohbv7begjlq3kf8dap6wpws85.9g_pzq_k0.gx6lde7790zuy3myp-zd3zxa14sly5g_"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000700000008")))}
testObject_BotUserView_provider_8 :: BotUserView
testObject_BotUserView_provider_8 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000500000001"))), botUserViewName = Name {fromName = "\ENQEy\SUB\DC4\ESC\1036011\ETB\DLEW\25768S\DC37%b^\171693\60686=[\151929\fw>i7s\1075391#\1078811w2#\97293l_\a\190101`13\20707\&8\1022282N`Dx\185872\99737\ACK&C\tk8\1035100\1072081\165582-t!'A0X)\20030\DC4\DC2"}, botUserViewColour = ColourId {fromColourId = -4}, botUserViewHandle = Just (Handle {fromHandle = "t2h4zuf9fa9x6b43fbhs17tbd7yi.ivhy28c6_rgmxvxt3xp_b6za57oe70yucwnoilgju4u-o960glxp.aaepngs_gteeg.w.0o6zu8-zjn4uh57r_lcv6ffwsvm0oif5dtjuqay92q-oremtyii8yixh1nb1j_68xygc505dopsie8lyop.3x4js5ri9qkswgchhz-cmvfhl5o77.vbjm_8-5377tm6_lvhiun8-k3x"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000700000006")))}
testObject_BotUserView_provider_9 :: BotUserView
testObject_BotUserView_provider_9 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000100000001"))), botUserViewName = Name {fromName = "\9443\1063609\171748\173060yNr\12948\r=\990481\f\RS_#Y\v\ETX8\1030251l\1062443\ESCf\r\128502uJ\1078009\63191\1013140\23158ht\8051}!\989646 \FS\a\1018372\SOHis)\DC2\156829=Kd\NULk\DC4\1074584C\97709~$~\ac&PLR8V\b#\1063692\1058009\16024wlS\SUB&z\\\2586\DC4x"}, botUserViewColour = ColourId {fromColourId = -2}, botUserViewHandle = Just (Handle {fromHandle = "lpvk"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000005-0000-0004-0000-000600000008")))}
testObject_BotUserView_provider_10 :: BotUserView
testObject_BotUserView_provider_10 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000600000005"))), botUserViewName = Name {fromName = "\ETB;\996252\1109913\&3\1087507\1008167%M\70731ff\DC1\1053039\1048537(MY\v\1041865/\DC1\DC1,}#\SOH\175545\USU\US\1071960\US\r\RS \1082010n\SUBc(\1073357\ESC~i\np\1038645\15557vVM7eb\142996-\1090804Y\r\1007825u\41678\153067\745r\DC2\1112443Fm(\157959\"z w\163672\NUL\DELf\166320\&8]\1040059\DLE\153585\EM>\RS-\1086447\1081529T\991183\1098602\&1GZAj"}, botUserViewColour = ColourId {fromColourId = -1}, botUserViewHandle = Just (Handle {fromHandle = "h-7y7c2"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0006-0000-000800000008")))}
testObject_BotUserView_provider_11 :: BotUserView
testObject_BotUserView_provider_11 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000006-0000-0004-0000-000200000003"))), botUserViewName = Name {fromName = "#Vph\NAK\1064720\ACK/\83340)&sZ\161359\160408\&07\CANQ\1031623[k\nTZS(X]\17317\1025680yG\SYN\97411\983372\1035128lucQ\990183\NAKiI6"}, botUserViewColour = ColourId {fromColourId = 5}, botUserViewHandle = Nothing, botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001")))}
testObject_BotUserView_provider_12 :: BotUserView
testObject_BotUserView_provider_12 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000300000003"))), botUserViewName = Name {fromName = ")wF}&\SUB\1018653\&8\988625\ETB\73746 8&2\ESC\1095121E\1059297\&4\140070{[\US\US\FS|\133248\rMm;FI\180920y\992194D\1017728\43978!A\11460"}, botUserViewColour = ColourId {fromColourId = 6}, botUserViewHandle = Just (Handle {fromHandle = "2-dwt1tu9rl4nl3zjjr_x.i_rmuiqx2.iixz3bvj3hztt1urxt3s606w4qn..x1rzonq3hagktv08tavj_.ems.u2sw.1-2dxxvu-etgb2enx4h2vzit3ks34b4bxqse6yazd4xp2n9v.f_o52l7wzn140_2nmg6g4wsyd08.d4w6f_u2rncdrhje3cgyt4c94r3-j31mdq0pnuhxq62678_oe1skb8d79mfvdgkm0v2wsx68"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000700000007")))}
testObject_BotUserView_provider_13 :: BotUserView
testObject_BotUserView_provider_13 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000500000002"))), botUserViewName = Name {fromName = ",4 \FS6C!YNbDW\DEL\42469\&52UWJ,$\21583\CAND\1065923\ETX\16805*X.|"}, botUserViewColour = ColourId {fromColourId = 0}, botUserViewHandle = Just (Handle {fromHandle = "3vs0ovwbn5kaew2e6imm5s1_91kfff2gkwvdhpi9yssqnfeqc5w500jmci5ndkmjkxgk.2p-3.ffraf7kw6l.s_yr_q.gv4w5tkzuptbk0_59y."}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0008-0000-000400000006")))}
testObject_BotUserView_provider_14 :: BotUserView
testObject_BotUserView_provider_14 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000600000003"))), botUserViewName = Name {fromName = "\SYNN~\155434c\98963\&5\26880+\186050\61331YX\ETBYF\58810{/B\1017644\23902\1055344\NAK\STXmI$ 1cd\31966\1059679\94923$y4wA,\RSs\RS\175255J%fE\58609ya\1003510*Dod}\NAK\1106038^\172766\SUB\1044349\1005311?\1091158#hy`/\1061550\1032715Z\NAKgjL7\25274\DLE^\1030117\GS#|"}, botUserViewColour = ColourId {fromColourId = -2}, botUserViewHandle = Nothing, botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000003-0000-0007-0000-000300000001")))}
testObject_BotUserView_provider_15 :: BotUserView
testObject_BotUserView_provider_15 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000005-0000-0001-0000-000800000002"))), botUserViewName = Name {fromName = "x\1094467\&9\160013#R]Sa\174690\SYN\EM\1004075\f\aZ`#\169548x5w)S\60679\168787;I"}, botUserViewColour = ColourId {fromColourId = 3}, botUserViewHandle = Just (Handle {fromHandle = "w92dug5kck_xggrymxmypw4nf7jq9stg-qlq13c4.h5s0mfrx638o6j7qzleo2ix95ixhnn70ichz.yjl6d2l1a2mcv-m5l872v8dnxoncqk615fel107mjl9k-o9_u70u-v1-62mcd88drjiptl34d.nq9-u2u-92ht150pxuhw-3xt6xsuu5"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000200000001")))}
testObject_BotUserView_provider_16 :: BotUserView
testObject_BotUserView_provider_16 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000700000008"))), botUserViewName = Name {fromName = "\1095210\1086436\1044067V+a\1066624\50097E8\1000682 \1050046U^By\1113179\b\DLEBjE\164170\RS\\\1091516Se\998540Mi\ENQ\70098z\r!y\NUL`\ETB\ESC\1000389\128935#r\"Ha9k\SYN\50982\&0G\DC2"}, botUserViewColour = ColourId {fromColourId = -1}, botUserViewHandle = Just (Handle {fromHandle = "y.j6"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0007-0000-000600000008")))}
testObject_BotUserView_provider_17 :: BotUserView
testObject_BotUserView_provider_17 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000500000003"))), botUserViewName = Name {fromName = ",%\EOTaX\GS2p\SOH\STX\v\SI&\51106\&2@\v\ETX\1060281\57623\11610\1011888k"}, botUserViewColour = ColourId {fromColourId = -4}, botUserViewHandle = Nothing, botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000004-0000-0006-0000-000200000006")))}
testObject_BotUserView_provider_18 :: BotUserView
testObject_BotUserView_provider_18 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000005-0000-0008-0000-000100000006"))), botUserViewName = Name {fromName = "n5r}\38083\189351J\b\37489\1073410K'' ?Q4H\162108Z\989231pfg\CAN\RS\ESCu\\'L1\t$\129460[\SYN\EM%RVLWh\ENQ\1014586k\bf./+\ESCVL\1062922^\"\a)ZY\172649\DC4\SO\RSy7\DC4\1110469d\1100651'[j\1060968U\168434\&8?Pt-inL\994698Z\1018097\1094670vD\ETB\tip.\SI\1013986Ze\v\\\ACK\147717fW\24912#\161339%"}, botUserViewColour = ColourId {fromColourId = -8}, botUserViewHandle = Just (Handle {fromHandle = "2uaxl_4en5"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001")))}
testObject_BotUserView_provider_19 :: BotUserView
testObject_BotUserView_provider_19 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000600000005"))), botUserViewName = Name {fromName = "h\EM\43645\r\DC3\1085597pDc\189751?8\150876\1072159\176462\&4%+\166208\986248v\ACK\1000269-\171804~s[\1043899\135172P \58203VI\48347!;0"}, botUserViewColour = ColourId {fromColourId = 5}, botUserViewHandle = Nothing, botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000007-0000-0002-0000-000600000006")))}
testObject_BotUserView_provider_20 :: BotUserView
testObject_BotUserView_provider_20 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000007-0000-0004-0000-000500000004"))), botUserViewName = Name {fromName = "\194856\1002895r\RS\FS\985003@\1034842nhC\ACKc\RSERPu\fi%\1074065s\3448XK\32396iX\99063rWu\83370l!\1021694\ENQ{s0d\51367\&8\33088\&0\DLEAG[\21321a\1066853\DC3<P5\NAK+/Mfi`"}, botUserViewColour = ColourId {fromColourId = 5}, botUserViewHandle = Just (Handle {fromHandle = "chih_aszfrla1qk1ul6ia40fvykylp98x.duu33nyhwrq7y6412jzmm4g4s_6equ35_k6u-dvsb76vjsuh74p7zwkuqpckvq9dvmbyps12.bv8vq1m0s83opgz_l0pn05.jqh8p2fs97oxwdptbehabqxtn266wgi0xq_-jcagf5ve9rgps5qjzkd405jhi08tzbl9bf8t"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000007-0000-0001-0000-000000000007")))}

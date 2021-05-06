{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Event_team where
import Control.Lens ( (.~) )
import Data.Id ( Id(Id) )
import Data.Range ( unsafeRange )
import Imports ( Maybe(Just, Nothing), read, (&), fromJust )
import qualified Data.UUID as UUID ( fromString )
import GHC.Exts ( IsList(fromList) )
import Wire.API.Event.Team
    ( eventData,
      newEvent,
      Event,
      EventData(EdMemberUpdate, EdMemberLeave, EdTeamCreate,
                EdTeamUpdate, EdConvCreate, EdConvDelete),
      EventType(MemberUpdate, MemberLeave, TeamCreate, TeamDelete,
                TeamUpdate, ConvCreate, ConvDelete) )
import Wire.API.Team
    ( TeamUpdateData(TeamUpdateData, _nameUpdate, _iconUpdate,
                     _iconKeyUpdate),
      newTeam,
      teamIconKey,
      TeamBinding(Binding, NonBinding) )
import Wire.API.Team.Permission
    ( Permissions(Permissions, _self, _copy),
      Perm(GetTeamConversations, CreateConversation,
           DoNotUseDeprecatedDeleteConversation, AddTeamMember,
           RemoveTeamMember, DoNotUseDeprecatedModifyConvName, GetBilling,
           SetBilling, SetTeamData, GetMemberPermissions,
           SetMemberPermissions) )

testObject_Event_team_1 :: Event
testObject_Event_team_1 = (newEvent (TeamUpdate) ((Id (fromJust (UUID.fromString "00007525-0000-6b54-0000-66c8000062ec")))) (read ("1864-05-19 17:25:34.44993912091 UTC")) & eventData .~ (Just (EdTeamUpdate (TeamUpdateData {_nameUpdate = Just (unsafeRange ("z9u\DC4/+I\t\DC2P\191234\1069462\984993\SOH*c\1073904{\184421c*\177651\164209\175593\1038728\1024593\71847\&7\1085445!\1070673W\CANb7\96950\988775\ETB\1038209\194560\133541~>\1381!\998249]\CAN?\144431(n\ESC\n\ap&\EM_6z\f")), _iconUpdate = Nothing, _iconKeyUpdate = Nothing}))))
testObject_Event_team_2 :: Event
testObject_Event_team_2 = (newEvent (TeamUpdate) ((Id (fromJust (UUID.fromString "00007882-0000-24e9-0000-771400000e66")))) (read ("1864-05-17 22:13:00.289913538537 UTC")) & eventData .~ (Just (EdTeamUpdate (TeamUpdateData {_nameUpdate = Nothing, _iconUpdate = Just (unsafeRange ("*\70203\1006657UU%B57s\144631g,k\1102734\993142i@V\t=\1011790>\b\GSN\1039258i\b\27518\988302U%\1003595\69906\1012817\147545+\tA\\\77828'@Md]M4\b\SYN\DEL\98491v-\29133\SI\34518\1027062t)m\16661r^Q\1000350o\ACK\SYNK\29572\1069463X7g#K]J&\SI\9345\1109772\"oV\182206\74515G\ESC\1066183\SYN\1089542f\1013343CdaXp\120825j\94737+ZJn\10274S\1047382\1087672{M\DLE\73923\1079109~\111334\n\95379&c]\173969a\DEL\21333UX+8\1044335\SO\SOV\1036426\FS\CANK9<\1057978{W\1045623\&9\RS\45048\t|j>r\9413\ACKW\SOE\16128\&8\98145&\1009844\59300\1103238\DC2IX^-_W\ACK;n;H0\38490\f\78856\SI\1019080*\164801\178192\ACK\STX\EMg~3 1Wn\1054287\191375\CAN\fThK\DEL)mG\SOH\a\ENQ>\fyW\182305)\STX\DLE\SO\1018070n\US\135617\b\154307\SI'T-\SYN\94344\1021064Iza\1993yyM\83333-X\142738{\STX?\1061501\171399*o-<")), _iconKeyUpdate = Just (unsafeRange ("\SOH7vm.\SUBU;%_L\DC4\1031188\SYN\1107142\1079244\165796\DC1\ESC[\RS\DC1k\1095184\188346\177589\152478c\STX\163313\ESC\SOH^O\t\38633ioqa\133468\1003301\38168o\71208\1068737K\996381w\1102930\129078\188577V]\7886g7\985120z<:c\GSmC\v\157247h\5841\189621J\ACK!b\DC3fC8\EOT\990586W\SIY\1060068\\{j\158232f\DC2V\173306V=c9d \20709\1051995T\SOH\DC49\DC3(q\172519s\ETB\DC2\EOT\988743\SOHI$0W#X<\ESC\ENQ\SOP Iv)\46653\139071<}n00\20706'\1084413\1097458\&1t"))}))))
testObject_Event_team_3 :: Event
testObject_Event_team_3 = (newEvent (MemberLeave) ((Id (fromJust (UUID.fromString "000079d9-0000-4b7e-0000-733300001681")))) (read ("1864-05-04 05:05:49.026787247901 UTC")) & eventData .~ (Just (EdMemberLeave (Id (fromJust (UUID.fromString "0000448f-0000-4f23-0000-2d1d00000140"))))))
testObject_Event_team_4 :: Event
testObject_Event_team_4 = (newEvent (TeamCreate) ((Id (fromJust (UUID.fromString "00004217-0000-133e-0000-6b87000003e3")))) (read ("1864-05-07 17:58:12.891593156775 UTC")) & eventData .~ (Just (EdTeamCreate (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000000000004")))) ((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000400000001")))) ("\1072535p&,\RS") ("jI\f\1054392") (NonBinding) & teamIconKey .~ (Just "\1668")))))
testObject_Event_team_5 :: Event
testObject_Event_team_5 = (newEvent (ConvDelete) ((Id (fromJust (UUID.fromString "00005f87-0000-6929-0000-39d1000079c1")))) (read ("1864-05-27 16:57:39.038473676504 UTC")) & eventData .~ (Just (EdConvDelete (Id (fromJust (UUID.fromString "000068e6-0000-0d46-0000-62a7000070f7"))))))
testObject_Event_team_6 :: Event
testObject_Event_team_6 = (newEvent (TeamDelete) ((Id (fromJust (UUID.fromString "00001df0-0000-5c03-0000-1f6300004d3c")))) (read ("1864-05-31 23:39:09.078152334666 UTC")) & eventData .~ (Nothing))
testObject_Event_team_7 :: Event
testObject_Event_team_7 = (newEvent (TeamDelete) ((Id (fromJust (UUID.fromString "0000214c-0000-6cb7-0000-261d00004b45")))) (read ("1864-04-21 02:41:21.701985303576 UTC")) & eventData .~ (Nothing))
testObject_Event_team_8 :: Event
testObject_Event_team_8 = (newEvent (TeamCreate) ((Id (fromJust (UUID.fromString "0000418f-0000-2403-0000-231b000016a7")))) (read ("1864-05-22 16:55:26.077518461483 UTC")) & eventData .~ (Just (EdTeamCreate (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000100000001")))) ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001")))) ("\1075221\1061556\a") (".\r") (Binding) & teamIconKey .~ (Just "L7`")))))
testObject_Event_team_9 :: Event
testObject_Event_team_9 = (newEvent (MemberLeave) ((Id (fromJust (UUID.fromString "000010dd-0000-5605-0000-2c1500002027")))) (read ("1864-06-05 04:14:45.127354015507 UTC")) & eventData .~ (Just (EdMemberLeave (Id (fromJust (UUID.fromString "00007982-0000-2a4f-0000-0bd700001312"))))))
testObject_Event_team_10 :: Event
testObject_Event_team_10 = (newEvent (TeamUpdate) ((Id (fromJust (UUID.fromString "00003830-0000-2e98-0000-187300005b65")))) (read ("1864-05-10 05:08:29.831099818664 UTC")) & eventData .~ (Just (EdTeamUpdate (TeamUpdateData {_nameUpdate = Just (unsafeRange ("\140428hi\44805VqA\"#\586\&5\ETX\188958#d4q\166156Z)D\1112122\1065100\70025\EMdq\EOTB\1037584y|Z\996452\DC2Xo#Q\"9r61\119227@\ay$\162530g=\SO-\153369W\nR\ETX\17520\STX\172706\RS|\142931)<\65669\1031590\SI\"\ESC4\DLE{\129144oh>od\v\FSx(\CAN?f/&\62549#*r \143393\1094143\69677\1010635.>\NAK\997273\&8\1003960(\1034428|\tV\6388\DLE\4782\153292\69939\1080223\ETB\n:E\NUL\45370\ETX=g\1069862qH\131905\NUL\134583V\1021947\22564*\\\151590\ACK\39152}\984358\97923\&5H/\1039949<\1048268p+\154672>^S\1077438~b\983335\194804Z1 \1045339\140671\a\US\FS\SUBu\DLE\1011113Y\46769\1102859\"x\GSG\US\SI\984446\SYN;D[\ETB\1107579#\77933\EM>th\1091019\20366%R\1102581D\RS{_m\ACKb8y-\139242Y'o\DC1l\1017027\1095971J\RSW/\SO#\30130\&5\ACKV\986388\1076689c\EM")), _iconUpdate = Nothing, _iconKeyUpdate = Just (unsafeRange ("a\1057128\31840\137020?\159020\&4\77997"))}))))
testObject_Event_team_11 :: Event
testObject_Event_team_11 = (newEvent (ConvDelete) ((Id (fromJust (UUID.fromString "00006350-0000-28bc-0000-6d4200007fba")))) (read ("1864-05-11 13:27:39.775711227674 UTC")) & eventData .~ (Just (EdConvDelete (Id (fromJust (UUID.fromString "0000498f-0000-5b35-0000-6a6d00007df6"))))))
testObject_Event_team_12 :: Event
testObject_Event_team_12 = (newEvent (TeamDelete) ((Id (fromJust (UUID.fromString "00006071-0000-2d16-0000-3ec500006710")))) (read ("1864-05-23 10:59:17.901050485335 UTC")) & eventData .~ (Nothing))
testObject_Event_team_13 :: Event
testObject_Event_team_13 = (newEvent (TeamCreate) ((Id (fromJust (UUID.fromString "00004ce0-0000-7c7e-0000-320100000670")))) (read ("1864-05-23 11:07:47.567764632505 UTC")) & eventData .~ (Just (EdTeamCreate (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000300000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000400000004")))) ("X") ("") (Binding) & teamIconKey .~ (Just "\ACKI\1004010@'")))))
testObject_Event_team_14 :: Event
testObject_Event_team_14 = (newEvent (ConvCreate) ((Id (fromJust (UUID.fromString "00001ba3-0000-4b66-0000-46aa000057ee")))) (read ("1864-04-23 05:49:20.560949996801 UTC")) & eventData .~ (Just (EdConvCreate (Id (fromJust (UUID.fromString "000073d0-0000-0088-0000-215500000b10"))))))
testObject_Event_team_15 :: Event
testObject_Event_team_15 = (newEvent (TeamDelete) ((Id (fromJust (UUID.fromString "000078d7-0000-6fea-0000-6bb4000067bc")))) (read ("1864-06-06 19:29:56.075884458899 UTC")) & eventData .~ (Nothing))
testObject_Event_team_16 :: Event
testObject_Event_team_16 = (newEvent (TeamUpdate) ((Id (fromJust (UUID.fromString "000055a6-0000-5c36-0000-635800002aac")))) (read ("1864-05-22 21:26:32.044655591875 UTC")) & eventData .~ (Just (EdTeamUpdate (TeamUpdateData {_nameUpdate = Just (unsafeRange ("\1040893}\175937\\{P\987971\\k\995770q\DLE\1092341\GSB\FS1\113700M,PT?5+{hX\RS\1111152\146217Ur\183961P\SYN\1030119=CjS\998546;U[\GShQ'\RSw\DLE[\6544M\ACK`\1602q\ESC\CANPVK\n\177216OU\54981z\1104480>5\SOH\37342D\141302?\1015066\&3\170220I\USD\163388\STX")), _iconUpdate = Just (unsafeRange ("HW\132538\&0\1096002d5\1096608|aNCl\ETX\153121z\SUB\191153%\42380]\95614^>2ENq\NUL\1100712\&4}\127282\1056520xY4\21845t\SI/\1207\NAK`,E}\15415\1037505\1032428u\1112516\vU\59841x}\ESC\DLEKs\188619r>\t:\t\1065602KY\21471\&7\1065439\1056586}RF\ACKE\DELXM%Q\DEL\127489\&3\1018128|\SOH\1013980\142199sQ\44265oY\SUB]+\20193\r\1034499?\1011624|!,\54246i<i\a")), _iconKeyUpdate = Just (unsafeRange (")\992984\9223\&2\DC1\58777y\vN$3e\SOH&6D\EM\2846R]\7338]>\FSSz%\141297\f\EM&1q\n\1053846\DC4\67810h\1053604\151142m\DC2\1048542aUF]J\150791CL\SUBW\"@zVT\71104T*!\1013921$\ACK\18823fU{=(\SOId\94559\22275\RSk\SUB\136655\RS#%}4\a\t\f2\14829\42308\1029475.%\\\USAZ/\1094422'f\NAKlk\1018772V6+kn!`rR\EOTCr\US>\1096906\1108203\1010352\n,M/9\1002479w$\989843\996237\DC3/\133389i\SO\v^+c\rb::\US~\SYN1\DC3M9g\bx@}&M\NAK\34298\41915GR\1009824*\n\ESCX\ESC\125135\NAK/\v\1098615ru<t\998470\144263T\1024725\1006707+p[M(R+\SOH\167102\83442\1054899\1037590\4219F\STX\1047938\155167\128856x\USJ\SOZ\99596^)*a/>r>|&\rBFxc\10647!M5\DC1]f\1042852Q[Vm|go\a\1083460l\n9HDo\SYN&>\1036775]\136393\1059743/"))}))))
testObject_Event_team_17 :: Event
testObject_Event_team_17 = (newEvent (TeamUpdate) ((Id (fromJust (UUID.fromString "00001fd8-0000-4c0a-0000-27e0000060b5")))) (read ("1864-05-11 03:11:27.930572326331 UTC")) & eventData .~ (Just (EdTeamUpdate (TeamUpdateData {_nameUpdate = Just (unsafeRange ("pfM\r\1023408\ACK-:q\NAK\ENQ\1071350+\65155P\1001568\ETX\SYNy\RSim\EMPH\ETB\SOH\DC4\46458{\1036359\138943 \FS@ehk+6\r\141631>o\SYNZf\EOT\1022840,ben\1055448\US!\GS\47028cR\111004\DLE\1093642\48661\EOTAiy\1055689z^\1113740\&3\1022670Z>\SI\"[F\FSjNn\1095690\986292>\ETB\SUB\160589_O~Z@\SYNCB\ETB\ETBxL\991076`Zdo'KYiN-%\1015713\1094373\&5+\44304\GS\DEL#$\US\1039684G\v_Tly\DEL;\f\62775{\987921~\167888\&8\4975=%\1044921\171275Q`\991835\10141g\FSm\153190c\NUL\\#\180061\SYN3tI\134419U8\ACK\SI\29955/\989193N\b\1064754c\158165<\172024\SOl,\1039236^&?l\NUL\136678Q\SUB\"\154373O18s(^YXa\133412\999185")), _iconUpdate = Just (unsafeRange ("\1049859n\159642Pc!L\53937\59814\NULm\DC3r\100990&\SO\173160V\82978\t6\FSh\ACKl6K(\1041041\ETX,ws0\131584L\CAN\159957H{_tP3<\NAK\1086812u\161885\147042\&5\1018951$L&S%\DC3nY\46825\SUBP39\NUL\f]\986987`[N\NUL\178590\DC1\157848\1111163d\DLE*b?UDnq\1067509\&4\\\996634P?\1097810\1063103\NUL\1057612/M\136641\66187Y*\147692\143267#U\142140\NUL\157623\37730q\173637@\SO\92567?fG\1095176|\GStl")), _iconKeyUpdate = Just (unsafeRange ("?/\EM\194700\1006607I-\tJ\n\1099683$\1089894=;\1088773\ENQ\ETX9\1076133+?6F\133166\99584kCt\EOT/\t\986840h\DC12::E]7l2\SOq+\169679X\989231\v\1113788RN\148290\&4\DLE\CAN89K5#\71891i\v\1072691\SUBg\1102387\18265f\12306\&0e+\1000481M\b)\1096905up|u<\1053960l-\DC3\1060747\RS\ETXi\\Hpu\v\1011540N\t\1085807k\174338\DC4J,\ESC7,X\SYN"))}))))
testObject_Event_team_18 :: Event
testObject_Event_team_18 = (newEvent (ConvCreate) ((Id (fromJust (UUID.fromString "00002da6-0000-252f-0000-785b00003152")))) (read ("1864-06-03 01:30:25.412028683799 UTC")) & eventData .~ (Just (EdConvCreate (Id (fromJust (UUID.fromString "000079e1-0000-0bb1-0000-06b9000023b8"))))))
testObject_Event_team_19 :: Event
testObject_Event_team_19 = (newEvent (ConvDelete) ((Id (fromJust (UUID.fromString "00003c02-0000-77e1-0000-5f2d00004b35")))) (read ("1864-05-07 12:11:52.544535809 UTC")) & eventData .~ (Just (EdConvDelete (Id (fromJust (UUID.fromString "00006653-0000-0f4b-0000-4f4b00004634"))))))
testObject_Event_team_20 :: Event
testObject_Event_team_20 = (newEvent (MemberUpdate) ((Id (fromJust (UUID.fromString "00005683-0000-57ef-0000-3cb4000031bf")))) (read ("1864-06-04 03:17:04.611330164772 UTC")) & eventData .~ (Just (EdMemberUpdate (Id (fromJust (UUID.fromString "00006e81-0000-0940-0000-371800002743"))) (Just (Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,SetTeamData,GetMemberPermissions,SetMemberPermissions,GetTeamConversations], _copy = fromList []})))))

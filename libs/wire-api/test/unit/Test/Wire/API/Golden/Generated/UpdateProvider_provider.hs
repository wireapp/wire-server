{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UpdateProvider_provider where
import Data.Coerce ( coerce )
import Data.Misc ( HttpsUrl(HttpsUrl) )
import Imports ( Maybe(Just, Nothing) )
import URI.ByteString
    ( URIRef(URI, uriScheme, uriAuthority, uriPath, uriQuery,
             uriFragment),
      Authority(Authority, authorityUserInfo, authorityHost,
                authorityPort),
      Host(Host, hostBS),
      Query(Query, queryPairs),
      Scheme(Scheme, schemeBS) )
import Wire.API.Provider ( UpdateProvider(..) )
import Wire.API.User.Profile ( Name(Name, fromName) )

testObject_UpdateProvider_provider_1 :: UpdateProvider
testObject_UpdateProvider_provider_1 = UpdateProvider {updateProviderName = Just (Name {fromName = "'xc\153104-!SVn][\ETB\993028w;\SOH\"\177850\48404L(\ENQ\b\162274)~\1080316.Z6^@'\1065883\FSR\STX~Q&D\180156%mV_DXME4W'4[\991449\1100936SK8JO%\163621'(\SIc0F\1032427B\"\DC4\1036683\ACK\145727F`_\132957_Q|\1111342!\\\SI\DC2C|z!\US\1036345\\G?\168098\1085991\&7\DC3sG\1003814eDW\DLE\95620'\992273\&1\\=%\bF"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\ESC\vQp0\74002Y\999729\EOTE"}
testObject_UpdateProvider_provider_2 :: UpdateProvider
testObject_UpdateProvider_provider_2 = UpdateProvider {updateProviderName = Just (Name {fromName = "Ys$:(^\97509\1070542\RS\US\995276\ESCn&<w\16607\20551/A\1084151L\ETB[Eap\152345\NUL"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "Y"}
testObject_UpdateProvider_provider_3 :: UpdateProvider
testObject_UpdateProvider_provider_3 = UpdateProvider {updateProviderName = Just (Name {fromName = "\SOH\41588\ETX\57541\51486\\\t\bD-ZX\nS\b\ENQH\DC4Y1\173911)\FS\\\159297?d\DC2p\159351fJ\22606h\EM\151851\170306i\RSI\EMt)\1046982wq!:C\CAN1oG\DLEN\15932U_]\1097551u\136228d+J/\1099616"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\1020484\1065127\SUBw\ENQ\ACK\989884"}
testObject_UpdateProvider_provider_4 :: UpdateProvider
testObject_UpdateProvider_provider_4 = UpdateProvider {updateProviderName = Nothing, updateProviderUrl = Nothing, updateProviderDescr = Just "<\18139"}
testObject_UpdateProvider_provider_5 :: UpdateProvider
testObject_UpdateProvider_provider_5 = UpdateProvider {updateProviderName = Just (Name {fromName = "{9u_\175080?U\1046712HVi\tXb\RS<o_\ESCm'Q\30048\142321\FS<O?qCe[\1082613\fw\DC1O\CAN\998014N\CANd\1108432I\1103720\r7\1062816>\GSl\1110889a*"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Nothing}
testObject_UpdateProvider_provider_6 :: UpdateProvider
testObject_UpdateProvider_provider_6 = UpdateProvider {updateProviderName = Just (Name {fromName = "+\SUB\166991\&8K\998554A\1096914\43714^ \1089050\1103098f\a\SUB\USg\132655\984139\&7U\996578\135626\6182PF>r\39744FU\ESC`)\a\v\21578iK\37980\1060957#L{\DC4|\SUB\1066658\1027999e)TL~V\GS;8\22111!x\DC1\ETB/L@9"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\988779\189290a1h@\STX"}
testObject_UpdateProvider_provider_7 :: UpdateProvider
testObject_UpdateProvider_provider_7 = UpdateProvider {updateProviderName = Just (Name {fromName = "\fZ\159626\ENQ\100168\1078796\1044086\&1H\1054549\SI\78202~\1021764\DC1q\DC1N\SOH\NUL\ENQ\DC2\ETX\1049364/^\1105410P\"\189225-o\1070778IW\64849\&0p\\)TE\DELX*\EMX7\1085819\12878\\|\997308\&9\1034656\1044022\1020096\&3Kb/\\VSf&\r#\SOH\STX\139585\&9^Gd!(Ug^x-6v3+x9\1101707\NUL"}), updateProviderUrl = Nothing, updateProviderDescr = Just "\14270\SYN6\ETX{k\DC2\CAN"}
testObject_UpdateProvider_provider_8 :: UpdateProvider
testObject_UpdateProvider_provider_8 = UpdateProvider {updateProviderName = Just (Name {fromName = "s(\FS>U\155817WE0^\1062629\34216\&9\EMt\DC1K\b\51170\f\ESCF\149237\ESC\STXvv\DC3\EMN\"\1010965\NUL\f$\FS1\1104550\1041096\SYN%Ut\n\4598\ETB]\1043164=\ESC@"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just ""}
testObject_UpdateProvider_provider_9 :: UpdateProvider
testObject_UpdateProvider_provider_9 = UpdateProvider {updateProviderName = Just (Name {fromName = "\137714\1087855&SF/m*\95261\ACK0\163162\ETBF;eO`escm\FS`\1037166\1068369-f\145461J\SO\1021299k\SI\SUB\SOH\1074169\185316\DC3>x\SOH/\1056296\994587\"\DC4\NULGc\DC2\ESC\1027243K5\29804@a\"Ed\51634s\DC4H\61912\DEL\1030731O\48030/\GS\SYN\68226;\1018111\1043232\&9D;e\DC2\1041566<|\1088746\DC205\986104\&4\GS|\n\SUB\1048402\DEL>\1077472\DC4SEDW\DEL4\FSOE\7439\65479\DLEmW\1032402\\\\t\SYN\96353Z\28964\162660\61012"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\DC4\FSw\1037118\rU0\1097317\27076%"}
testObject_UpdateProvider_provider_10 :: UpdateProvider
testObject_UpdateProvider_provider_10 = UpdateProvider {updateProviderName = Just (Name {fromName = "}wO\1017822sW\SO\1001356\NAK\"\t'!Z2y\58846\68021\&5\DC1\f \SUBLs/d^C\1058321\1034276L\ESC#JV\987477\1041032;C\CAN[i#?\1005813\1019256X;TF\1074471\&6\r\DC1"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just ""}
testObject_UpdateProvider_provider_11 :: UpdateProvider
testObject_UpdateProvider_provider_11 = UpdateProvider {updateProviderName = Just (Name {fromName = "0\1082046\CANVE);\ESC\1064330F\aI\41836\167147\"\t\32247\SI"}), updateProviderUrl = Nothing, updateProviderDescr = Nothing}
testObject_UpdateProvider_provider_12 :: UpdateProvider
testObject_UpdateProvider_provider_12 = UpdateProvider {updateProviderName = Nothing, updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Nothing}
testObject_UpdateProvider_provider_13 :: UpdateProvider
testObject_UpdateProvider_provider_13 = UpdateProvider {updateProviderName = Nothing, updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\GS@\1069767"}
testObject_UpdateProvider_provider_14 :: UpdateProvider
testObject_UpdateProvider_provider_14 = UpdateProvider {updateProviderName = Nothing, updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\1055702\66803\1096417"}
testObject_UpdateProvider_provider_15 :: UpdateProvider
testObject_UpdateProvider_provider_15 = UpdateProvider {updateProviderName = Just (Name {fromName = "ptZCDS?t\1041804L\FSN75\25613n\1050235~(\SOFIg\17289\1052540\127504y'*\SOH&?Z"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\540\RSt3\1045676"}
testObject_UpdateProvider_provider_16 :: UpdateProvider
testObject_UpdateProvider_provider_16 = UpdateProvider {updateProviderName = Just (Name {fromName = "U\1109329G4q\32069q\FSG6}\EOTr)\f^f\1051044n';e-h\SYN0\165274)6oyy~2\3977\28693p.g\174371&\143159Ko\1016567W\n\NAK\178126\SO02\ESC\17462\as\160172\128663\1095298\1004247\183785\SI!\40567\SYN\28707\1100917^Y"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just ""}
testObject_UpdateProvider_provider_17 :: UpdateProvider
testObject_UpdateProvider_provider_17 = UpdateProvider {updateProviderName = Just (Name {fromName = " \DC1RM\ESC\59008_FX|\1011433\SOj\1081758_xq\EM5;8\1042575\ETX\144254\ACKXh{%\166769\133216\DLE}\60435\FS)\NAK\US\n\FS$G$\v\ENQ\18140\FS\SIp\1109318\FS\ENQ\ACK4\7407a~'\t1:C7W\50771c2"}), updateProviderUrl = Nothing, updateProviderDescr = Just "\NULx\71174M"}
testObject_UpdateProvider_provider_18 :: UpdateProvider
testObject_UpdateProvider_provider_18 = UpdateProvider {updateProviderName = Nothing, updateProviderUrl = Nothing, updateProviderDescr = Just ""}
testObject_UpdateProvider_provider_19 :: UpdateProvider
testObject_UpdateProvider_provider_19 = UpdateProvider {updateProviderName = Just (Name {fromName = "(\1032589~\164312\DC1\37062\US\1095748\EMRYC\99934{g\ETB\1039139m]Y0\138250=\GS\1080631\1097429`o\3363*{\ESC8x{~\DC4f-\175092"}), updateProviderUrl = Nothing, updateProviderDescr = Nothing}
testObject_UpdateProvider_provider_20 :: UpdateProvider
testObject_UpdateProvider_provider_20 = UpdateProvider {updateProviderName = Just (Name {fromName = "\37483\9488Yp \ESC\SO?\b\1077335\1084450q\53156\&6?\83108\96840\ENQZ\168498\DEL(\986745"}), updateProviderUrl = Nothing, updateProviderDescr = Just "_%\1054856@6 ]"}

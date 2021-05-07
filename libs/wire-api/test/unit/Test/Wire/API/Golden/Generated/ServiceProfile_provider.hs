{-# LANGUAGE OverloadedLists #-}

module Test.Wire.API.Golden.Generated.ServiceProfile_provider where

import Data.Id (Id (Id))
import qualified Data.UUID as UUID (fromString)
import GHC.Exts (IsList (fromList))
import Imports
  ( Bool (False, True),
    Maybe (Just, Nothing),
    fromJust,
  )
import Wire.API.Provider
  ( ServiceTag
      ( AudioTag,
        BooksTag,
        BusinessTag,
        EducationTag,
        EntertainmentTag,
        FinanceTag,
        FoodDrinkTag,
        GamesTag,
        MedicalTag,
        MusicTag,
        PollTag,
        ProductivityTag,
        RatingTag,
        TravelTag,
        TutorialTag,
        VideoTag
      ),
  )
import Wire.API.Provider.Service (ServiceProfile (..))
import Wire.API.User.Profile
  ( Asset (ImageAsset),
    AssetSize (AssetComplete, AssetPreview),
    Name (Name, fromName),
  )

testObject_ServiceProfile_provider_1 :: ServiceProfile
testObject_ServiceProfile_provider_1 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))), serviceProfileName = Name {fromName = "i`\"\147623Oep\78546_\1031126\7094\&7g\1092061(V\162415\RS/zT2\30367\1027942?\992952u7\SYNk\ETX\144968\1034561=)\36321\FSp{\153301w;Q!M|\1080545\EM<\28397\1061400$_.\189221\GS\63280&I\139792\1052400\1056777\NAKOA\40460\NUL\SOH\1077304[|\22459\4623c3^H\DC2\CAN~\DC4\1093450\37818Ed7\22651\1066772\fHk\vv\DC1)Mc:"}, serviceProfileSummary = "\1008770\60807", serviceProfileDescr = "/Q", serviceProfileAssets = [(ImageAsset "\ESC" (Nothing))], serviceProfileTags = fromList [], serviceProfileEnabled = True}

testObject_ServiceProfile_provider_2 :: ServiceProfile
testObject_ServiceProfile_provider_2 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))), serviceProfileName = Name {fromName = "\CAN&8\ENQT\1086712A\29359KU%BH4,\10095s/\1070196 C4\SYNPS~\f\990737\17575\1070774`\66459\DC2EI$%K0\29393]w\13586\&3X\NUL\1038796x\RS0h\189771\SILk#F2YXw\1113736\1006551\&8\155429\46267!\1109155\1095499\10284t\SUB{,"}, serviceProfileSummary = ")/", serviceProfileDescr = "", serviceProfileAssets = [(ImageAsset "" (Just AssetComplete)), (ImageAsset "" (Just AssetComplete)), (ImageAsset "" (Just AssetComplete)), (ImageAsset "" (Just AssetComplete))], serviceProfileTags = fromList [FoodDrinkTag, TravelTag], serviceProfileEnabled = True}

testObject_ServiceProfile_provider_3 :: ServiceProfile
testObject_ServiceProfile_provider_3 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000002"))), serviceProfileName = Name {fromName = "\1109783\1019914\EOT|6U\158436\1083299\149833\&0&+\DC4\96215\DC1p\177107v\74974\GS\fn\EOTf \r\1040257+2O"}, serviceProfileSummary = "\ETX* ", serviceProfileDescr = "\136788It", serviceProfileAssets = [(ImageAsset "" (Nothing)), (ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [], serviceProfileEnabled = True}

testObject_ServiceProfile_provider_4 :: ServiceProfile
testObject_ServiceProfile_provider_4 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), serviceProfileName = Name {fromName = "\187532A\a\983547\1063277R\139884O\ETB+\SOH[L#\ACK\983894\92471\NAK^\1089558q@\SUB!';#\173830?z\1067443`\DLE#\EM:\ACK\DC4nH)A\1106685\1059913ev\ETBy\DC3\1049611@\GSte\1032190"}, serviceProfileSummary = "4E", serviceProfileDescr = "(", serviceProfileAssets = [(ImageAsset "1" (Just AssetComplete))], serviceProfileTags = fromList [AudioTag, RatingTag], serviceProfileEnabled = True}

testObject_ServiceProfile_provider_5 :: ServiceProfile
testObject_ServiceProfile_provider_5 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))), serviceProfileName = Name {fromName = "Y\DC4~-%5>p9\97813\11698\1024016\29511Ne~"}, serviceProfileSummary = "\DC3", serviceProfileDescr = "\1017669Y", serviceProfileAssets = [], serviceProfileTags = fromList [], serviceProfileEnabled = True}

testObject_ServiceProfile_provider_6 :: ServiceProfile
testObject_ServiceProfile_provider_6 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), serviceProfileName = Name {fromName = "QN\STX\"[\1071371_\SO\21981:#\171302c\f@wqW\rLV\1066410~_D:u\1015519\&3'I\SOH\r`9\142860\1110900\1089091c77~P\"\SO\DC3*9\FS\b\138313[\6076s\46767\\4\1072814(\FS-E:2*I'>{axLT/r}9\45356\128493RC\1058631\1009452!\136451v>`\1006672o\DELG\51720SJ\SYNo\1028308\181942\74100\151888"}, serviceProfileSummary = "4>#", serviceProfileDescr = "D\DEL", serviceProfileAssets = [(ImageAsset "" (Nothing)), (ImageAsset "" (Just AssetComplete)), (ImageAsset "" (Just AssetComplete)), (ImageAsset "" (Nothing)), (ImageAsset "" (Just AssetComplete))], serviceProfileTags = fromList [], serviceProfileEnabled = False}

testObject_ServiceProfile_provider_7 :: ServiceProfile
testObject_ServiceProfile_provider_7 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000"))), serviceProfileName = Name {fromName = "Yi\995451f\128150\&9\1053915~\NUL\ETB\SO\179920j\1007299vyQq#^(a\RS\1096920)\1040685\&2Tu$:\SI\40085`&ik\57473\1015812\120065b\RSB\1034073g5\DC3D;\ESC\US\43434\"\53134\EM#i\1015045iP\r\1009897\134223\DC1I\157067\b\ETB\"\166140\SO>A\31390"}, serviceProfileSummary = "0\992827", serviceProfileDescr = "11*", serviceProfileAssets = [], serviceProfileTags = fromList [AudioTag, TutorialTag], serviceProfileEnabled = False}

testObject_ServiceProfile_provider_8 :: ServiceProfile
testObject_ServiceProfile_provider_8 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), serviceProfileName = Name {fromName = "X,\997714\ESC\154425\DEL\1063625\139060:t\39786f\ESCj\1081642Is4\171580A*\1098132\ETBu\t_Xw\SOH"}, serviceProfileSummary = "", serviceProfileDescr = "\ACK", serviceProfileAssets = [], serviceProfileTags = fromList [BooksTag, BusinessTag, GamesTag], serviceProfileEnabled = True}

testObject_ServiceProfile_provider_9 :: ServiceProfile
testObject_ServiceProfile_provider_9 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), serviceProfileName = Name {fromName = "\EM\73877+\DC2\NUL!\USV\f\1025396\1106635_\1106841H#4\STX\1104704\DEL"}, serviceProfileSummary = "a\1088958", serviceProfileDescr = "AU", serviceProfileAssets = [(ImageAsset "\DC1" (Nothing))], serviceProfileTags = fromList [BusinessTag, FinanceTag, PollTag], serviceProfileEnabled = False}

testObject_ServiceProfile_provider_10 :: ServiceProfile
testObject_ServiceProfile_provider_10 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), serviceProfileName = Name {fromName = ":h[\1059282\1033090\913Y$\ENQ\NAKE\1086801\186280\STX\US\28752"}, serviceProfileSummary = ",AD", serviceProfileDescr = "s&\118974", serviceProfileAssets = [(ImageAsset "" (Just AssetPreview)), (ImageAsset "" (Nothing))], serviceProfileTags = fromList [], serviceProfileEnabled = False}

testObject_ServiceProfile_provider_11 :: ServiceProfile
testObject_ServiceProfile_provider_11 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), serviceProfileName = Name {fromName = "$c\1046843\SYNVQ\23425\184122\&3\NAK@\"}e|bBr\1005857\DEL\1025435\1073603\1065252\n\RSuCS\128308\&0t\1079277\&9\136640\"\DC4dO+^t\SYN\SUB\SUB\DLExT\126465`4V\GSDf\v\STX\"\\\ACKT`9+\DLE \997402\66795\29575c%\10908fv\165096d{z\ETB\1045334\183275s\ENQ\18690P"}, serviceProfileSummary = "yF", serviceProfileDescr = "", serviceProfileAssets = [], serviceProfileTags = fromList [MusicTag, RatingTag, TutorialTag], serviceProfileEnabled = False}

testObject_ServiceProfile_provider_12 :: ServiceProfile
testObject_ServiceProfile_provider_12 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), serviceProfileName = Name {fromName = "H{eal2\SOq!Eb)\1095225X,\b<\138899%\50335\67981\FS\34375\NAK\GS6\120296\1001806`JE@\118993D\51314%{\ACK.\v\184656\50561\aAv\1095544\16863Tk9\a\97118\&3#*_+\171101o"}, serviceProfileSummary = "\SOv", serviceProfileDescr = "WR\1112551", serviceProfileAssets = [], serviceProfileTags = fromList [EducationTag, MedicalTag, ProductivityTag], serviceProfileEnabled = False}

testObject_ServiceProfile_provider_13 :: ServiceProfile
testObject_ServiceProfile_provider_13 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002"))), serviceProfileName = Name {fromName = ":[\".\152322\USvU\1055877"}, serviceProfileSummary = "", serviceProfileDescr = "A", serviceProfileAssets = [(ImageAsset "B" (Nothing))], serviceProfileTags = fromList [ProductivityTag], serviceProfileEnabled = False}

testObject_ServiceProfile_provider_14 :: ServiceProfile
testObject_ServiceProfile_provider_14 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))), serviceProfileName = Name {fromName = "8Y#1L\97071\&2\168676Si\159235\1073647p"}, serviceProfileSummary = "", serviceProfileDescr = "\EM", serviceProfileAssets = [], serviceProfileTags = fromList [EntertainmentTag, ProductivityTag], serviceProfileEnabled = True}

testObject_ServiceProfile_provider_15 :: ServiceProfile
testObject_ServiceProfile_provider_15 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), serviceProfileName = Name {fromName = "=\152496\SYN9\1042551\1043075\RS\SYN\EOT\13551!\149191b\1050296NV\b(G\186581.AD\US!\63825\r79P6<\98907\99923\1067459y\1051025KVw!\53598-\37169\1067352\&7\r{/(6\1059173\137714\ETX\138977m\1076339B\US%}ag\SOH\1075928L\165604\21719\97717~`;4\1000027w\EM\\`%2u\99170P\1079881dm\41595f\a7)\DELfUWBYt\68317\SI\65517#u\b\EMP:J\23265^L\1111793"}, serviceProfileSummary = "*P`", serviceProfileDescr = "u`\ENQ", serviceProfileAssets = [(ImageAsset "*" (Nothing))], serviceProfileTags = fromList [MusicTag, RatingTag], serviceProfileEnabled = False}

testObject_ServiceProfile_provider_16 :: ServiceProfile
testObject_ServiceProfile_provider_16 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))), serviceProfileName = Name {fromName = "\96487\r7]\1085280\DC2m/\149566\1006818,VKS['\1076799\1108979s\1080417\SOH\1015395\f\178667aDP\EOT\ETX\997696\&0"}, serviceProfileSummary = "U,", serviceProfileDescr = "S\n", serviceProfileAssets = [(ImageAsset "" (Just AssetPreview)), (ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [], serviceProfileEnabled = False}

testObject_ServiceProfile_provider_17 :: ServiceProfile
testObject_ServiceProfile_provider_17 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))), serviceProfileName = Name {fromName = ":n\b\1014187D\r\tk\1032992Q\187486\1107848\SOH6\STX\188758t\SYN8hz,\1099172mk\nI\143211!\137935(1Y\50524\a,\172216\1018683\184032o\SYN\1030886\154423\993847y{k+\1092845-\SOHj\DC4J\DC1qC{P\152867w\SYN\v\ESC\120845`{B\ESC\SO^(N\194986\t\1029525\1050730\&6\1033609\DC2$\999592,\RS\f\31719Wh\150289@\1053386\ACKe\tb%\179300xQ{u\NULe\22791\&7D:\32561A\998216w#@xB\EOTfsb\1032099\41477,\46761\856x%P"}, serviceProfileSummary = "\SO4c", serviceProfileDescr = "\SI", serviceProfileAssets = [(ImageAsset "" (Just AssetComplete)), (ImageAsset "" (Just AssetComplete))], serviceProfileTags = fromList [], serviceProfileEnabled = False}

testObject_ServiceProfile_provider_18 :: ServiceProfile
testObject_ServiceProfile_provider_18 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000"))), serviceProfileName = Name {fromName = "3\54553\ENQ\142228Vj:\NAK\52768\r\DLEo\186106"}, serviceProfileSummary = "", serviceProfileDescr = "\20788", serviceProfileAssets = [], serviceProfileTags = fromList [RatingTag], serviceProfileEnabled = True}

testObject_ServiceProfile_provider_19 :: ServiceProfile
testObject_ServiceProfile_provider_19 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), serviceProfileName = Name {fromName = "\ACK\62873\1002290\SI\1099161\1083278\99151\DC4\SOH\DC4\1005694\1073449\SI\1101819\46257_D?\66010%"}, serviceProfileSummary = "\1042245\94011\4346", serviceProfileDescr = "\1033302", serviceProfileAssets = [], serviceProfileTags = fromList [FoodDrinkTag, MedicalTag, VideoTag], serviceProfileEnabled = False}

testObject_ServiceProfile_provider_20 :: ServiceProfile
testObject_ServiceProfile_provider_20 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), serviceProfileName = Name {fromName = "b:/-B\1055305L\EOTlQ\DC3\48756\NAK\187607\177558\68314P\SYN}F\991538(\SI\FS\1097983\&2,]o\187565"}, serviceProfileSummary = "\13832", serviceProfileDescr = "6\185131?", serviceProfileAssets = [], serviceProfileTags = fromList [GamesTag], serviceProfileEnabled = False}

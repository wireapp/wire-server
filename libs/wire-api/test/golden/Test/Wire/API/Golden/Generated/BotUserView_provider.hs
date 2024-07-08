-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.Wire.API.Golden.Generated.BotUserView_provider where

import Data.Handle (parseHandle)
import Data.Id (Id (Id))
import Data.UUID qualified as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust)
import Wire.API.Provider.Bot (BotUserView (..))
import Wire.API.User.Profile (ColourId (ColourId, fromColourId), Name (Name, fromName))

testObject_BotUserView_provider_1 :: BotUserView
testObject_BotUserView_provider_1 =
  BotUserView
    { botUserViewId = Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002")),
      botUserViewName =
        Name
          { fromName =
              "\DC1\26122U5z$\CAN\GS t1\RS\\\STX\163323_4K\1108113\1030339\78439)\DC3\171456\FS\1039863\1089420n\7092\1008914\\4Nn;\171427)\182846y\SO\n|\DEL1#pK\51301b\t<v\US/>\132598+\SOH\5517\DELjJ\179985\191367Z  `$"
          },
      botUserViewColour = ColourId {fromColourId = -8},
      botUserViewHandle = Just (fromJust (parseHandle "fpa2vx")),
      botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002")))
    }

testObject_BotUserView_provider_2 :: BotUserView
testObject_BotUserView_provider_2 =
  BotUserView
    { botUserViewId = Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000500000001")),
      botUserViewName =
        Name
          { fromName =
              "v\1099438\1020222\SOM\989617\t\ETB\\\1068888\187702nE7?\SOH:\r\1050763m \1065605}Y\989133b_\DLEDVa\1054567uJJ|\1086658\US)\DC3C"
          },
      botUserViewColour = ColourId {fromColourId = -5},
      botUserViewHandle = Just (fromJust (parseHandle "mz")),
      botUserViewTeam = Nothing
    }

testObject_BotUserView_provider_3 :: BotUserView
testObject_BotUserView_provider_3 =
  BotUserView
    { botUserViewId = Id (fromJust (UUID.fromString "00000004-0000-0006-0000-000100000002")),
      botUserViewName = Name {fromName = "\EOT\a.\ACK\1104026\ETB"},
      botUserViewColour = ColourId {fromColourId = 7},
      botUserViewHandle = Nothing,
      botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000500000003")))
    }

testObject_BotUserView_provider_4 :: BotUserView
testObject_BotUserView_provider_4 =
  BotUserView
    { botUserViewId = Id (fromJust (UUID.fromString "00000008-0000-0004-0000-000300000007")),
      botUserViewName = Name {fromName = "\SUB\STX)gKj\FS\1076685\v6cg\f]N!t\\\1017810\&8\70320\&7I\ETXCS\DC4e\FS\FS"},
      botUserViewColour = ColourId {fromColourId = -2},
      botUserViewHandle = Just (fromJust (parseHandle "7.w")),
      botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000200000000")))
    }

testObject_BotUserView_provider_5 :: BotUserView
testObject_BotUserView_provider_5 =
  BotUserView
    { botUserViewId = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000500000008")),
      botUserViewName = Name {fromName = "w"},
      botUserViewColour = ColourId {fromColourId = -1},
      botUserViewHandle = Just (fromJust (parseHandle "tidlyhr")),
      botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000800000005")))
    }

testObject_BotUserView_provider_6 :: BotUserView
testObject_BotUserView_provider_6 =
  BotUserView
    { botUserViewId = Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000000000002")),
      botUserViewName =
        Name
          { fromName =
              "\STX\bEd\52196:\78192\994324]\121075\vG\CAN\DC3\ETB\1035344R\1089997Ky6e\21650QT\1061894\CANEE4W\984387^\170551s\t\145385iP\RS.XV(kK\CAN7b\DLE\1091894f\1045011s\1110268EAm^\1008493\ETB\163757\&2\SOH7\NULQjmaTO \990236W\USb\1001434\STXK\ACK\v\20946Z\1080220fwFip\SOv#\11071\1003061/\r\99414\36598,V\EM\64664Uy\t\141047XJ\488\1025642^\166643\135893E\1103431?G7\ETX\1031802Y\63065%)\52177."
          },
      botUserViewColour = ColourId {fromColourId = -5},
      botUserViewHandle =
        Just (fromJust (parseHandle "uz3cgdxtkev-40624m0eh_y06g-c9isv-ob.r84rneq2vm.440nxc_n44_3d0-6u9l7")),
      botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000300000001")))
    }

testObject_BotUserView_provider_7 :: BotUserView
testObject_BotUserView_provider_7 =
  BotUserView
    { botUserViewId = Id (fromJust (UUID.fromString "00000006-0000-0003-0000-000800000002")),
      botUserViewName =
        Name
          { fromName =
              "}$d}\RSY\1064459\1052613\96622np\1076823_\150435\1064267\&4<Ob\DC2\DLE\1086550\135013$Ph/`Q;7\1070615Z\985112\"`\SI\169008D#%"
          },
      botUserViewColour = ColourId {fromColourId = 3},
      botUserViewHandle = Just (fromJust (parseHandle "-c7916..")),
      botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0005-0000-000200000001")))
    }

testObject_BotUserView_provider_8 :: BotUserView
testObject_BotUserView_provider_8 =
  BotUserView
    { botUserViewId = Id (fromJust (UUID.fromString "00000008-0000-0008-0000-000500000003")),
      botUserViewName = Name {fromName = "s\1060675\46766`4Ps\990821z\SOHef"},
      botUserViewColour = ColourId {fromColourId = -2},
      botUserViewHandle =
        Just (fromJust (parseHandle "p1brgaim3u1-2grwj1-ij_m1p0_t.vqk92o0nxg1t928_r-qj58xhcg-ai.zfj9q63h2nno5om5kt38oo58vx81b27f4f_kzra9r6.56-gfb2ymhl3c2na7_pmsio59vezrgq-09g8yd_4hv2pdj.5rm62cp39qb_v7-x3q2a2yom-so1t-cpi2x5x6o1hwabe3ruci82gfar.czdlwukqi8dhvdse3b5o4_diowoozf1p5mcget81fa_zv8eff")),
      botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000600000007")))
    }

testObject_BotUserView_provider_9 :: BotUserView
testObject_BotUserView_provider_9 =
  BotUserView
    { botUserViewId = Id (fromJust (UUID.fromString "00000007-0000-0005-0000-000100000001")),
      botUserViewName =
        Name
          { fromName =
              "Vp9uC\ESC\f}g\DC3\149050D\CANrDl)\1046730G\1113624\a\ETB\1056868DQM\179156\SUBF\1045143;\1019794x\RSB\1093343\EM\STXFN\EM18W\t>\f\vu"
          },
      botUserViewColour = ColourId {fromColourId = -7},
      botUserViewHandle =
        Just (fromJust (parseHandle "qsbule-m4sswyr84uqh0.nzlyuher98kp-fkxn7xydbazbdeg-j-8sco56tr4i928muztpmzc-6nlzi4ob.-4lyhr6ra.wsc7jn6wv.l1mno080ax890jgd7.u_-2.-4v3n8")),
      botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000700000002")))
    }

testObject_BotUserView_provider_10 :: BotUserView
testObject_BotUserView_provider_10 =
  BotUserView
    { botUserViewId = Id (fromJust (UUID.fromString "00000008-0000-0005-0000-000200000007")),
      botUserViewName =
        Name {fromName = "]\1060113<]\47337\1007468\152444\10283r\128813XH\57454\&8icz\164234|\b#\ETBW@T"},
      botUserViewColour = ColourId {fromColourId = -6},
      botUserViewHandle = Just (fromJust (parseHandle "oihf1d")),
      botUserViewTeam = Nothing
    }

testObject_BotUserView_provider_11 :: BotUserView
testObject_BotUserView_provider_11 =
  BotUserView
    { botUserViewId = Id (fromJust (UUID.fromString "00000001-0000-0006-0000-000400000008")),
      botUserViewName =
        Name
          { fromName =
              "VwU\1026161\1038025*n\1095455lxS\SYNj\1091607y\1027202\NAK_',[>\SO<\n\1072483\ETX\142043$\986707qq{\1017506\DLE\1028303j\28095\&2\RS\ETX\r\162025m\159299\r\1060110i\SYN\DLEB\30844A)}T\1057676Li:0\1091417\t{\US\1058882h#\"\SYNX\61987\50175\&2\DC3*-b\1065285\NAK\39549T\6155 \RS\173727w\178047.\n\149471X`\20164a"
          },
      botUserViewColour = ColourId {fromColourId = -8},
      botUserViewHandle = Just (fromJust (parseHandle "pe1a")),
      botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000000000001")))
    }

testObject_BotUserView_provider_12 :: BotUserView
testObject_BotUserView_provider_12 =
  BotUserView
    { botUserViewId = Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000600000008")),
      botUserViewName =
        Name
          { fromName =
              "\NAK3|\178314{P\1040385Y[\SO\1071159\1040158\1010776\DELN\"\GSs=8d4:'\1030848o&~*\"\CANO[T9yt,9\STX7\nx\r"
          },
      botUserViewColour = ColourId {fromColourId = 8},
      botUserViewHandle = Just (fromJust (parseHandle "9pbwmf1pu")),
      botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000300000004")))
    }

testObject_BotUserView_provider_13 :: BotUserView
testObject_BotUserView_provider_13 =
  BotUserView
    { botUserViewId = Id (fromJust (UUID.fromString "00000000-0000-0008-0000-000300000005")),
      botUserViewName = Name {fromName = "q"},
      botUserViewColour = ColourId {fromColourId = -2},
      botUserViewHandle = Just (fromJust (parseHandle "3b")),
      botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000008-0000-0002-0000-000400000004")))
    }

testObject_BotUserView_provider_14 :: BotUserView
testObject_BotUserView_provider_14 =
  BotUserView
    { botUserViewId = Id (fromJust (UUID.fromString "00000008-0000-0002-0000-000500000000")),
      botUserViewName =
        Name
          { fromName =
              "a\DLE\EOT#\SOH\ETX|pg~w\GS\1079903*y\49789Mn+PUm\996301-\EM{\985036( q-t\SUB7s\1014955Xy\DC2\1100718\";1\145181G\DEL\DC4is^\STXNN0sc\t\43532\1070307\1086813dzcm\v\v*Ph\985566&L\1091319S\1032296\SYNw\1065810me\155218"
          },
      botUserViewColour = ColourId {fromColourId = 3},
      botUserViewHandle =
        Just (fromJust (parseHandle "c96fkuf-n70btjq075dki054avcu250987uq348n5bali201abmhiil8n9aiocxhuzuaf1gd733hi-2f7.wtlbb8d30cxymsvmb5ehb_9cu1din7j-996_h1tsvigyg")),
      botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000300000000")))
    }

testObject_BotUserView_provider_15 :: BotUserView
testObject_BotUserView_provider_15 =
  BotUserView
    { botUserViewId = Id (fromJust (UUID.fromString "00000002-0000-0008-0000-000100000007")),
      botUserViewName =
        Name
          { fromName =
              "~;Q\NULY<\152250H\46721\&7\EMQ\SYNz\"\NAK7\n\26201\15743\\a\153004\DC4Ip4\151032N\US1X6\DLERM\SUBC\1068323d\DC38Cs\NULO\1112182dF\\g~vj!J\DC1\a\1078997\&3\RSHf\STX\984173c\20151;z\33583\EM\SOH\DLE\EOT\1103829\153925_\1046171-\EM\1031631+\US/&>rNy,U\1047882\&7\1005658\NAK2"
          },
      botUserViewColour = ColourId {fromColourId = -1},
      botUserViewHandle = Just (fromJust (parseHandle "j4z9ty7y-wt_ldl_tddmmrhdfp4myz9fjrqdg2dkh5r9vxcs5z")),
      botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0004-0000-000300000004")))
    }

testObject_BotUserView_provider_16 :: BotUserView
testObject_BotUserView_provider_16 =
  BotUserView
    { botUserViewId = Id (fromJust (UUID.fromString "00000006-0000-0007-0000-000800000005")),
      botUserViewName =
        Name
          { fromName =
              "W0\DC23\179352_\150603\&9\1081508\41244!\USNh\1010987\48629\1008710+\30291\147681S\23109\94906H[sp^\EOT(\r\184575\v>I{G\CAN\1090476\129048\FS\GS\181835K\1026670oOJ\USB]t\1042482L wY\1027509\11746\DC4l5Y\46221[,TcoF~_\ENQ\r\42008\136798\ETB"
          },
      botUserViewColour = ColourId {fromColourId = -4},
      botUserViewHandle = Just (fromJust (parseHandle "_mvtpq.f")),
      botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000005-0000-0001-0000-000700000005")))
    }

testObject_BotUserView_provider_17 :: BotUserView
testObject_BotUserView_provider_17 =
  BotUserView
    { botUserViewId = Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000400000000")),
      botUserViewName =
        Name
          { fromName =
              ")o}s)\181243+0\EOT\154402C\1048068\1060448\SID[c c\r\1108938$'f6\1002325 ~|,A\f\32588\FSJ\1011697?\166257MJp\1738\DEL\DC2:}B'\DLEQ\54387\136046\1057923\DC2A\DC4\140654\SOH\r\1012989\DC1\188221\1007075?"
          },
      botUserViewColour = ColourId {fromColourId = 1},
      botUserViewHandle = Just (fromJust (parseHandle "f.xl")),
      botUserViewTeam = Nothing
    }

testObject_BotUserView_provider_18 :: BotUserView
testObject_BotUserView_provider_18 =
  BotUserView
    { botUserViewId = Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000700000000")),
      botUserViewName =
        Name
          { fromName =
              "\988095\134570T^ff6\SOH6@\DEL\1025500%\1044243\FSvM_s\176\ETB$K\1095116.\NAKm[\US\128932\EOT\SOH)\178049f\134315\1041068\&0kTn!9\SIL\1024745\n\a\1029970\\K(\146913\150726\SUB\NUL\1000860^W?\SOn|-\nR<\1099109\1046581\1036758\157276\GSQu\NAK\46380\FS\50047\1049174\183149\1111902b4\USly\DEL`'X%$mW]k\1051138\98086"
          },
      botUserViewColour = ColourId {fromColourId = -7},
      botUserViewHandle = Just (fromJust (parseHandle "b-p")),
      botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000700000006")))
    }

testObject_BotUserView_provider_19 :: BotUserView
testObject_BotUserView_provider_19 =
  BotUserView
    { botUserViewId = Id (fromJust (UUID.fromString "00000005-0000-0007-0000-000200000005")),
      botUserViewName = Name {fromName = "\CAN0\STX\STX\SOH='\b\ETX\119524Y8\1048503 \EMa\72317\134511,q\SOH'"},
      botUserViewColour = ColourId {fromColourId = -1},
      botUserViewHandle =
        Just (fromJust (parseHandle "g_ms.jaq23mkzzhouss60itfsrux5lapflg0xqotoz76f-ori4aglkqwj-raa_wr4ypirq9c9-w17nwre3414mvmm-vgetkk-07k1dgekjrzcvk-_w33giuc8wcak590c29h457nks5xzpn6tq0wtcorgq7210uaminql8ygrklj3vh11p.sg-nrbnmm2.dxmo0zzhr3xco")),
      botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0000-0000-000400000008")))
    }

testObject_BotUserView_provider_20 :: BotUserView
testObject_BotUserView_provider_20 =
  BotUserView
    { botUserViewId = Id (fromJust (UUID.fromString "00000000-0000-0005-0000-000500000003")),
      botUserViewName =
        Name
          { fromName =
              "QAM@y\SOe\CAN\22229\&3t-V\83293?\1019171I@5\1065175\STX\11407\DLE\ACK\1012398]D-\1078867U?\1073138\&6\172153\48875\NAK\v\t*X~JNS\137081\30717\DC13r\49961\24581u*iD\DC23\ETX\1024431y?`e;H\178150\\\1417\"*np\144459\141328\74077\141898+\USs.\172107\&0\133720_yr\21048EkS="
          },
      botUserViewColour = ColourId {fromColourId = -8},
      botUserViewHandle =
        Just (fromJust (parseHandle "p35n6vhgb5sh71n.-har73f0tp1urvyml_5ni8n01ommlrlx5chb9z7bhp_rehr1geua0--yxs5x3m3dgmvhy8-a-07gbc0owxv2d9mj_pqzss9op.ovxyrid8l36nkw1b5f4sr2.li7bmtmcwe76.zxj9lwbqtqt8v77v6ncnmebtl3whz6790x34rcyqe.jxc6glk2-7d.janj7d1.c70bjkjpzqp0pi64hoiei854tefqdlz246bht")),
      botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0004-0000-000700000004")))
    }

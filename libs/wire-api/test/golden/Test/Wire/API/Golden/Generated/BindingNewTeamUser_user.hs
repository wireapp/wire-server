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

module Test.Wire.API.Golden.Generated.BindingNewTeamUser_user where

import Data.Currency (Alpha (XUA))
import Data.Id (Id (..))
import Data.Range (unsafeRange)
import Data.UUID as UUID
import Imports (Maybe (Just, Nothing), fromJust)
import Wire.API.Asset
import Wire.API.Team
import Wire.API.User (BindingNewTeamUser (..))

testObject_BindingNewTeamUser_user_1 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_1 =
  BindingNewTeamUser
    { bnuTeam =
        NewTeam
          { _newTeamName =
              unsafeRange
                "\fe\ENQ\1011760zm\166331\&6+)g;5\989956Z\8196\&41\DC1\n\STX\ETX%|\NULM\996272S=`I\59956UK1\1003466]X\r\SUBa\EM!\74407+\ETXepRw\ACK\ENQ#\127835\1061771\1036174\1018930UX\66821]>i&r\137805\1055913Z\1070413\&6\DC4\DC4\1024114\1058863\1044802\ESC\SYNa4\NUL\1059602\1015948\123628\tLZ\ACKw$=\SYNu\ETXE1\63200C'\ENQ\151764\47003\134542$\100516\1112326\&9;#\1044763\1015439&\ESC\1026916k/\tu\\pk\NUL\STX\1083510)\FS/Lni]Q\NUL\SIZ|=\DC1V]]\FS5\156475U6>(\17233'\CAN\179678%'I1-D\"\1098303\n\78699\npkHY#\NUL\1014868u]\1078674\147414\STX\USj'\993967'\CAN\1042144&\35396E\37802=\135058Da\STX\v\1100351=\1083565V#\993183\RS\FSN#`uny\1003178\1094898\&53#\DEL/|,+\243pW\44721i4j",
            _newTeamIcon = DefaultIcon,
            _newTeamIconKey =
              Just
                ( unsafeRange
                    "\ACKc\151665L ,\STX\NAK[\SUB\DC1\63043\GSxe\1000559c\US\DC4<`|\29113\147003Q\1028347\987929<{\NUL^\FST\141040J\1071963U\EOT\SYN\65033\DC3G\1003198+\EM\181213xr\v\32449\ESCyTD@>Ou\70496j\43574E\STX6e\983711\SO\ESC\135327\&34\1063210\41000\1018151\&8\1057958\163400uxW\41951\1080957Y\ACK\141633(\CAN\FS$D\1055410\148196\36291\SI3\1082544#\SYN?\ETX\ACK0*W3\ACK\1085759i\35231h\NAK-\42529\1034909\ACKH?\\Tv\1098776\54330Q\46933\DLE-@k%{=4\SUB!w&\1042435D\DC2cuT^\DC4\GSH\b\137953^]\985924jXA\1010085\133569@fV,OA\185077\38677F\154006Az^g7\177712),C\1020911}.\72736\996321~V\1077077\1024186(9^z\1014725\67354\&3}Gj\1078379\fd>\57781\1088153Y\177269p#^\1054503L`S~\1101440\DC23\EOT\145319\24591\92747\13418as:F\ETX"
                )
          },
      bnuCurrency = Just XUA
    }

testObject_BindingNewTeamUser_user_2 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_2 =
  BindingNewTeamUser
    { bnuTeam =
        NewTeam
          { _newTeamName =
              unsafeRange
                "G\EOT\DC47\1030077bCy\83226&5\"\96437B$\STX\DC2QJb_\15727\1104659Y \156055\1044397Y\1004994g\v\991186xkJUi\1028168.=-\1054839\&2\1113630U\ESC]\SUB\1091929\DLE}R\157290\DC1\1111740\1096562+R/\1083774\170894p(M\ENQ5Fw<\144133E\1005699R\DLE44\1060383\SO%@FPG\986135JJ\vE\GSz\RS_\tb]0t_Ax}\rt\1057458h\DC3O\ACK\991050`\1038022vm-?$!)~\152722bh\RS\1011653\1007510\&0x \1092001\1078327+)A&mRfL\1109449\ENQ\1049319>K@\US\1006511\ab\vPDWG,\1062888/J~)%7?aRr\989765\&4*^\1035118K*\996771\EM\"\SO\987994\186383l\n\tE\136474\1037228\NAK\a\n\78251c?\\\ENQj\"\ESCpe\98450\NUL=\EM>J",
            _newTeamIcon = Icon (AssetKeyV3 (Id (fromJust (UUID.fromString "55b9ad19-315c-4bda-8c0f-5d7b0e143008"))) AssetEternal),
            _newTeamIconKey =
              Just
                ( unsafeRange
                    "-\ACK\59597v^\SOH_>p\13939\ETX\SYN\EOT\ENQ\2922\1080262]\45888\917616\SI;v}q\47502\190968\a\SI\1113366&~\51980<\GS\1024632`,\1033586sn\2651H\160130\1100746\176758:qNi]\1051932'\1000100#\a#T\171243}\990743\DC2\1008291M_\FS\DC4\988716\1091854\EM,\SO\CAN^]\77867\&9\1112574-\a\SOHID. FAp\EOT\1033411\1004852(S\1052010\68416\129120\DLEsI\ETXe|Mv-\"q\49103zM\14348$H\SOH\139130\1004399D]\SUB\1056469\ESC\151220qW2\ENQ\1104272\RSy\1018323gg\1018839 /\1079527\98975\18928~&y\b\ACK\1084334\1047493\36198\SO\FS\SYN\RSt\\a.V\SO\&Hy8k\US$O\699Xu/="
                )
          },
      bnuCurrency = Nothing
    }

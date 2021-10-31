{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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
module Test.Wire.API.Golden.Generated.UserIdentity_user where

import Imports (Maybe (Just, Nothing))
import Wire.API.User
  ( Email (Email, emailDomain, emailLocal),
    Phone (Phone, fromPhone),
    UserIdentity (..),
    UserSSOId (UserSSOId, UserScimExternalId),
  )
import Wire.API.User.Identity (mkSimpleSampleUref)

testObject_UserIdentity_user_1 :: UserIdentity
testObject_UserIdentity_user_1 =
  EmailIdentity (Email {emailLocal = "S\ENQX\1076723$\STX\"\1110507e\1015716\24831\1031964L\ETB", emailDomain = "P.b"})

testObject_UserIdentity_user_2 :: UserIdentity
testObject_UserIdentity_user_2 =
  EmailIdentity
    ( Email
        { emailLocal = "\1061008\1068189\1013266\EOT\vE\ENQW\SYNO\DC3X_F\9141\STX $}\179559\USJ3\128480S?",
          emailDomain = "4WL;'\DLEl1]x\119077"
        }
    )

testObject_UserIdentity_user_3 :: UserIdentity
testObject_UserIdentity_user_3 =
  EmailIdentity
    ( Email
        { emailLocal = "\10821:\DC4E\60072i\1074224P\1054022\1037567\&6phe\DC3\ETXH,\CAN\v\145604\v>",
          emailDomain = "bwtC\1110390z2RT28\STX\1049837<3Y"
        }
    )

testObject_UserIdentity_user_4 :: UserIdentity
testObject_UserIdentity_user_4 =
  FullIdentity
    (Email {emailLocal = "\rH)\65718", emailDomain = ")\1107842\US\27126\t\ACK\1111725_{\154804\&7#"})
    (Phone {fromPhone = "+2559583362"})

testObject_UserIdentity_user_5 :: UserIdentity
testObject_UserIdentity_user_5 =
  SSOIdentity (UserSSOId mkSimpleSampleUref) Nothing (Just (Phone {fromPhone = "+49198172826"}))

testObject_UserIdentity_user_6 :: UserIdentity
testObject_UserIdentity_user_6 = PhoneIdentity (Phone {fromPhone = "+03038459796465"})

testObject_UserIdentity_user_7 :: UserIdentity
testObject_UserIdentity_user_7 = PhoneIdentity (Phone {fromPhone = "+805676294"})

testObject_UserIdentity_user_8 :: UserIdentity
testObject_UserIdentity_user_8 = SSOIdentity (UserSSOId mkSimpleSampleUref) Nothing (Just (Phone {fromPhone = "+149548802116267"}))

testObject_UserIdentity_user_9 :: UserIdentity
testObject_UserIdentity_user_9 =
  EmailIdentity
    ( Email
        { emailLocal = "'\ACKB\1000542\&90\NAKKK\EOTin\1096701r\EOT",
          emailDomain = "Jj\\\172302>nY\9522\987654VO\DC2Q\r_:$\7618\EOTc~H8e}{g"
        }
    )

testObject_UserIdentity_user_10 :: UserIdentity
testObject_UserIdentity_user_10 =
  EmailIdentity
    ( Email
        { emailLocal = "No\b\1006784b=`yl\133702p.w\1048001\142089\DC4\149735lm\183993&j9\a",
          emailDomain = "\1054243.1\1031882\ETB_\1053320Q\1087931z.Ywe\1016096\39626>"
        }
    )

testObject_UserIdentity_user_11 :: UserIdentity
testObject_UserIdentity_user_11 = PhoneIdentity (Phone {fromPhone = "+755837448"})

testObject_UserIdentity_user_12 :: UserIdentity
testObject_UserIdentity_user_12 =
  EmailIdentity (Email {emailLocal = "K\1012027\DC2", emailDomain = "\DC4N0Q\4986rva\NAK5\1080896+S\1070062;\FS%\NAK"})

testObject_UserIdentity_user_13 :: UserIdentity
testObject_UserIdentity_user_13 =
  FullIdentity
    (Email {emailLocal = "e\ACK\1036331\1062258vN:%\1058229\SUBSi\1035816Qq", emailDomain = ""})
    (Phone {fromPhone = "+387350906"})

testObject_UserIdentity_user_14 :: UserIdentity
testObject_UserIdentity_user_14 =
  FullIdentity
    ( Email
        { emailLocal = "\1004575\184062\CAN\92545\&3\US<=gg",
          emailDomain = "\1035369\1022539Nbo\tQ:\1085902f\136614L\1009643"
        }
    )
    (Phone {fromPhone = "+79378139213406"})

testObject_UserIdentity_user_15 :: UserIdentity
testObject_UserIdentity_user_15 = PhoneIdentity (Phone {fromPhone = "+092380942233194"})

testObject_UserIdentity_user_16 :: UserIdentity
testObject_UserIdentity_user_16 =
  SSOIdentity
    (UserSSOId mkSimpleSampleUref)
    (Just (Email {emailLocal = "%x\DC3\1049873\EOT.", emailDomain = "G\48751t.6"}))
    (Just (Phone {fromPhone = "+298116118047"}))

testObject_UserIdentity_user_17 :: UserIdentity
testObject_UserIdentity_user_17 =
  SSOIdentity (UserScimExternalId "") (Just (Email {emailLocal = "\GS\FS1k", emailDomain = "CV7\147439K"})) Nothing

testObject_UserIdentity_user_18 :: UserIdentity
testObject_UserIdentity_user_18 = PhoneIdentity (Phone {fromPhone = "+7322674905"})

testObject_UserIdentity_user_19 :: UserIdentity
testObject_UserIdentity_user_19 = PhoneIdentity (Phone {fromPhone = "+133514352685272"})

testObject_UserIdentity_user_20 :: UserIdentity
testObject_UserIdentity_user_20 =
  FullIdentity
    (Email {emailLocal = "\133292A", emailDomain = "|\1083873\1005880N<\DC3z9\NAKV;^\1015230"})
    (Phone {fromPhone = "+926403020"})

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

module Test.Wire.API.Golden.Generated.LoginIdV5_user where

import Data.Handle (parseHandle)
import Data.Maybe
import Wire.API.User
import Wire.API.User.Auth

testObject_LoginIdV5_user_1 :: LoginIdV5
testObject_LoginIdV5_user_1 =
  LoginV5ByEmail
    (Email {emailLocal = "~]z^?j\NAK\1088399\1112814X{)\1087092t\f", emailDomain = "\1113045\n\vL$\ENQY\NUL\DELUj?H%"})

testObject_LoginIdV5_user_2 :: LoginIdV5
testObject_LoginIdV5_user_2 = LoginV5ByPhone (Phone {fromPhone = "+178807168"})

testObject_LoginIdV5_user_3 :: LoginIdV5
testObject_LoginIdV5_user_3 =
  LoginV5ByEmail
    ( Email
        { emailLocal = "0\1088863^\1000125\144267\NUL)|\183379:",
          emailDomain = "q6e/$\1033221Zb\1050001)\991223\&05i\20077~q\1071660\128584y"
        }
    )

testObject_LoginIdV5_user_4 :: LoginIdV5
testObject_LoginIdV5_user_4 = LoginV5ByHandle (fromJust (parseHandle "7a8gg3v98"))

testObject_LoginIdV5_user_5 :: LoginIdV5
testObject_LoginIdV5_user_5 = LoginV5ByPhone (Phone {fromPhone = "+041157889572"})

testObject_LoginIdV5_user_6 :: LoginIdV5
testObject_LoginIdV5_user_6 = LoginV5ByPhone (Phone {fromPhone = "+2351341820189"})

testObject_LoginIdV5_user_7 :: LoginIdV5
testObject_LoginIdV5_user_7 = LoginV5ByHandle (fromJust (parseHandle "lb"))

testObject_LoginIdV5_user_8 :: LoginIdV5
testObject_LoginIdV5_user_8 = LoginV5ByPhone (Phone {fromPhone = "+2831673805093"})

testObject_LoginIdV5_user_9 :: LoginIdV5
testObject_LoginIdV5_user_9 = LoginV5ByPhone (Phone {fromPhone = "+1091378734554"})

testObject_LoginIdV5_user_10 :: LoginIdV5
testObject_LoginIdV5_user_10 =
  LoginV5ByHandle (fromJust (parseHandle "z58-6fbjhtx11d8t6oplyijpkc2.fp_lf3kpk3_.qle4iecjun2xd0tpcordlg2bwv636v3cthpgwah3undqmuofgzp8ry6gc6g-n-kxnj7sl6771hxou7-t_ps_lu_t3.4ukz6dh6fkjq2i3aggtkbpzbd1162.qv.rbtb6e.90-xpayg65z9t9lk2aur452zcs9a"))

testObject_LoginIdV5_user_11 :: LoginIdV5
testObject_LoginIdV5_user_11 =
  LoginV5ByEmail
    ( Email
        { emailLocal = "\154036\140469A\1031528ovP Ig\92578t';\6199\SOHC\29188\157632{\n%\1090626\v2\GS\180557\1112803&",
          emailDomain = "m\180009U{f&.3\3846\&1?Ew\30701G-"
        }
    )

testObject_LoginIdV5_user_12 :: LoginIdV5
testObject_LoginIdV5_user_12 =
  LoginV5ByEmail (Email {emailLocal = "", emailDomain = "\18232\EM+h\ENQ(D\SO\28757\993545 \a\r1"})

testObject_LoginIdV5_user_13 :: LoginIdV5
testObject_LoginIdV5_user_13 =
  LoginV5ByEmail
    ( Email
        { emailLocal = "5-h\1094050\1011032&$og\1084464\26226\989383<%\2855\fGF-yJ\f*cK",
          emailDomain = "*g\EM\120758\&7$L\CAN\59033\57589\tV\1102330D\a\\yK\1090380T"
        }
    )

testObject_LoginIdV5_user_14 :: LoginIdV5
testObject_LoginIdV5_user_14 = LoginV5ByPhone (Phone {fromPhone = "+8668821360611"})

testObject_LoginIdV5_user_15 :: LoginIdV5
testObject_LoginIdV5_user_15 =
  LoginV5ByEmail
    ( Email
        { emailLocal = "\ACK\ENQX\ACK&\94893\&8\1044677\&7E`Y'\DC1TV\ACK\DLE",
          emailDomain = "\GS\ESCj\999191,j\994949\1043277#a1)}\DC3Vk\SOHQ7&;"
        }
    )

testObject_LoginIdV5_user_16 :: LoginIdV5
testObject_LoginIdV5_user_16 =
  LoginV5ByEmail
    ( Email
        { emailLocal = "\1013039\&1",
          emailDomain =
            "\v`\EM\49692v\1082687;F\18618\&0\4155Sgu%>\1076869y\v\1018080\NAK\133308\US\1025555\ACKs\SI\a\US"
        }
    )

testObject_LoginIdV5_user_17 :: LoginIdV5
testObject_LoginIdV5_user_17 = LoginV5ByHandle (fromJust (parseHandle "e3iusdy"))

testObject_LoginIdV5_user_18 :: LoginIdV5
testObject_LoginIdV5_user_18 =
  LoginV5ByHandle (fromJust (parseHandle "8vpices3usz1dfs4u2lf_e3jendod_szl1z111_eoj4b7k7ajj-xo.qzbw4espf3smnz_"))

testObject_LoginIdV5_user_19 :: LoginIdV5
testObject_LoginIdV5_user_19 = LoginV5ByHandle (fromJust (parseHandle "3jzpp2bo8"))

testObject_LoginIdV5_user_20 :: LoginIdV5
testObject_LoginIdV5_user_20 = LoginV5ByEmail (Email {emailLocal = "", emailDomain = "\155899"})

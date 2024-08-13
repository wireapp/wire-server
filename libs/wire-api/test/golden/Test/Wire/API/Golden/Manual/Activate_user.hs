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

module Test.Wire.API.Golden.Manual.Activate_user where

import Data.Text.Ascii (AsciiChars (validate))
import Imports (Bool (False, True), fromRight, undefined)
import Wire.API.User
import Wire.API.User.Activation

testObject_Activate_user_1 :: Activate
testObject_Activate_user_1 =
  Activate
    { activateTarget =
        ActivateKey (ActivationKey {fromActivationKey = fromRight undefined (validate "e3sm9EjNmzA=")}),
      activateCode = ActivationCode {fromActivationCode = fromRight undefined (validate "fg==")},
      activateDryrun = False
    }

testObject_Activate_user_2 :: Activate
testObject_Activate_user_2 =
  Activate
    { activateTarget = ActivateEmail (unsafeEmailAddress "\1002810\NUL\1075125" "k\\\SOHa\SYN*\176499"),
      activateCode = ActivationCode {fromActivationCode = fromRight undefined (validate "")},
      activateDryrun = False
    }

testObject_Activate_user_3 :: Activate
testObject_Activate_user_3 =
  Activate
    { activateTarget = ActivateKey (ActivationKey {fromActivationKey = fromRight undefined (validate "DkV9xQ==")}),
      activateCode = ActivationCode {fromActivationCode = fromRight undefined (validate "61wG")},
      activateDryrun = True
    }

testObject_Activate_user_4 :: Activate
testObject_Activate_user_4 =
  Activate
    { activateTarget = ActivateKey (ActivationKey {fromActivationKey = fromRight undefined (validate "V3mr5D4=")}),
      activateCode = ActivationCode {fromActivationCode = fromRight undefined (validate "sScBopoNTb0=")},
      activateDryrun = True
    }

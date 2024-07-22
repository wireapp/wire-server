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

module Test.Wire.API.Golden.Generated.ActivateV5_user where

import Data.Text.Ascii (AsciiChars (validate))
import Imports (Bool (False, True), fromRight, undefined)
import Wire.API.User
import Wire.API.User.Activation

testObject_ActivateV5_user_1 :: ActivateV5
testObject_ActivateV5_user_1 =
  ActivateV5
    { activateV5Target = ActivateV5Phone (Phone {fromPhone = "+45520903"}),
      activateV5Code = ActivationCode {fromActivationCode = fromRight undefined (validate "HUUpJQ==")},
      activateV5Dryrun = True
    }

testObject_ActivateV5_user_2 :: ActivateV5
testObject_ActivateV5_user_2 =
  ActivateV5
    { activateV5Target =
        ActivateV5Key (ActivationKey {fromActivationKey = fromRight undefined (validate "e3sm9EjNmzA=")}),
      activateV5Code = ActivationCode {fromActivationCode = fromRight undefined (validate "fg==")},
      activateV5Dryrun = False
    }

testObject_ActivateV5_user_3 :: ActivateV5
testObject_ActivateV5_user_3 =
  ActivateV5
    { activateV5Target = ActivateV5Phone (Phone {fromPhone = "+44508058"}),
      activateV5Code = ActivationCode {fromActivationCode = fromRight undefined (validate "OAbwDkw=")},
      activateV5Dryrun = True
    }

testObject_ActivateV5_user_4 :: ActivateV5
testObject_ActivateV5_user_4 =
  ActivateV5
    { activateV5Target = ActivateV5Phone (Phone {fromPhone = "+97751884"}),
      activateV5Code = ActivationCode {fromActivationCode = fromRight undefined (validate "811p-743Gvpi")},
      activateV5Dryrun = False
    }

testObject_ActivateV5_user_5 :: ActivateV5
testObject_ActivateV5_user_5 =
  ActivateV5
    { activateV5Target = ActivateV5Email (Email {emailLocal = "\1002810\NUL\1075125", emailDomain = "k\\\SOHa\SYN*\176499"}),
      activateV5Code = ActivationCode {fromActivationCode = fromRight undefined (validate "")},
      activateV5Dryrun = False
    }

testObject_ActivateV5_user_6 :: ActivateV5
testObject_ActivateV5_user_6 =
  ActivateV5
    { activateV5Target = ActivateV5Email (Email {emailLocal = "\1104323i>\1007870Ha!", emailDomain = ""}),
      activateV5Code = ActivationCode {fromActivationCode = fromRight undefined (validate "FXrNll0Kqg==")},
      activateV5Dryrun = False
    }

testObject_ActivateV5_user_7 :: ActivateV5
testObject_ActivateV5_user_7 =
  ActivateV5
    { activateV5Target = ActivateV5Key (ActivationKey {fromActivationKey = fromRight undefined (validate "jQ==")}),
      activateV5Code = ActivationCode {fromActivationCode = fromRight undefined (validate "8yl3qERc")},
      activateV5Dryrun = False
    }

testObject_ActivateV5_user_8 :: ActivateV5
testObject_ActivateV5_user_8 =
  ActivateV5
    { activateV5Target = ActivateV5Phone (Phone {fromPhone = "+3276478697350"}),
      activateV5Code = ActivationCode {fromActivationCode = fromRight undefined (validate "NF20Avw=")},
      activateV5Dryrun = True
    }

testObject_ActivateV5_user_9 :: ActivateV5
testObject_ActivateV5_user_9 =
  ActivateV5
    { activateV5Target = ActivateV5Key (ActivationKey {fromActivationKey = fromRight undefined (validate "DkV9xQ==")}),
      activateV5Code = ActivationCode {fromActivationCode = fromRight undefined (validate "61wG")},
      activateV5Dryrun = True
    }

testObject_ActivateV5_user_10 :: ActivateV5
testObject_ActivateV5_user_10 =
  ActivateV5
    { activateV5Target = ActivateV5Key (ActivationKey {fromActivationKey = fromRight undefined (validate "1szizA==")}),
      activateV5Code = ActivationCode {fromActivationCode = fromRight undefined (validate "kcvCq2A=")},
      activateV5Dryrun = False
    }

testObject_ActivateV5_user_11 :: ActivateV5
testObject_ActivateV5_user_11 =
  ActivateV5
    { activateV5Target = ActivateV5Email (Email {emailLocal = "\ETX4\SUB", emailDomain = ""}),
      activateV5Code = ActivationCode {fromActivationCode = fromRight undefined (validate "MZpmmg==")},
      activateV5Dryrun = False
    }

testObject_ActivateV5_user_12 :: ActivateV5
testObject_ActivateV5_user_12 =
  ActivateV5
    { activateV5Target = ActivateV5Key (ActivationKey {fromActivationKey = fromRight undefined (validate "V3mr5D4=")}),
      activateV5Code = ActivationCode {fromActivationCode = fromRight undefined (validate "sScBopoNTb0=")},
      activateV5Dryrun = True
    }

testObject_ActivateV5_user_13 :: ActivateV5
testObject_ActivateV5_user_13 =
  ActivateV5
    { activateV5Target =
        ActivateV5Key (ActivationKey {fromActivationKey = fromRight undefined (validate "haH9_sUNFw==")}),
      activateV5Code = ActivationCode {fromActivationCode = fromRight undefined (validate "ysvb")},
      activateV5Dryrun = False
    }

testObject_ActivateV5_user_14 :: ActivateV5
testObject_ActivateV5_user_14 =
  ActivateV5
    { activateV5Target = ActivateV5Phone (Phone {fromPhone = "+13340815619"}),
      activateV5Code = ActivationCode {fromActivationCode = fromRight undefined (validate "hQ==")},
      activateV5Dryrun = True
    }

testObject_ActivateV5_user_15 :: ActivateV5
testObject_ActivateV5_user_15 =
  ActivateV5
    { activateV5Target =
        ActivateV5Email (Email {emailLocal = "\22308W[\1041599G\996204]{\n", emailDomain = " V8\992253\NAK*"}),
      activateV5Code = ActivationCode {fromActivationCode = fromRight undefined (validate "biTZ")},
      activateV5Dryrun = False
    }

testObject_ActivateV5_user_16 :: ActivateV5
testObject_ActivateV5_user_16 =
  ActivateV5
    { activateV5Target = ActivateV5Phone (Phone {fromPhone = "+77635104433"}),
      activateV5Code = ActivationCode {fromActivationCode = fromRight undefined (validate "5W4=")},
      activateV5Dryrun = True
    }

testObject_ActivateV5_user_17 :: ActivateV5
testObject_ActivateV5_user_17 =
  ActivateV5
    { activateV5Target = ActivateV5Phone (Phone {fromPhone = "+556856857856"}),
      activateV5Code = ActivationCode {fromActivationCode = fromRight undefined (validate "ShjEcgx6P0Hs")},
      activateV5Dryrun = False
    }

testObject_ActivateV5_user_18 :: ActivateV5
testObject_ActivateV5_user_18 =
  ActivateV5
    { activateV5Target =
        ActivateV5Email (Email {emailLocal = "2\1107376B\1099134\ETX2\US\1080331", emailDomain = "v\SOH\SO\1007855/e"}),
      activateV5Code = ActivationCode {fromActivationCode = fromRight undefined (validate "xRvktQ==")},
      activateV5Dryrun = False
    }

testObject_ActivateV5_user_19 :: ActivateV5
testObject_ActivateV5_user_19 =
  ActivateV5
    { activateV5Target = ActivateV5Key (ActivationKey {fromActivationKey = fromRight undefined (validate "1fCrdg==")}),
      activateV5Code = ActivationCode {fromActivationCode = fromRight undefined (validate "")},
      activateV5Dryrun = False
    }

testObject_ActivateV5_user_20 :: ActivateV5
testObject_ActivateV5_user_20 =
  ActivateV5
    { activateV5Target = ActivateV5Phone (Phone {fromPhone = "+893051142276"}),
      activateV5Code = ActivationCode {fromActivationCode = fromRight undefined (validate "7PtclAevMzA=")},
      activateV5Dryrun = False
    }

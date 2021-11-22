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
module Test.Wire.API.Golden.Generated.NewServiceResponse_provider where

import Data.Id (Id (Id))
import Data.Text.Ascii (AsciiChars (validate))
import qualified Data.UUID as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust, fromRight, undefined)
import Wire.API.Provider (ServiceToken (ServiceToken))
import Wire.API.Provider.Service (NewServiceResponse (..))

testObject_NewServiceResponse_provider_1 :: NewServiceResponse
testObject_NewServiceResponse_provider_1 =
  NewServiceResponse
    { rsNewServiceId = (Id (fromJust (UUID.fromString "0000007f-0000-0076-0000-00140000003c"))),
      rsNewServiceToken = Nothing
    }

testObject_NewServiceResponse_provider_2 :: NewServiceResponse
testObject_NewServiceResponse_provider_2 =
  NewServiceResponse
    { rsNewServiceId = (Id (fromJust (UUID.fromString "0000000b-0000-001a-0000-00760000003c"))),
      rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate (""))))
    }

testObject_NewServiceResponse_provider_3 :: NewServiceResponse
testObject_NewServiceResponse_provider_3 =
  NewServiceResponse
    { rsNewServiceId = (Id (fromJust (UUID.fromString "0000006d-0000-0017-0000-003200000046"))),
      rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("r5t59oHh0QO-LabQ"))))
    }

testObject_NewServiceResponse_provider_4 :: NewServiceResponse
testObject_NewServiceResponse_provider_4 =
  NewServiceResponse
    { rsNewServiceId = (Id (fromJust (UUID.fromString "00000031-0000-0070-0000-001500000009"))),
      rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("Vg=="))))
    }

testObject_NewServiceResponse_provider_5 :: NewServiceResponse
testObject_NewServiceResponse_provider_5 =
  NewServiceResponse
    { rsNewServiceId = (Id (fromJust (UUID.fromString "00000011-0000-003a-0000-005600000042"))),
      rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("xriWfCcjSkRgzsyR7Q=="))))
    }

testObject_NewServiceResponse_provider_6 :: NewServiceResponse
testObject_NewServiceResponse_provider_6 =
  NewServiceResponse
    { rsNewServiceId = (Id (fromJust (UUID.fromString "00000052-0000-006a-0000-000600000016"))),
      rsNewServiceToken = Nothing
    }

testObject_NewServiceResponse_provider_7 :: NewServiceResponse
testObject_NewServiceResponse_provider_7 =
  NewServiceResponse
    { rsNewServiceId = (Id (fromJust (UUID.fromString "00000064-0000-0068-0000-001c00000050"))),
      rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("ZYnc99Mbj9vs6Q=="))))
    }

testObject_NewServiceResponse_provider_8 :: NewServiceResponse
testObject_NewServiceResponse_provider_8 =
  NewServiceResponse
    { rsNewServiceId = (Id (fromJust (UUID.fromString "00000040-0000-005b-0000-006600000011"))),
      rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("dQoSScSqO--gFA=="))))
    }

testObject_NewServiceResponse_provider_9 :: NewServiceResponse
testObject_NewServiceResponse_provider_9 =
  NewServiceResponse
    { rsNewServiceId = (Id (fromJust (UUID.fromString "00000042-0000-0041-0000-003700000015"))),
      rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("aJWKRU079Q=="))))
    }

testObject_NewServiceResponse_provider_10 :: NewServiceResponse
testObject_NewServiceResponse_provider_10 =
  NewServiceResponse
    { rsNewServiceId = (Id (fromJust (UUID.fromString "0000004e-0000-003b-0000-005d0000004f"))),
      rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate (""))))
    }

testObject_NewServiceResponse_provider_11 :: NewServiceResponse
testObject_NewServiceResponse_provider_11 =
  NewServiceResponse
    { rsNewServiceId = (Id (fromJust (UUID.fromString "00000022-0000-007e-0000-00590000002a"))),
      rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("fjxhvLuMAS6jsck="))))
    }

testObject_NewServiceResponse_provider_12 :: NewServiceResponse
testObject_NewServiceResponse_provider_12 =
  NewServiceResponse
    { rsNewServiceId = (Id (fromJust (UUID.fromString "00000049-0000-0046-0000-005a0000006e"))),
      rsNewServiceToken = Nothing
    }

testObject_NewServiceResponse_provider_13 :: NewServiceResponse
testObject_NewServiceResponse_provider_13 =
  NewServiceResponse
    { rsNewServiceId = (Id (fromJust (UUID.fromString "00000016-0000-000c-0000-002000000049"))),
      rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("gUQcN1bP_8h3BBlT4pw="))))
    }

testObject_NewServiceResponse_provider_14 :: NewServiceResponse
testObject_NewServiceResponse_provider_14 =
  NewServiceResponse
    { rsNewServiceId = (Id (fromJust (UUID.fromString "00000026-0000-006a-0000-004c00000031"))),
      rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("mMpX0PDqAg=="))))
    }

testObject_NewServiceResponse_provider_15 :: NewServiceResponse
testObject_NewServiceResponse_provider_15 =
  NewServiceResponse
    { rsNewServiceId = (Id (fromJust (UUID.fromString "00000068-0000-0041-0000-00180000006f"))),
      rsNewServiceToken = Nothing
    }

testObject_NewServiceResponse_provider_16 :: NewServiceResponse
testObject_NewServiceResponse_provider_16 =
  NewServiceResponse
    { rsNewServiceId = (Id (fromJust (UUID.fromString "00000018-0000-0046-0000-00200000000c"))),
      rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("Cwj-OA=="))))
    }

testObject_NewServiceResponse_provider_17 :: NewServiceResponse
testObject_NewServiceResponse_provider_17 =
  NewServiceResponse
    { rsNewServiceId = (Id (fromJust (UUID.fromString "0000003f-0000-001b-0000-006400000030"))),
      rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("3qpW2poPhag="))))
    }

testObject_NewServiceResponse_provider_18 :: NewServiceResponse
testObject_NewServiceResponse_provider_18 =
  NewServiceResponse
    { rsNewServiceId = (Id (fromJust (UUID.fromString "00000073-0000-006f-0000-00560000001a"))),
      rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("SdKMiA2x5Lm8xg=="))))
    }

testObject_NewServiceResponse_provider_19 :: NewServiceResponse
testObject_NewServiceResponse_provider_19 =
  NewServiceResponse
    { rsNewServiceId = (Id (fromJust (UUID.fromString "00000051-0000-001b-0000-00410000001c"))),
      rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("5fs3s32wpdzsZSw="))))
    }

testObject_NewServiceResponse_provider_20 :: NewServiceResponse
testObject_NewServiceResponse_provider_20 =
  NewServiceResponse
    { rsNewServiceId = (Id (fromJust (UUID.fromString "00000018-0000-0017-0000-000800000011"))),
      rsNewServiceToken = Just (ServiceToken (fromRight undefined (validate ("aGP5hjgYDA=="))))
    }

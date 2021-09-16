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
module Test.Wire.API.Golden.Generated.ConvMembers_user where

import Data.Domain (Domain (Domain))
import Data.Id (Id (Id))
import Data.Qualified (Qualified (Qualified))
import qualified Data.UUID as UUID (fromString)
import Imports (Bool (False, True), Maybe (Just, Nothing), fromJust)
import Wire.API.Conversation
  ( ConvMembers (..),
    Member
      ( Member,
        memConvRoleName,
        memHidden,
        memHiddenRef,
        memId,
        memOtrArchived,
        memOtrArchivedRef,
        memOtrMutedRef,
        memOtrMutedStatus,
        memService
      ),
    OtherMember (..),
  )
import Wire.API.Conversation.Role (parseRoleName)
import Wire.API.Provider.Service (ServiceRef (ServiceRef, _serviceRefId, _serviceRefProvider))

domain :: Domain
domain = Domain "golden.example.com"

testObject_ConvMembers_user_1 :: ConvMembers
testObject_ConvMembers_user_1 =
  ConvMembers
    { cmSelf =
        Member
          { memId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
            memService =
              Just
                ( ServiceRef
                    { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
                      _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))
                    }
                ),
            memOtrMutedStatus = Nothing,
            memOtrMutedRef = Just "",
            memOtrArchived = True,
            memOtrArchivedRef = Just "",
            memHidden = False,
            memHiddenRef = Just "2",
            memConvRoleName =
              fromJust
                ( parseRoleName
                    "pqzher6cs67kz8fg0cd4o8aqs00kvkytkovzkjs1igz9eub_5xey_no8m2me3or8ukbtv05uq7gc54p6g52kwiygyqs3om7yu0istkixp_3395mkaxh9zljjyy8"
                )
          },
      cmOthers =
        [ OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000400000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001")),
                        _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))
                      }
                  ),
              omConvRoleName =
                fromJust
                  (parseRoleName "y4zf98vsd7b6zi1_3wch87_k8m0t8mpdhh8zlcq461s80oc0sl7yn85twxn89f7f4kwpd4_hj9q2m3za")
            }
        ]
    }

testObject_ConvMembers_user_2 :: ConvMembers
testObject_ConvMembers_user_2 =
  ConvMembers
    { cmSelf =
        Member
          { memId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
            memService =
              Just
                ( ServiceRef
                    { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
                      _serviceRefProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))
                    }
                ),
            memOtrMutedStatus = Nothing,
            memOtrMutedRef = Just "",
            memOtrArchived = False,
            memOtrArchivedRef = Nothing,
            memHidden = False,
            memHiddenRef = Just "",
            memConvRoleName = fromJust (parseRoleName "hoz2iwweprpt270t14yq_ge8dbej")
          },
      cmOthers = []
    }

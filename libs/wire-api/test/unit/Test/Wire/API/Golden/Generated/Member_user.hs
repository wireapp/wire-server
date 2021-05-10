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

module Test.Wire.API.Golden.Generated.Member_user where

import Data.Id (Id (Id))
import qualified Data.UUID as UUID (fromString)
import Imports
  ( Bool (False, True),
    Maybe (Just, Nothing),
    fromJust,
  )
import Wire.API.Conversation
  ( Member (..),
    MutedStatus (MutedStatus, fromMutedStatus),
  )
import Wire.API.Conversation.Role (parseRoleName)
import Wire.API.Provider.Service
  ( ServiceRef (ServiceRef, _serviceRefId, _serviceRefProvider),
  )

testObject_Member_user_1 :: Member
testObject_Member_user_1 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Nothing, memOtrArchived = False, memOtrArchivedRef = Just "\140694", memHidden = True, memHiddenRef = Just "\1032750", memConvRoleName = (fromJust (parseRoleName "q4g4_8r4m6hz7hx5ob32nexko2ntb3dmv5vogdmm8dhbwzei6rv45b_90kzg11gw6zsq"))}

testObject_Member_user_2 :: Member
testObject_Member_user_2 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000002"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "\DEL#\173146", memHidden = True, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "cxrqwei1me7ftnrql1p2ew9aj1c5um89xip09ymj6wyj5cqfc4s903yxpv9e5j1j_8744acstc_a"))}

testObject_Member_user_3 :: Member
testObject_Member_user_3 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Nothing, memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = False, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "m1h8210wgo2f5tg2jgqqmvb4_p6b8m6ycix5d8ci438oulya_al0mxbks6vhzu777afc5uagd5"))}

testObject_Member_user_4 :: Member
testObject_Member_user_4 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Nothing, memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = True, memHiddenRef = Just "{", memConvRoleName = (fromJust (parseRoleName "tg09zlo0p6ubotujq"))}

testObject_Member_user_5 :: Member
testObject_Member_user_5 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "\1031068+\CAN", memConvRoleName = (fromJust (parseRoleName "7y0wbix6igwheorpywk1nm22ox2lr1_p4bna_v31w4gx8vrnlu87j13_z8u7w5x971b3aurv0s_ump5eahxvciig6s1n7x7h2dtnr8vsqd81zarrvbl53litl3_"))}

testObject_Member_user_6 :: Member
testObject_Member_user_6 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))}), memOtrMuted = True, memOtrMutedStatus = Nothing, memOtrMutedRef = Nothing, memOtrArchived = False, memOtrArchivedRef = Just "u?f", memHidden = True, memHiddenRef = Just "\136752", memConvRoleName = (fromJust (parseRoleName "7txy4uu2tmqtn9h79uyilsvkl932ofr2rr3d9mhy8_kpzctem3qyrqnyjwq1veuijjn3o1z4n5neix0c4ns7pxpyulwz3waxig0nci0d9dy02ed7_guomtgxajx"))}

testObject_Member_user_7 :: Member
testObject_Member_user_7 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "pv2oqq4__gmhtskkv9e4digue3d2is3wf1cp1sd4"))}

testObject_Member_user_8 :: Member
testObject_Member_user_8 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))}), memOtrMuted = True, memOtrMutedStatus = Nothing, memOtrMutedRef = Nothing, memOtrArchived = True, memOtrArchivedRef = Just "\998298,\172080", memHidden = False, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "633hv48jp9a2x4803m3wfv4mv1z7r688eo5kg6uh12hqwks5ewl0mvbvzeucg_831oxr0eggar6gp7k6441c5qlrfik"))}

testObject_Member_user_9 :: Member
testObject_Member_user_9 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "\\?", memOtrArchived = True, memOtrArchivedRef = Just ".", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "88txfajsznilzfsvr3a6lax5v2o_4f9q0utm29al2x45271loig1gyhcbzrm5hwx0w8lqhc2l4ql4enji8dx7f56zmotyn0rgyabkqd6yhxqjd5up2y"))}

testObject_Member_user_10 :: Member
testObject_Member_user_10 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "\1075676z", memOtrArchived = True, memOtrArchivedRef = Nothing, memHidden = False, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "8_k_cle199r7m2w7mvtc3tu55ycm5qnnxjs7buzjjyaydrhi"))}

testObject_Member_user_11 :: Member
testObject_Member_user_11 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))}), memOtrMuted = True, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "\165862\EOT", memHidden = False, memHiddenRef = Just "-]\158933", memConvRoleName = (fromJust (parseRoleName "gsg85grqngzal1y4ptw9qjadg67sv4uileboe_gle521hybeht"))}

testObject_Member_user_12 :: Member
testObject_Member_user_12 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))), memService = Nothing, memOtrMuted = False, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "R", memOtrArchived = False, memOtrArchivedRef = Just "\DC2\tn", memHidden = False, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "1bgt0uzs1sao2otf76egq864wn0sj0wxlj_bdak6a6adkpfiphoihxs3q55ao66w0_07yavk5kfxiazxbo2caba6pzuqfl2ce_dhgj3dc0nkcyasw0weid__v1ycd"))}

testObject_Member_user_13 :: Member
testObject_Member_user_13 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Nothing, memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "i_tncpnct2bq0zc2588w6rp_nzcc9bdp4y9o47b6nix_lxv9bnqqms308ta2_edqnmgkk8o3wgs44s47q9g5nv2u9122pl675lljzpxx65"))}

testObject_Member_user_14 :: Member
testObject_Member_user_14 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))}), memOtrMuted = True, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "*&", memOtrArchived = False, memOtrArchivedRef = Just "Iv", memHidden = True, memHiddenRef = Just "\"", memConvRoleName = (fromJust (parseRoleName "uuttutvr4y5qhlbn1u94e0u74zei_zilbp9bk5is3cvkqywo1cmmb6heqodku7poq2_y"))}

testObject_Member_user_15 :: Member
testObject_Member_user_15 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Nothing, memOtrMutedRef = Nothing, memOtrArchived = True, memOtrArchivedRef = Nothing, memHidden = False, memHiddenRef = Just "\NUL^", memConvRoleName = (fromJust (parseRoleName "480poa1dpixu9xztve6ybatlhmevixphxvay9cw"))}

testObject_Member_user_16 :: Member
testObject_Member_user_16 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Nothing, memOtrArchived = True, memOtrArchivedRef = Nothing, memHidden = True, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "n_ka0q5qzafpovvvcrpmjctgwp45pfampamwcyry615ffkldjfzdf_jhuf7m0vb2n9mk90r3krdcqkeutj21_89wqelz0mx7o63cf195cjtqaoi"))}

testObject_Member_user_17 :: Member
testObject_Member_user_17 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Just "x\25043", memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = True, memHiddenRef = Just "/+\DC1", memConvRoleName = (fromJust (parseRoleName "2sknhz0oe74sqrgglg5d"))}

testObject_Member_user_18 :: Member
testObject_Member_user_18 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))), memService = Nothing, memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Just "-`", memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "yx997merbgjglzysg93xqhhz6jxs9god5a"))}

testObject_Member_user_19 :: Member
testObject_Member_user_19 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Just "@c", memOtrArchived = True, memOtrArchivedRef = Just "~}", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "chucv55xke4fmjd9hsptz_5_5_z17ted3ugpzobp986y6won8kmmdublh"))}

testObject_Member_user_20 :: Member
testObject_Member_user_20 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Just ",\67228", memOtrArchived = False, memOtrArchivedRef = Just "'", memHidden = False, memHiddenRef = Just "q", memConvRoleName = (fromJust (parseRoleName "s8egh5lk5tru_qwecadtn4frf9lhgpv1yqj4ptdrbfgt6fhc4f6h54nheqialc"))}

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

module Test.Wire.API.Golden.Generated.NewConvManaged_user where

import Data.Id (Id (Id))
import Data.Misc (Milliseconds (Ms, ms))
import qualified Data.Set as Set (fromList)
import qualified Data.UUID as UUID (fromString)
import Imports (Bool (True), Maybe (Just, Nothing), fromJust)
import Wire.API.Conversation
  ( Access (CodeAccess, InviteAccess, LinkAccess, PrivateAccess),
    AccessRole
      ( ActivatedAccessRole,
        NonActivatedAccessRole,
        PrivateAccessRole,
        TeamAccessRole
      ),
    ConvTeamInfo (ConvTeamInfo, cnvManaged, cnvTeamId),
    NewConv
      ( NewConv,
        newConvAccess,
        newConvAccessRole,
        newConvMessageTimer,
        newConvName,
        newConvReceiptMode,
        newConvTeam,
        newConvUsers,
        newConvUsersRole
      ),
    NewConvManaged (..),
    ReceiptMode (ReceiptMode, unReceiptMode),
  )
import Wire.API.Conversation.Role (parseRoleName)

testObject_NewConvManaged_user_1 :: NewConvManaged
testObject_NewConvManaged_user_1 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Nothing, newConvAccess = Set.fromList [], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 193643728192048}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 4}), newConvUsersRole = (fromJust (parseRoleName "37q9eeybycp5972td4oo9_r7y16eh6n67z5spda8sffy8qv"))})

testObject_NewConvManaged_user_2 :: NewConvManaged
testObject_NewConvManaged_user_2 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000400000000")))], newConvName = Just "\995491\SUB5", newConvAccess = Set.fromList [PrivateAccess, InviteAccess, LinkAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 5509522199847054}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "bewzponl1a3c_l6ou"))})

testObject_NewConvManaged_user_3 :: NewConvManaged
testObject_NewConvManaged_user_3 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))], newConvName = Just "NwK", newConvAccess = Set.fromList [CodeAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 582808797322573}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "kbqqptmp0tna583vobrgtadyismkkpnjdwsef9jlgvezy00yu5u2rds0ng11vppapcn9n7enwrg7tkwxvg1mz_rh7pcoi_btpcyg5akueydofop60j"))})

testObject_NewConvManaged_user_4 :: NewConvManaged
testObject_NewConvManaged_user_4 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))], newConvName = Just "k\61561-", newConvAccess = Set.fromList [PrivateAccess, LinkAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "pa5z_izggck3l0sn9qc_yx06bwh_k1vf2drs3c8w35wyqwl3tco54d7lvnbh3udjzs8avs0j1dxr7v40ldpqgy4lszpnxx3f0hpy_37ofx30s9oa9t"))})

testObject_NewConvManaged_user_5 :: NewConvManaged
testObject_NewConvManaged_user_5 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))], newConvName = Just "v", newConvAccess = Set.fromList [], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 1570858821505994}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "mqkbqyi796z05v6jj4ijc7nl5unid6eq1t028pp9awv2_8bc61wq0zl"))})

testObject_NewConvManaged_user_6 :: NewConvManaged
testObject_NewConvManaged_user_6 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))], newConvName = Just "P\1098873\r", newConvAccess = Set.fromList [InviteAccess, CodeAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 6614365418177275}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "gf5j3kqv7_6g55q6clu47jn2iq5yy_zvnm2753m2llu04bhb5ct_v53u7kmwdesgs832yylb_v5eddllgbostkjj7qwv7"))})

testObject_NewConvManaged_user_7 :: NewConvManaged
testObject_NewConvManaged_user_7 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))], newConvName = Just "\CAN", newConvAccess = Set.fromList [], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 7417375067718994}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "ce4ylezzp_1s9b3xbw7akybntwbaa21p8ijqsk53ymljzx_kubjl4tjvrdwb8jjm21cznytrtaffnemverdd39vqvbfxn_pl_"))})

testObject_NewConvManaged_user_8 :: NewConvManaged
testObject_NewConvManaged_user_8 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))], newConvName = Just "&", newConvAccess = Set.fromList [], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "ms6f79wv82ftbu608wl9jeu8xatyhb1p1ck5t9yht9xqjcldet9kj6gp4b"))})

testObject_NewConvManaged_user_9 :: NewConvManaged
testObject_NewConvManaged_user_9 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Nothing, newConvAccess = Set.fromList [PrivateAccess, InviteAccess, LinkAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 2550845209410146}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "yd"))})

testObject_NewConvManaged_user_10 :: NewConvManaged
testObject_NewConvManaged_user_10 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))], newConvName = Just "z\1112901", newConvAccess = Set.fromList [LinkAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 8061252799624904}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "vakyrex99a9vi36iemc7wj4gu39s4wf30c2r_mqatgyer4y4dzypd_y7x3i9embufwc6e4nfuqvuvu9p72r8xdnmho615wrlkcr1h4e8tnokdio9t"))})

testObject_NewConvManaged_user_11 :: NewConvManaged
testObject_NewConvManaged_user_11 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "r", newConvAccess = Set.fromList [InviteAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 6292627004994884}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "z5r2jhv40n91iwidhdui7jaa6i"))})

testObject_NewConvManaged_user_12 :: NewConvManaged
testObject_NewConvManaged_user_12 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))], newConvName = Just "", newConvAccess = Set.fromList [PrivateAccess, CodeAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 7043412511612101}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "4ek91n56al0j6mc3ovtreftmizb2qoh05m_zozgr6ar1vorbh2gdx3b72gm_q65h815zuy_2qehwf9t20l3mabd53813168ccg"))})

testObject_NewConvManaged_user_13 :: NewConvManaged
testObject_NewConvManaged_user_13 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "\tB", newConvAccess = Set.fromList [PrivateAccess, InviteAccess, LinkAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "220zl1nunhfwpvjm85iz_ixp98rq1dlwnqp7s18efvnonl6skfzkqvkxanemogf7ok3q29y5jzd4prt6s6ybl0ko47iu_zux5x7"))})

testObject_NewConvManaged_user_14 :: NewConvManaged
testObject_NewConvManaged_user_14 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))], newConvName = Just "", newConvAccess = Set.fromList [CodeAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 8669416711689656}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "d1t6fg7c_ksqcoe6e308n2xblhwg8m7ahrnia88at1"))})

testObject_NewConvManaged_user_15 :: NewConvManaged
testObject_NewConvManaged_user_15 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))], newConvName = Nothing, newConvAccess = Set.fromList [PrivateAccess, CodeAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 1166285470102499}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "5uxmr3gdwwipozxh"))})

testObject_NewConvManaged_user_16 :: NewConvManaged
testObject_NewConvManaged_user_16 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "", newConvAccess = Set.fromList [InviteAccess, CodeAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 4425819976591162}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "dc5k1w34ghjhorv48fy8f3_ya4n8sq1vrr7oojpht2_tbfviuu9i43aaxgpce744vxs6ikex7q35mv17svnwotre29fm"))})

testObject_NewConvManaged_user_17 :: NewConvManaged
testObject_NewConvManaged_user_17 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))], newConvName = Just "", newConvAccess = Set.fromList [LinkAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 5065871950676797}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "faa_2g09yehf26736w2p59cuw_giwelkq2d_y4q5sj49n9"))})

testObject_NewConvManaged_user_18 :: NewConvManaged
testObject_NewConvManaged_user_18 = NewConvManaged (NewConv {newConvUsers = [], newConvName = Just "\36412\tJ", newConvAccess = Set.fromList [], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), cnvManaged = True}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "e9_exzeqnr163dd14s7van73vd_9lth079onlldtkmk"))})

testObject_NewConvManaged_user_19 :: NewConvManaged
testObject_NewConvManaged_user_19 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000400000002")))], newConvName = Just "", newConvAccess = Set.fromList [PrivateAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 8428756728484885}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "x2g84s2bn8uq_i6yizofg4xx9yvbseuw53u8oafwx5cwn26i5xl2ojio90cwv2kz0pl9p6hfrogrp"))})

testObject_NewConvManaged_user_20 :: NewConvManaged
testObject_NewConvManaged_user_20 = NewConvManaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))], newConvName = Just "\SOH?(", newConvAccess = Set.fromList [PrivateAccess, InviteAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), cnvManaged = True}), newConvMessageTimer = Just (Ms {ms = 6168723896440273}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "url0_7h6wig8d_ro8fwdmuqggbynbdkjshlg_ei8qqu"))})

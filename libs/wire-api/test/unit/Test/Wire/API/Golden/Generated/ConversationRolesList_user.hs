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
module Test.Wire.API.Golden.Generated.ConversationRolesList_user where

import qualified Data.Set as Set (fromList)
import Imports (Maybe (Just, Nothing), fromJust)
import Wire.API.Conversation.Role
  ( Action
      ( AddConversationMember,
        DeleteConversation,
        LeaveConversation,
        ModifyConversationAccess,
        ModifyConversationMessageTimer,
        ModifyConversationName,
        ModifyConversationReceiptMode,
        ModifyOtherConversationMember,
        RemoveConversationMember
      ),
    Actions (Actions),
    ConversationRolesList (..),
    parseRoleName,
    toConvRole,
  )

testObject_ConversationRolesList_user_1 :: ConversationRolesList
testObject_ConversationRolesList_user_1 =
  ConversationRolesList
    { convRolesList =
        [ ( fromJust
              ( toConvRole
                  (fromJust (parseRoleName "0g843hmarr"))
                  (Just ((Actions (Set.fromList [AddConversationMember, ModifyOtherConversationMember]))))
              )
          ),
          ( fromJust
              ( toConvRole
                  (fromJust (parseRoleName "8t_l205nb44v3byrnei9os7rah6d23arw3z"))
                  (Just ((Actions (Set.fromList [RemoveConversationMember, ModifyOtherConversationMember]))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))
        ]
    }

testObject_ConversationRolesList_user_2 :: ConversationRolesList
testObject_ConversationRolesList_user_2 =
  ConversationRolesList
    { convRolesList =
        [ ( fromJust
              ( toConvRole
                  (fromJust (parseRoleName "e83gfmq4_n2ei6jqgj565t3nuk6yu8wpuctbgspd1ja"))
                  (Just ((Actions (Set.fromList []))))
              )
          ),
          ( fromJust
              ( toConvRole
                  ( fromJust
                      ( parseRoleName
                          "s3ymkciq3hqvtcl5efbsucpb3rnis8lioqm377vq3eg46ghz2ewh3u3vcrscubfog_2zorb1z7zzx7saa01qepie8m_w011r6_unlz_6s0xskl8_cfusxpkbx1f"
                      )
                  )
                  (Just ((Actions (Set.fromList []))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
        ]
    }

testObject_ConversationRolesList_user_3 :: ConversationRolesList
testObject_ConversationRolesList_user_3 =
  ConversationRolesList
    { convRolesList =
        [ ( fromJust
              ( toConvRole
                  (fromJust (parseRoleName "heihzbym4l1etr1m_njqa19sqy"))
                  (Just ((Actions (Set.fromList [RemoveConversationMember]))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          ( fromJust
              ( toConvRole
                  ( fromJust
                      ( parseRoleName
                          "4ufidhmy67qfeix7mspuo2iwhdet_cygi8sm5_i3xlc3t7ijnu81a59dcdz1zyqcgutzwugfddd5i10nnzumfo90mntok"
                      )
                  )
                  (Just ((Actions (Set.fromList [DeleteConversation]))))
              )
          ),
          ( fromJust
              ( toConvRole
                  (fromJust (parseRoleName "ywdeyduc7ufos8m5zw15zgpm2jy006x6htzd7xt7qs4p36t401epiqvl928a1"))
                  (Just ((Actions (Set.fromList [ModifyConversationMessageTimer]))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          ( fromJust
              ( toConvRole
                  ( fromJust
                      ( parseRoleName
                          "2xpcq7dwfj3z2yiwuvucdgz71q9qka7cs6vnju6vpsnnuxvnwg563lt0wkld4s4xxwno3i1v446fws77a4wrhasgsccc3jv_gpubdxzmvevbw0ccbq"
                      )
                  )
                  (Just ((Actions (Set.fromList [RemoveConversationMember]))))
              )
          ),
          ( fromJust
              ( toConvRole
                  ( fromJust
                      ( parseRoleName
                          "ycok9utkz3_l_h99sao16bbv1fc7ky7oki9gzi3mx5yao_vmlgmin46fttl4arm31f8smcmuq0sc1xtnlu2ubkw2k6inrge9sxt8dkgf4mc_9h2w_2hheuwr6s"
                      )
                  )
                  (Just ((Actions (Set.fromList [ModifyConversationReceiptMode]))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          ( fromJust
              ( toConvRole
                  ( fromJust
                      (parseRoleName "cqsgxrpcsqpc9lh8avqirs5y0o7y5_sbf7oofqha55jl1enj1mjaaih39t7cmmcx2hi7akh2ksyr5al5")
                  )
                  (Just ((Actions (Set.fromList [ModifyConversationName]))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
        ]
    }

testObject_ConversationRolesList_user_4 :: ConversationRolesList
testObject_ConversationRolesList_user_4 = ConversationRolesList {convRolesList = []}

testObject_ConversationRolesList_user_5 :: ConversationRolesList
testObject_ConversationRolesList_user_5 =
  ConversationRolesList
    { convRolesList =
        [ ( fromJust
              ( toConvRole
                  (fromJust (parseRoleName "p2bm1_l6vbj4qefxizficnju6_iyl6t2sdlzbhif94i7p2n6s"))
                  (Just ((Actions (Set.fromList []))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          ( fromJust
              ( toConvRole
                  (fromJust (parseRoleName "b4hn3o_rowfzdef6uj4z7d3yvmrg7kf2"))
                  (Just ((Actions (Set.fromList []))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          ( fromJust
              ( toConvRole
                  (fromJust (parseRoleName "7bgbztsquv_u3j6tj5hgz4o2ajnhza6y16tla8al79rp8g1h0fha7f7ducn"))
                  (Just ((Actions (Set.fromList []))))
              )
          )
        ]
    }

testObject_ConversationRolesList_user_6 :: ConversationRolesList
testObject_ConversationRolesList_user_6 =
  ConversationRolesList
    { convRolesList =
        [ (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))
        ]
    }

testObject_ConversationRolesList_user_7 :: ConversationRolesList
testObject_ConversationRolesList_user_7 =
  ConversationRolesList
    { convRolesList =
        [ (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          ( fromJust
              ( toConvRole
                  (fromJust (parseRoleName "ksyk7k7cb739t77es2iluk3336btnq2y52938t4b7ugz2vl0bovd"))
                  (Just ((Actions (Set.fromList [ModifyConversationName]))))
              )
          ),
          ( fromJust
              ( toConvRole
                  (fromJust (parseRoleName "isfh6p6g_to3f9bc4af6qa_rz3xjw_p"))
                  (Just ((Actions (Set.fromList [DeleteConversation]))))
              )
          ),
          ( fromJust
              ( toConvRole
                  (fromJust (parseRoleName "gvone2nabpj79ryqs696wdglpf23xs65vv5cbyga3vutq"))
                  (Just ((Actions (Set.fromList [LeaveConversation]))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "i4trq6lq11")) (Just ((Actions (Set.fromList []))))))
        ]
    }

testObject_ConversationRolesList_user_8 :: ConversationRolesList
testObject_ConversationRolesList_user_8 =
  ConversationRolesList
    { convRolesList =
        [ (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          ( fromJust
              ( toConvRole
                  ( fromJust
                      ( parseRoleName
                          "l6osv5ffa9a4woj9d7f3eg66_4nx7uereh2f05wuqt8ulk0pgjjdvdnimc5hcvofbeysbpm6b2n88lghp715h_9hn0e02u9gqh4b0pz04_"
                      )
                  )
                  (Just ((Actions (Set.fromList []))))
              )
          )
        ]
    }

testObject_ConversationRolesList_user_9 :: ConversationRolesList
testObject_ConversationRolesList_user_9 =
  ConversationRolesList
    { convRolesList =
        [ ( fromJust
              ( toConvRole
                  ( fromJust
                      ( parseRoleName
                          "rmefq7syeavictewvu7hzpoc3tl2gqjoq0o0kxgjypwrkw30s_nhea59v4lxg4gq708wksgqmonkmep0czln_s45qu_blv8y9"
                      )
                  )
                  (Just ((Actions (Set.fromList [RemoveConversationMember, ModifyConversationAccess]))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
        ]
    }

testObject_ConversationRolesList_user_10 :: ConversationRolesList
testObject_ConversationRolesList_user_10 =
  ConversationRolesList
    { convRolesList =
        [ (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "hryh5cdsxc6mgbwf_q6")) (Just ((Actions (Set.fromList [])))))),
          ( fromJust
              ( toConvRole
                  ( fromJust
                      ( parseRoleName
                          "rg7zsbeqar03dc8tm0cpbbarywzgx7zfwx0dxixp52mui8m88wf0okpeq8e5g0szljl1ycz3a_9f_wizfvmzpuz05_e7ooxnzetu3u0j0td3ut9fslvprt"
                      )
                  )
                  (Just ((Actions (Set.fromList []))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          ( fromJust
              ( toConvRole
                  (fromJust (parseRoleName "308ppv2ivrzqmc2xu9_uj3rs4f3qmzyc"))
                  (Just ((Actions (Set.fromList []))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          ( fromJust
              ( toConvRole
                  ( fromJust
                      ( parseRoleName
                          "__vsppi2uimni18xnaeuxzv5j0x56w2oma53ln3ib14l1p56b_0rnwklk8ogkc2u3zuw1ypz4uty1epak92byhp42rq_a3"
                      )
                  )
                  (Just ((Actions (Set.fromList []))))
              )
          ),
          ( fromJust
              (toConvRole (fromJust (parseRoleName "k3z3lop8j_k")) (Just ((Actions (Set.fromList [DeleteConversation])))))
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
        ]
    }

testObject_ConversationRolesList_user_11 :: ConversationRolesList
testObject_ConversationRolesList_user_11 =
  ConversationRolesList
    { convRolesList =
        [ ( fromJust
              ( toConvRole
                  (fromJust (parseRoleName "bnsi81m0tcsgtrah39sunugjnf5cv4qe9cy2_4084vghyzey6giq6ttpkqms2je"))
                  ( Just
                      ( ( Actions
                            ( Set.fromList
                                [ AddConversationMember,
                                  RemoveConversationMember,
                                  ModifyConversationName,
                                  ModifyConversationAccess,
                                  ModifyOtherConversationMember,
                                  LeaveConversation,
                                  DeleteConversation
                                ]
                            )
                        )
                      )
                  )
              )
          )
        ]
    }

testObject_ConversationRolesList_user_12 :: ConversationRolesList
testObject_ConversationRolesList_user_12 =
  ConversationRolesList
    { convRolesList =
        [ (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          ( fromJust
              ( toConvRole
                  ( fromJust
                      (parseRoleName "b04tpm3tvoex80nz98e90lefymeti4w7sp1a4uwcvgm381j16byr2nessks63v0dtru96ckva6cnbh0")
                  )
                  (Just ((Actions (Set.fromList [AddConversationMember]))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
        ]
    }

testObject_ConversationRolesList_user_13 :: ConversationRolesList
testObject_ConversationRolesList_user_13 =
  ConversationRolesList
    { convRolesList =
        [ (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          ( fromJust
              ( toConvRole
                  (fromJust (parseRoleName "123mi1cdo99vdqdnp_ol1pg2h24bf__5_"))
                  (Just ((Actions (Set.fromList []))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "10u348q")) (Just ((Actions (Set.fromList [])))))),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          ( fromJust
              ( toConvRole
                  ( fromJust
                      ( parseRoleName
                          "omk31lw6wrxlnicz79hznnlx5eozskm3r3w3jhzipw2pdg3b09h5zglxifmqti2zjlqnm_hmr6g8op5vpn7lg4_h2tnrenrmgbtan5nqinchf1"
                      )
                  )
                  (Just ((Actions (Set.fromList []))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          ( fromJust
              ( toConvRole
                  (fromJust (parseRoleName "qdk4sqhbn9mku06gey_k0sm3ek3m8yt6wxb8shgph1m0ks00"))
                  (Just ((Actions (Set.fromList []))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))
        ]
    }

testObject_ConversationRolesList_user_14 :: ConversationRolesList
testObject_ConversationRolesList_user_14 =
  ConversationRolesList
    { convRolesList =
        [ (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          ( fromJust
              ( toConvRole
                  ( fromJust
                      (parseRoleName "h7b57x14ns6437z_mesx0j57zpsixjpd3wl0vpgo3ew2gytiyymsoj5hewbfhb_2dl1eswjbj88k_ww_zigj")
                  )
                  (Just ((Actions (Set.fromList []))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "djhu4mvus")) (Just ((Actions (Set.fromList [])))))),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
        ]
    }

testObject_ConversationRolesList_user_15 :: ConversationRolesList
testObject_ConversationRolesList_user_15 =
  ConversationRolesList
    { convRolesList =
        [ ( fromJust
              ( toConvRole
                  (fromJust (parseRoleName "bxb790gxo1o8dh99kofgn0m463fg_nupp5_rmlzn_ikitu8un98x3teiybvx7srcqejb2dx"))
                  (Just ((Actions (Set.fromList []))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          ( fromJust
              ( toConvRole
                  (fromJust (parseRoleName "vtkdq7dc620aog28hmkxr7j8n2_3xib0dxpgv28d8ypqn_nf61utf69v9t2dy6kepnd_1"))
                  (Just ((Actions (Set.fromList [ModifyOtherConversationMember]))))
              )
          )
        ]
    }

testObject_ConversationRolesList_user_16 :: ConversationRolesList
testObject_ConversationRolesList_user_16 =
  ConversationRolesList
    { convRolesList =
        [ (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          ( fromJust
              ( toConvRole
                  ( fromJust
                      ( parseRoleName
                          "jcqbg6ctrw91f_rayto59n4i786tg3n8acm4nf28qggd1zuxo4jgf5b59er3eard_pg1xogrdmizf1rgc3ksqo12qkhcaia2on6"
                      )
                  )
                  (Just ((Actions (Set.fromList []))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          ( fromJust
              ( toConvRole
                  ( fromJust
                      (parseRoleName "6am2fi2abl47qpqi6yjozqqc1szr1oxovggb_nhmjg2vpjablbiwia_b77azg7pqyk1v7691wspxv")
                  )
                  (Just ((Actions (Set.fromList []))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          ( fromJust
              ( toConvRole
                  (fromJust (parseRoleName "nuyqeanpyt42ye0u5_on94mdkt3j4gljn"))
                  (Just ((Actions (Set.fromList [ModifyConversationMessageTimer]))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          ( fromJust
              ( toConvRole
                  ( fromJust
                      ( parseRoleName
                          "fqqq98mmj2z_7_6dxfajrl4ml6hvmqxn57z9wfmwahaxd9kvdebv6hughxqzxn5w8jdogq3hbxn8qoq7w2ev6lwo6zvq15"
                      )
                  )
                  (Just ((Actions (Set.fromList [ModifyConversationReceiptMode]))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
        ]
    }

testObject_ConversationRolesList_user_17 :: ConversationRolesList
testObject_ConversationRolesList_user_17 = ConversationRolesList {convRolesList = []}

testObject_ConversationRolesList_user_18 :: ConversationRolesList
testObject_ConversationRolesList_user_18 =
  ConversationRolesList
    { convRolesList =
        [ (fromJust (toConvRole (fromJust (parseRoleName "s2cl6cisn_jaw94")) (Just ((Actions (Set.fromList [])))))),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          ( fromJust
              ( toConvRole
                  (fromJust (parseRoleName "5st295e8exhw_ozq5_fbzyztszh1thu16g3rwx4but8f3m"))
                  (Just ((Actions (Set.fromList [ModifyConversationName]))))
              )
          )
        ]
    }

testObject_ConversationRolesList_user_19 :: ConversationRolesList
testObject_ConversationRolesList_user_19 = ConversationRolesList {convRolesList = []}

testObject_ConversationRolesList_user_20 :: ConversationRolesList
testObject_ConversationRolesList_user_20 =
  ConversationRolesList
    { convRolesList =
        [ (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          ( fromJust
              ( toConvRole
                  (fromJust (parseRoleName "b85f9ja4y8xih_39u8j2z3w2w0pexfyyawadiq1qxhrnjvfluzq_p3fbmlb8h9ph9y"))
                  (Just ((Actions (Set.fromList []))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "5doti_")) (Just ((Actions (Set.fromList [])))))),
          ( fromJust
              ( toConvRole
                  (fromJust (parseRoleName "hj97_m0bam8n53ly3jy8k6oqcq4j3rfjn_4bphxakifcuy8uoqhd90cd"))
                  (Just ((Actions (Set.fromList []))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),
          ( fromJust
              ( toConvRole
                  (fromJust (parseRoleName "gla3ue0e95dzuyiy03bls5q2rj3bwzjs9hd"))
                  (Just ((Actions (Set.fromList [AddConversationMember]))))
              )
          ),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),
          (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))
        ]
    }

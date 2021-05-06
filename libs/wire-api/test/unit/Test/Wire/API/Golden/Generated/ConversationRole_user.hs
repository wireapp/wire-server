{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ConversationRole_user where
import qualified Data.Set as Set ( fromList )
import Imports ( Maybe(Nothing, Just), fromJust )
import Wire.API.Conversation.Role
    ( parseRoleName,
      toConvRole,
      Action(DeleteConversation, AddConversationMember,
             RemoveConversationMember, LeaveConversation,
             ModifyConversationName, ModifyConversationMessageTimer,
             ModifyConversationReceiptMode, ModifyConversationAccess,
             ModifyOtherConversationMember),
      Actions(Actions),
      ConversationRole )

testObject_ConversationRole_user_1 :: ConversationRole
testObject_ConversationRole_user_1 = (fromJust (toConvRole (fromJust (parseRoleName "j45hk7vwc4gqjhmr37lg")) (Just ((Actions (Set.fromList [ModifyConversationName,ModifyConversationAccess,DeleteConversation]))))))
testObject_ConversationRole_user_2 :: ConversationRole
testObject_ConversationRole_user_2 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
testObject_ConversationRole_user_3 :: ConversationRole
testObject_ConversationRole_user_3 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
testObject_ConversationRole_user_4 :: ConversationRole
testObject_ConversationRole_user_4 = (fromJust (toConvRole (fromJust (parseRoleName "z20odi8t1h3lp23mrv1jvrv05rcfda85ophsokfvp9moxpxmxlh0igrbrw06vkko2tn6xgqlu6lhoj7b9")) (Just ((Actions (Set.fromList [AddConversationMember,RemoveConversationMember,ModifyConversationMessageTimer,ModifyConversationAccess,LeaveConversation,DeleteConversation]))))))
testObject_ConversationRole_user_5 :: ConversationRole
testObject_ConversationRole_user_5 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
testObject_ConversationRole_user_6 :: ConversationRole
testObject_ConversationRole_user_6 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
testObject_ConversationRole_user_7 :: ConversationRole
testObject_ConversationRole_user_7 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))
testObject_ConversationRole_user_8 :: ConversationRole
testObject_ConversationRole_user_8 = (fromJust (toConvRole (fromJust (parseRoleName "cng1oc37jh5_tf365jsu0jmvczrn98xqtt8_k2svxp5q1g8bpk1jvslysf4q9ly_13wo_5_fdys0kbssw0suh4230sffblx3hnpg179vvtof6ynxub9851d")) (Just ((Actions (Set.fromList [ModifyConversationAccess]))))))
testObject_ConversationRole_user_9 :: ConversationRole
testObject_ConversationRole_user_9 = (fromJust (toConvRole (fromJust (parseRoleName "7o4wgkq7m_ltisk3wz1q99g34lwnkm8efgy3quy5gaouqhioa4b0nx3inxjee5f1g92silcj6p15wzp16r94uy2md4kqqnw0yb82qv9ije_h5xlyfka1hirl2wtf2l")) (Just ((Actions (Set.fromList [AddConversationMember,ModifyConversationName,ModifyConversationMessageTimer,ModifyOtherConversationMember,LeaveConversation]))))))
testObject_ConversationRole_user_10 :: ConversationRole
testObject_ConversationRole_user_10 = (fromJust (toConvRole (fromJust (parseRoleName "h924h94z3q0vhqoj0khvvzcti4r4uzlfa1phw_hil__3c7_f0f0gj1zah_yox0we2ulyzdmuq3zlso3z2_5o145u")) (Just ((Actions (Set.fromList [ModifyConversationMessageTimer,ModifyConversationReceiptMode,ModifyOtherConversationMember]))))))
testObject_ConversationRole_user_11 :: ConversationRole
testObject_ConversationRole_user_11 = (fromJust (toConvRole (fromJust (parseRoleName "p0ys5l_tag55vh17wz75cb_vd6yjhui_zkndu2nthpyj3u9l45uctoykoxdukw78sfj07ngjtplk2m9")) (Just ((Actions (Set.fromList []))))))
testObject_ConversationRole_user_12 :: ConversationRole
testObject_ConversationRole_user_12 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
testObject_ConversationRole_user_13 :: ConversationRole
testObject_ConversationRole_user_13 = (fromJust (toConvRole (fromJust (parseRoleName "pxzovab0fb0pgmm4ti5j_2suw1sfwryl5z1d09036ror782ubv0iv6kndbk2stnubjrkenc76i20a")) (Just ((Actions (Set.fromList [AddConversationMember,RemoveConversationMember,ModifyConversationName,ModifyConversationMessageTimer,ModifyConversationReceiptMode,ModifyConversationAccess,ModifyOtherConversationMember,LeaveConversation,DeleteConversation]))))))
testObject_ConversationRole_user_14 :: ConversationRole
testObject_ConversationRole_user_14 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
testObject_ConversationRole_user_15 :: ConversationRole
testObject_ConversationRole_user_15 = (fromJust (toConvRole (fromJust (parseRoleName "mzufeua9aeakpz9jso7vdiu95ygfn")) (Just ((Actions (Set.fromList [ModifyConversationName]))))))
testObject_ConversationRole_user_16 :: ConversationRole
testObject_ConversationRole_user_16 = (fromJust (toConvRole (fromJust (parseRoleName "l5dvcv8gc31h6tnd9bhrizd1xow02qq3m6t0mo0s3mqrd8yvbklqb2q049cojxbgye4cjn5z68qq0xtu0af70fmg9f0ospg9gpktd37xs7q3g")) (Just ((Actions (Set.fromList [ModifyConversationName,ModifyConversationMessageTimer,ModifyConversationReceiptMode,ModifyConversationAccess,ModifyOtherConversationMember,DeleteConversation]))))))
testObject_ConversationRole_user_17 :: ConversationRole
testObject_ConversationRole_user_17 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
testObject_ConversationRole_user_18 :: ConversationRole
testObject_ConversationRole_user_18 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))
testObject_ConversationRole_user_19 :: ConversationRole
testObject_ConversationRole_user_19 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))
testObject_ConversationRole_user_20 :: ConversationRole
testObject_ConversationRole_user_20 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))

{-# LANGUAGE OverloadedLists #-}

module Test.Wire.API.Golden.Generated.ConversationRole_user where

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
        ModifyOtherConversationMember
      ),
    Actions (Actions),
    ConversationRole,
    parseRoleName,
    toConvRole,
  )

testObject_ConversationRole_user_1 :: ConversationRole
testObject_ConversationRole_user_1 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))

testObject_ConversationRole_user_2 :: ConversationRole
testObject_ConversationRole_user_2 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))

testObject_ConversationRole_user_3 :: ConversationRole
testObject_ConversationRole_user_3 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))

testObject_ConversationRole_user_4 :: ConversationRole
testObject_ConversationRole_user_4 = (fromJust (toConvRole (fromJust (parseRoleName "32s49begziet8bw2zajkjk5flc26_pl8lnx5vs")) (Just ((Actions (Set.fromList [ModifyConversationMessageTimer, ModifyConversationAccess]))))))

testObject_ConversationRole_user_5 :: ConversationRole
testObject_ConversationRole_user_5 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))

testObject_ConversationRole_user_6 :: ConversationRole
testObject_ConversationRole_user_6 = (fromJust (toConvRole (fromJust (parseRoleName "xtbwm41ey1yulpxoqnzn2lzfc6bmdy2k1pc1wglp51t4_kaj2nxqh9pwj8umsta3tvdh7fne2gdwvnntrbchkgw31f12q9sg46r9qh1qvu8")) (Just ((Actions (Set.fromList [AddConversationMember, ModifyConversationName, ModifyConversationMessageTimer, ModifyConversationReceiptMode, ModifyConversationAccess, ModifyOtherConversationMember, DeleteConversation]))))))

testObject_ConversationRole_user_7 :: ConversationRole
testObject_ConversationRole_user_7 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))

testObject_ConversationRole_user_8 :: ConversationRole
testObject_ConversationRole_user_8 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))

testObject_ConversationRole_user_9 :: ConversationRole
testObject_ConversationRole_user_9 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))

testObject_ConversationRole_user_10 :: ConversationRole
testObject_ConversationRole_user_10 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))

testObject_ConversationRole_user_11 :: ConversationRole
testObject_ConversationRole_user_11 = (fromJust (toConvRole (fromJust (parseRoleName "4yqqhwyg4cxggtjhjb55f32pn8h9p71tbzxymmlw8hlmz3f79zoai6bw75wy0433ut7is1m2hgs17vokq_hk9udz2jua_leu_l21oqeffcoy")) (Just ((Actions (Set.fromList [AddConversationMember, ModifyConversationName, ModifyConversationReceiptMode, ModifyOtherConversationMember, LeaveConversation, DeleteConversation]))))))

testObject_ConversationRole_user_12 :: ConversationRole
testObject_ConversationRole_user_12 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))

testObject_ConversationRole_user_13 :: ConversationRole
testObject_ConversationRole_user_13 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))

testObject_ConversationRole_user_14 :: ConversationRole
testObject_ConversationRole_user_14 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))

testObject_ConversationRole_user_15 :: ConversationRole
testObject_ConversationRole_user_15 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))

testObject_ConversationRole_user_16 :: ConversationRole
testObject_ConversationRole_user_16 = (fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))

testObject_ConversationRole_user_17 :: ConversationRole
testObject_ConversationRole_user_17 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))

testObject_ConversationRole_user_18 :: ConversationRole
testObject_ConversationRole_user_18 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))

testObject_ConversationRole_user_19 :: ConversationRole
testObject_ConversationRole_user_19 = (fromJust (toConvRole (fromJust (parseRoleName "idsg79l46dbcc6qqm12fucmnni7fe4xf6x_rnushx6gojdlgu5")) (Just ((Actions (Set.fromList [AddConversationMember, ModifyConversationAccess]))))))

testObject_ConversationRole_user_20 :: ConversationRole
testObject_ConversationRole_user_20 = (fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))

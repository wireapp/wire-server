{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ConversationAccessUpdate_user where
import Wire.API.Conversation
    ( Access(LinkAccess, CodeAccess, PrivateAccess, InviteAccess),
      AccessRole(PrivateAccessRole, ActivatedAccessRole, TeamAccessRole,
                 NonActivatedAccessRole),
      ConversationAccessUpdate(..) )

testObject_ConversationAccessUpdate_user_1 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_1 = ConversationAccessUpdate {cupAccess = [LinkAccess,CodeAccess], cupAccessRole = PrivateAccessRole}
testObject_ConversationAccessUpdate_user_2 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_2 = ConversationAccessUpdate {cupAccess = [CodeAccess], cupAccessRole = TeamAccessRole}
testObject_ConversationAccessUpdate_user_3 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_3 = ConversationAccessUpdate {cupAccess = [InviteAccess,CodeAccess], cupAccessRole = PrivateAccessRole}
testObject_ConversationAccessUpdate_user_4 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_4 = ConversationAccessUpdate {cupAccess = [CodeAccess,InviteAccess,InviteAccess,LinkAccess,LinkAccess,InviteAccess], cupAccessRole = ActivatedAccessRole}
testObject_ConversationAccessUpdate_user_5 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_5 = ConversationAccessUpdate {cupAccess = [InviteAccess], cupAccessRole = TeamAccessRole}
testObject_ConversationAccessUpdate_user_6 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_6 = ConversationAccessUpdate {cupAccess = [PrivateAccess], cupAccessRole = TeamAccessRole}
testObject_ConversationAccessUpdate_user_7 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_7 = ConversationAccessUpdate {cupAccess = [PrivateAccess,CodeAccess,LinkAccess,PrivateAccess,CodeAccess], cupAccessRole = TeamAccessRole}
testObject_ConversationAccessUpdate_user_8 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_8 = ConversationAccessUpdate {cupAccess = [LinkAccess,PrivateAccess,CodeAccess,InviteAccess], cupAccessRole = ActivatedAccessRole}
testObject_ConversationAccessUpdate_user_9 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_9 = ConversationAccessUpdate {cupAccess = [PrivateAccess,LinkAccess,CodeAccess,LinkAccess,LinkAccess,LinkAccess,LinkAccess,LinkAccess], cupAccessRole = PrivateAccessRole}
testObject_ConversationAccessUpdate_user_10 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_10 = ConversationAccessUpdate {cupAccess = [InviteAccess,LinkAccess], cupAccessRole = NonActivatedAccessRole}
testObject_ConversationAccessUpdate_user_11 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_11 = ConversationAccessUpdate {cupAccess = [PrivateAccess], cupAccessRole = ActivatedAccessRole}
testObject_ConversationAccessUpdate_user_12 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_12 = ConversationAccessUpdate {cupAccess = [], cupAccessRole = PrivateAccessRole}
testObject_ConversationAccessUpdate_user_13 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_13 = ConversationAccessUpdate {cupAccess = [PrivateAccess,LinkAccess,CodeAccess,PrivateAccess,PrivateAccess,PrivateAccess,LinkAccess], cupAccessRole = PrivateAccessRole}
testObject_ConversationAccessUpdate_user_14 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_14 = ConversationAccessUpdate {cupAccess = [], cupAccessRole = TeamAccessRole}
testObject_ConversationAccessUpdate_user_15 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_15 = ConversationAccessUpdate {cupAccess = [CodeAccess,LinkAccess,LinkAccess,InviteAccess,InviteAccess,PrivateAccess,PrivateAccess,InviteAccess,PrivateAccess], cupAccessRole = PrivateAccessRole}
testObject_ConversationAccessUpdate_user_16 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_16 = ConversationAccessUpdate {cupAccess = [InviteAccess], cupAccessRole = PrivateAccessRole}
testObject_ConversationAccessUpdate_user_17 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_17 = ConversationAccessUpdate {cupAccess = [PrivateAccess,InviteAccess,LinkAccess,InviteAccess], cupAccessRole = PrivateAccessRole}
testObject_ConversationAccessUpdate_user_18 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_18 = ConversationAccessUpdate {cupAccess = [InviteAccess], cupAccessRole = TeamAccessRole}
testObject_ConversationAccessUpdate_user_19 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_19 = ConversationAccessUpdate {cupAccess = [InviteAccess,LinkAccess], cupAccessRole = NonActivatedAccessRole}
testObject_ConversationAccessUpdate_user_20 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_20 = ConversationAccessUpdate {cupAccess = [], cupAccessRole = PrivateAccessRole}

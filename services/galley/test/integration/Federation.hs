module Federation where

import Control.Lens ((^.))
import Data.Domain
import Data.Id
import Data.Qualified
import Data.UUID qualified as UUID
import Galley.API.Util
import Galley.App
import Galley.Data.Conversation.Types qualified as Types
import Galley.Options
import Galley.Types.Conversations.Members (LocalMember (..), RemoteMember (..), defMemberStatus)
import Imports
import Test.Tasty.HUnit
import TestSetup
import Wire.API.Conversation
import Wire.API.Conversation.Protocol (Protocol (..))
import Wire.API.Conversation.Role (roleNameWireMember)

isConvMemberLTests :: TestM ()
isConvMemberLTests = do
  s <- ask
  let opts = s ^. tsGConf
      localDomain = opts ^. settings . federationDomain
      remoteDomain = Domain "far-away.example.com"
      convId = Id $ fromJust $ UUID.fromString "8cc34301-6949-46c5-bb93-00a72268e2f5"
      convLocalMembers = [LocalMember userId defMemberStatus Nothing roleNameWireMember]
      convRemoteMembers = [RemoteMember rUserId roleNameWireMember]
      lconv =
        toLocalUnsafe localDomain $
          Types.Conversation
            convId
            convLocalMembers
            convRemoteMembers
            False
            (defConversationMetadata (Just userId))
            ProtocolProteus
      lUserId :: Local UserId
      lUserId = toLocalUnsafe localDomain $ Id $ fromJust $ UUID.fromString "217352c0-8b2b-4653-ac76-a88d19490dad" -- A random V4 UUID
      userId = qUnqualified $ tUntagged lUserId
      rUserId :: Remote UserId
      rUserId = toRemoteUnsafe remoteDomain $ Id $ fromJust $ UUID.fromString "d87745f5-dfe7-4ff0-8772-b9c22118b372"
  liftIO $ assertBool "UserId" $ isConvMemberL lconv userId
  liftIO $ assertBool "Local UserId" $ isConvMemberL lconv lUserId
  liftIO $ assertBool "Remote UserId" $ isConvMemberL lconv rUserId
  liftIO $ assertBool "Qualified UserId (local)" $ isConvMemberL lconv $ tUntagged lUserId
  liftIO $ assertBool "Qualified UserId (remote)" $ isConvMemberL lconv $ tUntagged rUserId

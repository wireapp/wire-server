{-# LANGUAGE RecordWildCards #-}

module Federation where

import Cassandra qualified as C
import Control.Lens ((^.))
import Control.Monad.Catch
import Data.ByteString qualified as LBS
import Data.Domain
import Data.Id
import Data.Qualified
import Data.Set qualified as Set
import Data.UUID qualified as UUID
import Galley.API.Util
import Galley.App
import Galley.Data.Conversation.Types qualified as Types
import Galley.Options
import Galley.Types.Conversations.Members (LocalMember (..), RemoteMember (..), defMemberStatus)
import Imports
import Test.Tasty.HUnit
import TestSetup
import UnliftIO.Retry
import Wire.API.Conversation
import Wire.API.Conversation qualified as Public
import Wire.API.Conversation.Protocol (Protocol (..))
import Wire.API.Conversation.Role (roleNameWireMember)
import Wire.API.Routes.FederationDomainConfig
import Wire.API.Routes.MultiTablePaging
import Wire.API.Routes.MultiTablePaging qualified as Public

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 100000

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
            (defConversationMetadata userId)
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

fromFedList :: FederationDomainConfigs -> Set Domain
fromFedList = Set.fromList . fmap domain . remotes

constHandlers :: (MonadIO m) => [RetryStatus -> Handler m Bool]
constHandlers = [const $ Handler $ (\(_ :: SomeException) -> pure True)]

pageToConvIdPage :: Public.LocalOrRemoteTable -> C.PageWithState (Qualified ConvId) -> Public.ConvIdsPage
pageToConvIdPage table page@C.PageWithState {..} =
  Public.MultiTablePage
    { mtpResults = pwsResults,
      mtpHasMore = C.pwsHasMore page,
      mtpPagingState = Public.ConversationPagingState table (LBS.toStrict . C.unPagingState <$> pwsState)
    }

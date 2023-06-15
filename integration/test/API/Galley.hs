module API.Galley where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as BS
import Testlib.Prelude

data CreateConv = CreateConv
  { qualifiedUsers :: [Value],
    name :: Maybe String,
    access :: Maybe [String],
    accessRole :: Maybe [String],
    team :: Maybe String,
    messageTimer :: Maybe Int,
    receiptMode :: Maybe Int,
    newUsersRole :: String,
    protocol :: String
  }

defProteus :: CreateConv
defProteus =
  CreateConv
    { qualifiedUsers = [],
      name = Nothing,
      access = Nothing,
      accessRole = Nothing,
      team = Nothing,
      messageTimer = Nothing,
      receiptMode = Nothing,
      newUsersRole = "wire_admin",
      protocol = "proteus"
    }

defMLS :: CreateConv
defMLS = defProteus {protocol = "mls"}

instance MakesValue CreateConv where
  make cc = do
    quids <- for (cc.qualifiedUsers) objQidObject
    pure $
      Aeson.object $
        ( [ "qualified_users" .= quids,
            "conversation_role" .= cc.newUsersRole,
            "protocol" .= cc.protocol
          ]
            <> catMaybes
              [ "name" .=? cc.name,
                "access" .=? cc.access,
                "access_role_v2" .=? cc.access,
                "team" .=? (cc.team <&> \tid -> Aeson.object ["teamid" .= tid, "managed" .= False]),
                "message_timer" .=? cc.messageTimer,
                "receipt_mode" .=? cc.receiptMode
              ]
        )

postConversation ::
  ( HasCallStack,
    MakesValue user
  ) =>
  user ->
  CreateConv ->
  App Response
postConversation user cc = do
  req <- baseRequest user Galley Versioned "/conversations"
  ccv <- make cc
  submit "POST" $ req & addJSON ccv

putConversationProtocol ::
  ( HasCallStack,
    MakesValue user,
    MakesValue qcnv,
    MakesValue protocol
  ) =>
  user ->
  qcnv ->
  protocol ->
  App Response
putConversationProtocol user qcnv protocol = do
  (domain, cnv) <- objQid qcnv
  p <- asString protocol
  req <- baseRequest user Galley Versioned (joinHttpPath ["conversations", domain, cnv, "protocol"])
  submit "PUT" (req & addJSONObject ["protocol" .= p])

getConversation ::
  ( HasCallStack,
    MakesValue user,
    MakesValue qcnv
  ) =>
  user ->
  qcnv ->
  App Response
getConversation user qcnv = do
  (domain, cnv) <- objQid qcnv
  req <- baseRequest user Galley Versioned (joinHttpPath ["conversations", domain, cnv])
  submit "GET" req

getSubConversation ::
  ( HasCallStack,
    MakesValue user,
    MakesValue conv
  ) =>
  user ->
  conv ->
  String ->
  App Response
getSubConversation user conv sub = do
  (cnvDomain, cnvId) <- objQid conv
  req <-
    baseRequest user Galley Versioned $
      joinHttpPath
        [ "conversations",
          cnvDomain,
          cnvId,
          "subconversations",
          sub
        ]
  submit "GET" req

getSelfConversation :: (HasCallStack, MakesValue user) => user -> App Response
getSelfConversation user = do
  req <- baseRequest user Galley Versioned "/conversations/mls-self"
  submit "GET" $ req

data ListConversationIds = ListConversationIds {pagingState :: Maybe String, size :: Maybe Int}

instance Default ListConversationIds where
  def = ListConversationIds Nothing Nothing

listConversationIds :: MakesValue user => user -> ListConversationIds -> App Response
listConversationIds user args = do
  req <- baseRequest user Galley Versioned "/conversations/list-ids"
  submit "POST" $
    req
      & addJSONObject
        ( ["paging_state" .= s | s <- toList args.pagingState]
            <> ["size" .= s | s <- toList args.size]
        )

listConversations :: MakesValue user => user -> [Value] -> App Response
listConversations user cnvs = do
  req <- baseRequest user Galley Versioned "/conversations/list"
  submit "POST" $
    req
      & addJSONObject ["qualified_ids" .= cnvs]

postMLSMessage :: HasCallStack => ClientIdentity -> ByteString -> App Response
postMLSMessage cid msg = do
  req <- baseRequest cid Galley Versioned "/mls/messages"
  submit "POST" (addMLS msg req)

postMLSCommitBundle :: HasCallStack => ClientIdentity -> ByteString -> App Response
postMLSCommitBundle cid msg = do
  req <- baseRequest cid Galley Versioned "/mls/commit-bundles"
  submit "POST" (addMLS msg req)

getGroupInfo ::
  (HasCallStack, MakesValue user, MakesValue conv) =>
  user ->
  conv ->
  App Response
getGroupInfo user conv = do
  (qcnv, mSub) <- objSubConv conv
  (convDomain, convId) <- objQid qcnv
  let path = joinHttpPath $ case mSub of
        Nothing -> ["conversations", convDomain, convId, "groupinfo"]
        Just sub -> ["conversations", convDomain, convId, "subconversations", sub, "groupinfo"]
  req <- baseRequest user Galley Versioned path
  submit "GET" req

removeConversationMember ::
  (HasCallStack, MakesValue user, MakesValue conv) =>
  user ->
  conv ->
  App Response
removeConversationMember user conv = do
  (convDomain, convId) <- objQid conv
  (userDomain, userId) <- objQid user
  req <- baseRequest user Galley Versioned (joinHttpPath ["conversations", convDomain, convId, "members", userDomain, userId])
  submit "DELETE" req

updateConversationMember ::
  (HasCallStack, MakesValue user, MakesValue conv, MakesValue target) =>
  user ->
  conv ->
  target ->
  String ->
  App Response
updateConversationMember user conv target role = do
  (convDomain, convId) <- objQid conv
  (targetDomain, targetId) <- objQid target
  req <- baseRequest user Galley Versioned (joinHttpPath ["conversations", convDomain, convId, "members", targetDomain, targetId])
  submit "PUT" (req & addJSONObject ["conversation_role" .= role])

deleteTeamConv ::
  (HasCallStack, MakesValue team, MakesValue conv, MakesValue user) =>
  team ->
  conv ->
  user ->
  App Response
deleteTeamConv team conv user = do
  teamId <- objId team
  convId <- objId conv
  req <- baseRequest user Galley Versioned (joinHttpPath ["teams", teamId, "conversations", convId])
  submit "DELETE" req

getMLSOne2OneConversation ::
  (HasCallStack, MakesValue self, MakesValue other) =>
  self ->
  other ->
  App Response
getMLSOne2OneConversation self other = do
  (domain, uid) <- objQid other
  req <-
    baseRequest self Galley Versioned $
      joinHttpPath ["conversations", "one2one", domain, uid]
  submit "GET" req

getGroupClients ::
  (HasCallStack, MakesValue user) =>
  user ->
  String ->
  App Response
getGroupClients user groupId = do
  req <-
    baseRequest
      user
      Galley
      Unversioned
      (joinHttpPath ["i", "group", BS.unpack . B64U.encodeUnpadded . B64.decodeLenient $ BS.pack groupId])
  submit "GET" req

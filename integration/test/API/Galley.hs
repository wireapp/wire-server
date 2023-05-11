module API.Galley where

import qualified Data.Aeson as Aeson
import Data.String.Conversions (cs)
import qualified Data.Vector as Vector
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
    MakesValue user,
    MakesValue client
  ) =>
  user ->
  Maybe client ->
  CreateConv ->
  App Response
postConversation user mclient cc = do
  uid <- objId user
  domain <- objDomain user
  mcid <- for mclient objId
  req <- baseRequest domain Galley Versioned "/conversations"
  ccv <- make cc
  submit "POST" $
    req
      & zUser uid
      & maybe id zClient mcid
      & zConnection "conn"
      & addJSON ccv

putConversationProtocol ::
  ( HasCallStack,
    MakesValue user,
    MakesValue qcnv,
    MakesValue conn,
    MakesValue protocol
  ) =>
  user ->
  qcnv ->
  Maybe conn ->
  protocol ->
  App Response
putConversationProtocol user qcnv mconn protocol = do
  mconn' <- for mconn asString
  (domain, cnv) <- objQid qcnv
  p <- asString protocol
  uid <- objId user
  req <- baseRequest user Galley Versioned (joinHttpPath ["conversations", domain, cnv, "protocol"])
  submit
    "PUT"
    ( req
        & zUser uid
        & zConnection (fromMaybe "conn" mconn')
        & addJSONObject ["protocol" .= p]
    )

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
  uid <- objId user
  req <- baseRequest user Galley Versioned (joinHttpPath ["conversations", domain, cnv])
  submit
    "GET"
    ( req
        & zUser uid
    )

getFederationStatus ::
  ( HasCallStack,
    MakesValue user
  ) =>
  user ->
  [String] ->
  App Response
getFederationStatus user domains =
  let domainList = Aeson.Array (Vector.fromList $ Aeson.String . cs <$> domains)
   in do
        uid <- objId user
        req <- baseRequest user Galley Versioned "/federation-status"
        submit
          "GET"
          ( req
              & zUser uid
              & addJSONObject ["domains" .= domainList]
          )

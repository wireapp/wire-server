module Wire.UserStore.Cassandra where

import Cassandra
import Data.Id
import Database.CQL.Protocol
import Imports
import Polysemy
import Polysemy.Embed
import Wire.StoredUser
import Wire.UserStore

interpretUserStoreCassandra :: Member (Embed IO) r => ClientState -> InterpreterFor UserStore r
interpretUserStoreCassandra casClient =
  interpret $
    runEmbedded (runClient casClient) . \case
      GetUser uid -> getUserImpl uid

getUserImpl :: Member (Embed Client) r => UserId -> Sem r (Maybe StoredUser)
getUserImpl uid = embed $ do
  mUserTuple <- retry x1 $ query1 selectUser (params LocalQuorum (Identity uid))
  pure $ asRecord <$> mUserTuple

selectUser :: PrepQuery R (Identity UserId) (TupleType StoredUser)
selectUser =
  "SELECT id, name, picture, email, phone, sso_id, accent_id, assets, \
  \activated, status, expires, language, country, provider, service, \
  \handle, team, managed_by, supported_protocols \
  \FROM user where id = ?"

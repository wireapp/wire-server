module Wire.UserStore.Cassandra where

import Cassandra
import Data.Id
import Database.CQL.Protocol
import Imports
import Polysemy
import Polysemy.Embed
import Wire.API.User
import Wire.StoredUser
import Wire.UserStore

interpretUserStoreCassandra :: Member (Embed IO) r => ClientState -> InterpreterFor UserStore r
interpretUserStoreCassandra casClient =
  interpret $
    runEmbedded (runClient casClient) . \case
      GetUser uid -> getUserImpl uid
      UpdateUser uid update -> updateUserImpl uid update

getUserImpl :: Member (Embed Client) r => UserId -> Sem r (Maybe StoredUser)
getUserImpl uid = embed $ do
  mUserTuple <- retry x1 $ query1 selectUser (params LocalQuorum (Identity uid))
  pure $ asRecord <$> mUserTuple

updateUserImpl :: Member (Embed Client) r => UserId -> UserUpdate -> Sem r ()
updateUserImpl uid update = embed . retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  for_ update.uupName $ \n -> addPrepQuery userDisplayNameUpdate (n, uid)
  for_ update.uupPict $ \p -> addPrepQuery userPictUpdate (p, uid)
  for_ update.uupAssets $ \a -> addPrepQuery userAssetsUpdate (a, uid)
  for_ update.uupAccentId $ \c -> addPrepQuery userAccentIdUpdate (c, uid)

selectUser :: PrepQuery R (Identity UserId) (TupleType StoredUser)
selectUser =
  "SELECT id, name, picture, email, phone, sso_id, accent_id, assets, \
  \activated, status, expires, language, country, provider, service, \
  \handle, team, managed_by, supported_protocols \
  \FROM user where id = ?"

userDisplayNameUpdate :: PrepQuery W (Name, UserId) ()
userDisplayNameUpdate = "UPDATE user SET name = ? WHERE id = ?"

userPictUpdate :: PrepQuery W (Pict, UserId) ()
userPictUpdate = "UPDATE user SET picture = ? WHERE id = ?"

userAssetsUpdate :: PrepQuery W ([Asset], UserId) ()
userAssetsUpdate = "UPDATE user SET assets = ? WHERE id = ?"

userAccentIdUpdate :: PrepQuery W (ColourId, UserId) ()
userAccentIdUpdate = "UPDATE user SET accent_id = ? WHERE id = ?"

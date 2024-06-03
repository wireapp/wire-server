module Wire.UserKeyStore.Cassandra (interpretUserKeyStoreCassandra) where

import Cassandra
import Data.Id
import Imports
import Polysemy
import Polysemy.Embed
import Wire.UserKeyStore
import Wire.UserStore

interpretUserKeyStoreCassandra :: (Member (Embed IO) r, Member UserStore r) => ClientState -> InterpreterFor UserKeyStore r
interpretUserKeyStoreCassandra casClient =
  interpret $
    runEmbedded (runClient casClient) . \case
      LookupKey key -> embed $ lookupKeyImpl key
      InsertKey uid key -> embed $ insertKeyImpl uid key
      DeleteKey key -> embed $ deleteKeyImpl key
      DeleteKeyForUser uid key -> embed $ deleteKeyForUserImpl uid key
      ClaimKey key uid -> claimKeyImpl casClient key uid
      KeyAvailable key uid -> keyAvailableImpl casClient key uid

-- | Claim a 'UserKey' for a user.
claimKeyImpl ::
  (Member (Embed IO) r, Member UserStore r) =>
  ClientState ->
  -- | The key to claim.
  EmailKey ->
  -- | The user claiming the key.
  UserId ->
  Sem r Bool
claimKeyImpl client k u = do
  free <- keyAvailableImpl client k (Just u)
  when free (runClient client $ insertKeyImpl u k)
  pure free

-- | Check whether a 'UserKey' is available.
-- A key is available if it is not already activated for another user or
-- if the other user and the user looking to claim the key are the same.
keyAvailableImpl ::
  (Member (Embed IO) r, Member UserStore r) =>
  ClientState ->
  -- | The key to check.
  EmailKey ->
  -- | The user looking to claim the key, if any.
  Maybe UserId ->
  Sem r Bool
keyAvailableImpl client k u = do
  o <- runClient client $ lookupKeyImpl k
  case (o, u) of
    (Nothing, _) -> pure True
    (Just x, Just y) | x == y -> pure True
    (Just x, _) -> not <$> isActivated x

lookupKeyImpl :: (MonadClient m) => EmailKey -> m (Maybe UserId)
lookupKeyImpl k =
  fmap runIdentity
    <$> retry x1 (query1 keySelect (params LocalQuorum (Identity $ emailKeyUniq k)))

insertKeyImpl :: UserId -> EmailKey -> Client ()
insertKeyImpl u k = do
  retry x5 $ write keyInsert (params LocalQuorum (emailKeyUniq k, u))

deleteKeyImpl :: (MonadClient m) => EmailKey -> m ()
deleteKeyImpl k = do
  retry x5 $ write keyDelete (params LocalQuorum (Identity $ emailKeyUniq k))

-- | Delete `UserKey` for `UserId`
--
-- This function ensures that keys of other users aren't accidentally deleted.
-- E.g. the email address or phone number of a partially deleted user could
-- already belong to a new user. To not interrupt deletion flows (that may be
-- executed several times due to cassandra not supporting transactions)
-- `deleteKeyImplForUser` does not fail for missing keys or keys that belong to
-- another user: It always returns `()` as result.
deleteKeyForUserImpl :: (MonadClient m) => UserId -> EmailKey -> m ()
deleteKeyForUserImpl uid k = do
  mbKeyUid <- lookupKeyImpl k
  case mbKeyUid of
    Just keyUid | keyUid == uid -> deleteKeyImpl k
    _ -> pure ()

--------------------------------------------------------------------------------
-- Queries

keyInsert :: PrepQuery W (Text, UserId) ()
keyInsert = "INSERT INTO user_keys (key, user) VALUES (?, ?)"

keySelect :: PrepQuery R (Identity Text) (Identity UserId)
keySelect = "SELECT user FROM user_keys WHERE key = ?"

keyDelete :: PrepQuery W (Identity Text) ()
keyDelete = "DELETE FROM user_keys WHERE key = ?"

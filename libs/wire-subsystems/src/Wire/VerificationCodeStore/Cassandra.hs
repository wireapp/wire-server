module Wire.VerificationCodeStore.Cassandra where

import Cassandra hiding (Value)
import Data.RetryAfter
import Data.UUID
import Imports
import Polysemy
import Polysemy.Embed
import Wire.API.User.Identity
import Wire.VerificationCode
import Wire.VerificationCodeStore

interpretVerificationCodeStoreCassandra :: (Member (Embed IO) r) => ClientState -> InterpreterFor VerificationCodeStore r
interpretVerificationCodeStoreCassandra casClient =
  interpret $
    runEmbedded (runClient casClient) . \case
      InsertCode code -> embed $ insertCodeImpl code
      LookupCode key scope -> embed $ lookupCodeImpl key scope
      DeleteCode key scope -> embed $ deleteCodeImpl key scope
      InsertThrottle key scope ttl -> embed $ insertThrottleImpl key scope ttl
      LookupThrottle key scope -> embed $ lookupThrottleImpl key scope

insertCodeImpl :: (MonadClient m) => Code -> m ()
insertCodeImpl c = do
  let k = codeKey c
  let s = codeScope c
  let v = codeValue c
  let r = fromIntegral (codeRetries c)
  let a = codeAccount c
  let e = codeFor c
  let t = round (codeTTL c)
  retry x5 (write cql (params LocalQuorum (k, s, v, r, e, a, t)))
  where
    cql :: PrepQuery W (Key, Scope, Value, Retries, Email, Maybe UUID, Int32) ()
    cql =
      "INSERT INTO vcodes (key, scope, value, retries, email, account) \
      \VALUES (?, ?, ?, ?, ?, ?) USING TTL ?"

-- | Lookup a pending code.
lookupCodeImpl :: (MonadClient m) => Key -> Scope -> m (Maybe Code)
lookupCodeImpl k s = toCode <$$> retry x1 (query1 cql (params LocalQuorum (k, s)))
  where
    cql :: PrepQuery R (Key, Scope) (Value, Int32, Retries, Email, Maybe UUID)
    cql =
      "SELECT value, ttl(value), retries, email, account \
      \FROM vcodes WHERE key = ? AND scope = ?"

    toCode :: (Value, Int32, Retries, Email, Maybe UUID) -> Code
    toCode (val, ttl, retries, email, account) =
      Code
        { codeKey = k,
          codeScope = s,
          codeValue = val,
          codeTTL = Timeout (fromIntegral ttl),
          codeRetries = retries,
          codeFor = email,
          codeAccount = account
        }

-- | Delete a code associated with the given key and scope.
deleteCodeImpl :: (MonadClient m) => Key -> Scope -> m ()
deleteCodeImpl k s = retry x5 $ write cql (params LocalQuorum (k, s))
  where
    cql :: PrepQuery W (Key, Scope) ()
    cql = "DELETE FROM vcodes WHERE key = ? AND scope = ?"

lookupThrottleImpl :: (MonadClient m) => Key -> Scope -> m (Maybe RetryAfter)
lookupThrottleImpl k s = do
  fmap (RetryAfter . fromIntegral . runIdentity) <$> retry x1 (query1 cql (params LocalQuorum (k, s)))
  where
    cql :: PrepQuery R (Key, Scope) (Identity Int32)
    cql =
      "SELECT ttl(initial_delay) \
      \FROM vcodes_throttle WHERE key = ? AND scope = ?"

insertThrottleImpl :: (MonadClient m) => Key -> Scope -> Int -> m ()
insertThrottleImpl k s t = do
  retry x5 (write cql (params LocalQuorum (k, s, fromIntegral t, fromIntegral t)))
  where
    cql :: PrepQuery W (Key, Scope, Int32, Int32) ()
    cql =
      "INSERT INTO vcodes_throttle (key, scope, initial_delay) \
      \VALUES (?, ?, ?) USING TTL ?"

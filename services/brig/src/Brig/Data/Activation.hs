{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Activation of 'Email' addresses and 'Phone' numbers.
module Brig.Data.Activation
    ( Activation      (..)
    , ActivationKey   (..)
    , ActivationCode  (..)
    , ActivationEvent (..)
    , ActivationError (..)
    , newActivation
    , mkActivationKey
    , lookupActivationCode
    , activateKey
    , verifyCode
    ) where

import Brig.App (AppIO)
import Brig.Options
import Brig.Data.User
import Brig.Data.UserKey
import Brig.Data.PasswordReset
import Brig.Types
import Brig.Types.Intra
import Cassandra
import Control.Applicative ((<|>))
import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Foldable (for_)
import Data.Functor.Identity
import Data.Id
import Data.Int (Int32)
import Data.Text (Text, pack)
import OpenSSL.BN (randIntegerZeroToNMinusOne)
import OpenSSL.EVP.Digest (getDigestByName, digestBS)
import Text.Printf (printf)

import qualified Data.Text.Ascii    as Ascii
import qualified Data.Text.Lazy     as LT
import qualified Data.Text.Encoding as T

--  | The information associated with the pending activation of a 'UserKey'.
data Activation = Activation
    { activationKey  :: !ActivationKey
        -- ^ An opaque key for the original 'UserKey' pending activation.
    , activationCode :: !ActivationCode
        -- ^ The confidential activation code.
    } deriving (Eq)

data ActivationError
    = UserKeyExists          !LT.Text
    | InvalidActivationCode  !LT.Text
    | InvalidActivationEmail !Email
    | InvalidActivationPhone !Phone

data ActivationEvent
    = AccountActivated !UserAccount
    | EmailActivated   !UserId !Email
    | PhoneActivated   !UserId !Phone

-- | Max. number of activation attempts per 'ActivationKey'.
maxAttempts :: Int32
maxAttempts = 3

activateKey :: ActivationKey
            -> ActivationCode
            -> Maybe UserId
            -> ExceptT ActivationError AppIO (Maybe ActivationEvent)
activateKey k c u = verifyCode k c >>= pickUser >>= activate
  where
    pickUser (uk, u') = maybe (throwE invalidUser) (return . (uk,)) (u <|> u')

    activate (key, uid) = do
        a <- lift (lookupAccount uid) >>= maybe (throwE invalidUser) return
        unless (accountStatus a == Active) $
            throwE invalidCode
        case userIdentity (accountUser a) of
            Nothing -> do
                claim key uid
                let ident = foldKey EmailIdentity PhoneIdentity key
                lift $ activateUser uid ident
                let a' = a { accountUser = (accountUser a) { userIdentity = Just ident } }
                return . Just $ AccountActivated a'
            Just _ ->
                let oldKey = foldKey (\(_ :: Email) -> fmap userEmailKey . userEmail)
                                     (\(_ :: Phone) -> fmap userPhoneKey . userPhone)
                                     key
                           $ accountUser a
                in if oldKey == Just key then return Nothing else do
                    -- ensure no password reset codes remain on activation of new email
                    mkPasswordResetKey uid >>= lift . deletePasswordResetCode
                    claim key uid
                    lift $ foldKey (updateEmail uid) (updatePhone uid) key
                    for_ oldKey $ lift . deleteKey
                    return . Just $ foldKey (EmailActivated uid) (PhoneActivated uid) key

    claim key uid = do
        ok <- lift $ claimKey key uid
        unless ok $
            throwE . UserKeyExists . LT.fromStrict $ foldKey fromEmail fromPhone key

-- | Create a new pending activation for a given 'UserKey'.
newActivation :: UserKey
              -> Timeout           -- ^ The timeout for the activation code.
              -> Maybe UserId      -- ^ The user with whom to associate the activation code.
              -> AppIO Activation
newActivation uk timeout u = do
    (typ, key, code) <- liftIO $ foldKey
        (\e -> ("email", fromEmail e,) <$> genCode)
        (\p -> ("phone", fromPhone p,) <$> genCode)
        uk
    insert typ key code
  where
    insert t k c = do
        key <- liftIO $ mkActivationKey uk
        retry x5 . write keyInsert $ params Quorum (key, t, k, c, u, maxAttempts, round timeout)
        return $ Activation key c

    genCode = ActivationCode . Ascii.unsafeFromText . pack . printf "%06d"
           <$> randIntegerZeroToNMinusOne 1000000

-- | Lookup an activation code and it's associated owner (if any) for a 'UserKey'.
lookupActivationCode :: UserKey -> AppIO (Maybe (Maybe UserId, ActivationCode))
lookupActivationCode k = liftIO (mkActivationKey k) >>=
    retry x1 . query1 codeSelect . params Quorum . Identity

-- | Verify an activation code.
verifyCode :: ActivationKey
           -> ActivationCode
           -> ExceptT ActivationError AppIO (UserKey, Maybe UserId)
verifyCode key code = do
    s <- lift . retry x1 . query1 keySelect $ params Quorum (Identity key)
    case s of
        Just (ttl, Ascii t, k, c, u, r) ->
            if | c == code -> mkScope t k u
               | r >= 1    -> countdown (key, t, k, c, u, r-1, ttl) >> throwE invalidCode
               | otherwise -> revoke >> throwE invalidCode
        Nothing -> throwE invalidCode
  where
    mkScope "email" k u = case parseEmail k of
        Just  e -> return (userEmailKey e, u)
        Nothing -> throwE invalidCode
    mkScope "phone" k u = case parsePhone k of
        Just  p -> return (userPhoneKey p, u)
        Nothing -> throwE invalidCode
    mkScope _       _ _ = throwE invalidCode

    countdown = lift . retry x5 . write keyInsert . params Quorum
    revoke    = lift $ deleteActivationPair key

mkActivationKey :: UserKey -> IO ActivationKey
mkActivationKey k = do
    d  <- liftIO $ getDigestByName "SHA256"
    d' <- maybe (fail "SHA256 not found") return d
    let bs = digestBS d' (T.encodeUtf8 $ keyText k)
    return . ActivationKey $ Ascii.encodeBase64Url bs

deleteActivationPair :: ActivationKey -> AppIO ()
deleteActivationPair = write keyDelete . params Quorum . Identity

invalidUser :: ActivationError
invalidUser = InvalidActivationCode "User does not exist."

invalidCode :: ActivationError
invalidCode = InvalidActivationCode "Invalid activation code"

keyInsert :: PrepQuery W (ActivationKey, Text, Text, ActivationCode, Maybe UserId, Int32, Int32) ()
keyInsert = "INSERT INTO activation_keys \
            \(key, key_type, key_text, code, user, retries) VALUES \
            \(?  , ?       , ?       , ?   , ?   , ?      ) USING TTL ?"

keySelect :: PrepQuery R (Identity ActivationKey) (Int32, Ascii, Text, ActivationCode, Maybe UserId, Int32)
keySelect = "SELECT ttl(code) as ttl, key_type, key_text, code, user, retries FROM activation_keys WHERE key = ?"

codeSelect :: PrepQuery R (Identity ActivationKey) (Maybe UserId, ActivationCode)
codeSelect = "SELECT user, code FROM activation_keys WHERE key = ?"

keyDelete :: PrepQuery W (Identity ActivationKey) ()
keyDelete = "DELETE FROM activation_keys WHERE key = ?"

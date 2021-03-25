module Work where

import Brig.Data.UserKey (userEmailKey)
import Imports
import Spar.Data (lookupScimExternalId)
import Wire.API.User

foo :: Int
foo = 3

data Env = Env

-- changeExtId :: ExternalId -> ExternalId -> IO ()
-- changeExtId =

lookupExternalId :: Env -> TeamId -> Text -> IO (Maybe UserId)
lookupExternalId tid extid = wrapMonadClient $ Data.lookupScimExternalId (ExternalId tid extid)

lookupEmail :: Email -> Spar (Maybe UserId)
lookupEmail eml = userId . accountUser <$$> Brig.getBrigUserByEmail eml

-- listAccountsByIdentityH

-- getBrigUserByEmail :: (HasCallStack, MonadSparToBrig m) => Email -> m (Maybe UserAccount)
-- getBrigUserByEmail email = do
--   resp :: ResponseLBS <-
--     call $
--       method GET
--         . path "/i/users"
--         . queryItem "email" (toByteString' email)
--         . queryItem "includePendingInvitations" "true"
--   case statusCode resp of
--     200 -> do
--       macc <- listToMaybe <$> parseResponse @[UserAccount] "brig" resp
--       case userEmail . accountUser =<< macc of
--         Just email' | email' == email -> pure macc
--         _ -> pure Nothing
--     404 -> pure Nothing
--     _ -> rethrow "brig" resp

-- -- Should behave similar to Brig.API.User.lookupAccountsByIdentity
-- lookupAccountsByIdentity :: Either Email Phone -> Bool -> AppIO [UserAccount]
-- lookupAccountsByIdentity emailOrPhone includePendingInvitations = do
--   let uk = either userEmailKey userPhoneKey emailOrPhone
--   activeUid <- Data.lookupKey uk
--   uidFromKey <- (>>= fst) <$> Data.lookupActivationCode uk
--   result <- Data.lookupAccounts (nub $ catMaybes [activeUid, uidFromKey])
--   if includePendingInvitations
--     then pure result
--     else pure $ filter ((/= PendingInvitation) . accountStatus) result

-- -- Should behave similar to Brig.Data.UserKey.lookupKey
-- lookupKey :: UserKey -> AppIO (Maybe UserId)
-- lookupKey k =
--   fmap runIdentity
--     <$> retry x1 (query1 keySelect (params Quorum (Identity $ keyText k)))

-- Brig.Data.User. lookupAccounts
lookupAccounts :: [UserId] -> AppIO [UserAccount]
lookupAccounts usrs = do
  loc <- setDefaultLocale <$> view settings
  domain <- viewFederationDomain
  fmap (toUserAccount domain loc) <$> retry x1 (query accountsSelect (params Quorum (Identity usrs)))

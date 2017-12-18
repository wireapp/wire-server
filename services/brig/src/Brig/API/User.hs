{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- TODO: Move to Brig.User.Account
module Brig.API.User
    ( -- * User Accounts / Profiles
      createUser
    , Brig.API.User.updateUser
    , changeLocale
    , changeEmail
    , changePhone
    , changeHandle
    , lookupHandle
    , changeAccountStatus
    , Data.lookupAccounts
    , Data.lookupAccount
    , Data.lookupStatus
    , lookupAccountsByIdentity
    , lookupSelfProfile
    , lookupProfile
    , lookupProfiles
    , Data.lookupName
    , Data.lookupLocale
    , Data.lookupUser
    , removeEmail
    , removePhone
    , revokeIdentity
    , Brig.API.User.deleteUser
    , verifyDeleteUser
    , deleteAccount
    , checkHandles
    , isBlacklistedHandle
    , Data.reauthenticate

      -- * Activation
    , sendActivationCode
    , preverify
    , activate
    , Brig.API.User.lookupActivationCode
    , Data.isActivated

      -- * Password Management
    , changePassword
    , beginPasswordReset
    , completePasswordReset
    , lookupPasswordResetCode

      -- * Blacklisting
    , isBlacklisted
    , blacklistDelete
    , blacklistInsert

      -- * Utilities
    , fetchUserIdentity
    ) where

import Brig.App
import Brig.API.Types
import Brig.Data.Activation (ActivationEvent (..))
import Brig.Data.Invitation (InvitationInfo (..))
import Brig.Data.User hiding (updateSearchableStatus)
import Brig.Data.UserKey
import Brig.Options
import Brig.Password
import Brig.Types
import Brig.Types.Code (Timeout (..))
import Brig.Types.Intra
import Brig.User.Auth.Cookie (revokeAllCookies)
import Brig.User.Email
import Brig.User.Event
import Brig.User.Handle
import Brig.User.Handle.Blacklist
import Brig.User.Phone
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Concurrent.Async (mapConcurrently, mapConcurrently_)
import Control.Error
import Control.Lens (view, (^.))
import Control.Monad (mfilter, when, unless, void, join)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString.Conversion
import Data.Foldable
import Data.Id
import Data.List (nub)
import Data.List1 (List1)
import Data.Misc (PlainTextPassword (..))
import Data.Traversable (for)
import Network.Wai.Utilities
import System.Logger.Message

import qualified Brig.Blacklist             as Blacklist
import qualified Brig.Code                  as Code
import qualified Brig.Data.Activation       as Data
import qualified Brig.Data.Client           as Data
import qualified Brig.Data.Connection       as Data
import qualified Brig.Data.Invitation       as Data
import qualified Brig.Data.PasswordReset    as Data
import qualified Brig.Data.Properties       as Data
import qualified Brig.Data.User             as Data
import qualified Brig.Data.UserKey          as Data
import qualified Brig.IO.Intra              as Intra
import qualified Brig.Types.Team.Invitation as Team
import qualified Brig.Team.DB               as Team
import qualified Data.Map.Strict            as Map
import qualified Galley.Types.Teams         as Team
import qualified Galley.Types.Teams.Intra   as Team
import qualified System.Logger.Class        as Log

-------------------------------------------------------------------------------
-- Create User

createUser :: NewUser -> ExceptT CreateUserError AppIO CreateUserResult
createUser new@NewUser{..} = do
    -- Validate e-mail
    email <- for (newUserEmail new) $ \e ->
        maybe (throwE (InvalidEmail e))
              return
              (validateEmail e)

    -- Validate phone
    phone <- for (newUserPhone new) $ \p ->
        maybe (throwE (InvalidPhone p))
              return
              =<< lift (validatePhone p)

    let ident = newIdentity email phone
    let emKey = userEmailKey <$> email
    let phKey = userPhoneKey <$> phone

    -- Verify uniqueness and check the blacklist
    for_ (catMaybes [emKey, phKey]) $ \uk -> do
        checkKey Nothing uk
        blacklisted <- lift $ Blacklist.exists uk
        when blacklisted $
            throwE (BlacklistedUserKey uk)

    -- Look for an invitation, if a code is given
    invitation <- maybe (return Nothing) (findInvitation emKey phKey) newUserInvitationCode
    (newTeam, teamInvitation) <- handleTeam (nuTeam <$> newUserTeam) emKey

    -- team members are by default not searchable
    let searchable = SearchableStatus $ case (newTeam, teamInvitation) of
            (Nothing, Nothing) -> True
            _                  -> False

    -- Create account
    (account, pw) <- lift $ newAccount new { newUserIdentity = ident } (Team.inInvitation . fst <$> teamInvitation)
    let uid = userId (accountUser account)

    Log.info $ field "user" (toByteString uid) . msg (val "Creating user")
    activatedTeam <- lift $ do
        Data.insertAccount account pw False searchable
        Intra.createSelfConv uid
        Intra.onUserEvent uid Nothing (UserCreated account)
        -- If newUserEmailCode is set, team gets activated _now_ else createUser fails
        fmap join . for newTeam $ createTeam uid (isJust newUserEmailCode)

    (emailInvited, phoneInvited) <- case invitation of
        Just (inv, invInfo) -> case inIdentity inv of
            Left em -> do
                acceptInvitation account inv invInfo (userEmailKey em) (EmailIdentity em)
                return (True, False)
            Right ph -> do
                acceptInvitation account inv invInfo (userPhoneKey ph) (PhoneIdentity ph)
                return (False, True)
        Nothing -> return (False, False)

    (teamEmailInvited, joinedTeam) <- case teamInvitation of
        Just (inv, invInfo) -> do
                let em = Team.inIdentity inv
                acceptTeamInvitation account inv invInfo (userEmailKey em) (EmailIdentity em)
                Team.TeamName nm <- lift $ Intra.getTeamName (Team.inTeam inv)
                return (True, Just $ CreateUserTeam (Team.inTeam inv) nm)
        Nothing -> return (False, Nothing)

    -- Handle e-mail activation
    edata <- if emailInvited || teamEmailInvited
            then return Nothing
            else fmap join . for emKey $ \ek -> case newUserEmailCode of
                Nothing -> do
                    timeout <- setActivationTimeout <$> view settings
                    edata   <- lift $ Data.newActivation ek timeout (Just uid)
                    Log.info $ field "user"            (toByteString uid)
                             . field "activation.key"  (toByteString $ activationKey edata)
                             . msg (val "Created email activation key/code pair")
                    return $ Just edata
                Just c -> do
                    ak <- liftIO $ Data.mkActivationKey ek
                    void $ activate (ActivateKey ak) c (Just uid) !>> EmailActivationError
                    return Nothing

    -- Handle phone activation
    pdata <- if phoneInvited
            then return Nothing
            else fmap join . for phKey $ \pk -> case newUserPhoneCode of
                Nothing -> do
                    timeout <- setActivationTimeout <$> view settings
                    pdata   <- lift $ Data.newActivation pk timeout (Just uid)
                    Log.info $ field "user"            (toByteString uid)
                             . field "activation.key"  (toByteString $ activationKey pdata)
                             . msg (val "Created phone activation key/code pair")
                    return $ Just pdata
                Just c -> do
                    ak <- liftIO $ Data.mkActivationKey pk
                    void $ activate (ActivateKey ak) c (Just uid) !>> PhoneActivationError
                    return Nothing

    return $! CreateUserResult account edata pdata (activatedTeam <|> joinedTeam)
  where
    checkKey u k = do
        av <- lift $ Data.keyAvailable k u
        unless av $
            throwE $ DuplicateUserKey k

    createTeam uid activating t = do
        created <- Intra.createTeam uid t
        return $ if activating
                    then Just created
                    else Nothing

    handleTeam (Just (Left i))  e = (Nothing, ) <$> findTeamInvitation e i
    handleTeam (Just (Right t)) _ = return (Just t, Nothing)
    handleTeam Nothing          _ = return (Nothing, Nothing)

    findInvitation :: Maybe UserKey -> Maybe UserKey -> InvitationCode -> ExceptT CreateUserError AppIO (Maybe (Invitation, InvitationInfo))
    findInvitation Nothing Nothing _ = throwE MissingIdentity
    findInvitation e       p       i = lift (Data.lookupInvitationInfo i) >>= \case
        Just ii -> do
            inv <- lift $ Data.lookupInvitation (iiInviter ii) (iiInvId ii)
            case (inv, inIdentity <$> inv) of
                (Just invite, Just (Left em))  | e == Just (userEmailKey em) -> return $ Just (invite, ii)
                (Just invite, Just (Right ph)) | p == Just (userPhoneKey ph) -> return $ Just (invite, ii)
                _                                                        -> throwE InvalidInvitationCode
        Nothing -> throwE InvalidInvitationCode

    findTeamInvitation :: Maybe UserKey -> InvitationCode -> ExceptT CreateUserError AppIO (Maybe (Team.Invitation, Team.InvitationInfo))
    findTeamInvitation Nothing  _ = throwE MissingIdentity
    findTeamInvitation (Just e) c = lift (Team.lookupInvitationInfo c) >>= \case
        Just ii -> do
            inv <- lift $ Team.lookupInvitation (Team.iiTeam ii) (Team.iiInvId ii)
            case (inv, Team.inIdentity <$> inv) of
                (Just invite, Just em) | e == userEmailKey em -> ensureMemberCanJoin (Team.iiTeam ii) >>
                                                                 (return $ Just (invite, ii))
                _                                             -> throwE InvalidInvitationCode
        Nothing -> throwE InvalidInvitationCode

    ensureMemberCanJoin tid = do
        mems <- lift $ Intra.getTeamMembers tid
        when (length (mems^.Team.teamMembers) >= 128) $
            throwE TooManyTeamMembers

    acceptTeamInvitation account inv ii uk ident = do
        let uid = userId (accountUser account)
        ok <- lift $ Data.claimKey uk uid
        unless ok $
            throwE $ DuplicateUserKey uk
        added <- lift $ Intra.addTeamMember uid (Team.iiTeam ii)
        unless added $
            throwE TooManyTeamMembers
        lift $ do
            activateUser uid ident
            void $ onActivated (AccountActivated account)
            Log.info $ field "user" (toByteString uid)
                     . field "team" (toByteString $ Team.iiTeam ii)
                     . msg (val "Accepting invitation")
            Team.deleteInvitation (Team.inTeam inv) (Team.inInvitation inv)

    acceptInvitation account inv ii uk ident = do
        -- Check that the inviter is an active user
        status <- lift $ Data.lookupStatus (inInviter inv)
        unless (status == Just Active) $
            throwE InvalidInvitationCode
        let uid = userId (accountUser account)
        ok <- lift $ Data.claimKey uk uid
        unless ok $
            throwE $ DuplicateUserKey uk
        lift $ do
            activateUser uid ident
            void $ onActivated (AccountActivated account)
            Log.info $ field "user" (toByteString uid)
                     . field "inviter" (toByteString $ iiInviter ii)
                     . msg (val "Accepting invitation")
            -- Connect users and create the 1:1 connection
            c   <- Intra.createConnectConv uid (iiInviter ii) Nothing Nothing Nothing
            _   <- Intra.acceptConnectConv (iiInviter ii) Nothing c
            ucs <- Data.connectUsers uid [(iiInviter ii, c)]
            let toEvent uc = ConnectionUpdated uc Nothing (mfilter (const $ ucFrom uc /= uid) (Just $ inName inv))
            forM_ ucs $ Intra.onConnectionEvent uid Nothing . toEvent
            Data.deleteInvitation (inInviter inv) (inInvitation inv)

-------------------------------------------------------------------------------
-- Update Profile

updateUser :: UserId -> ConnId -> UserUpdate -> AppIO ()
updateUser uid conn uu = do
    Data.updateUser uid uu
    Intra.onUserEvent uid (Just conn) (profileUpdated uid uu)

-------------------------------------------------------------------------------
-- Update Locale

changeLocale :: UserId -> ConnId -> LocaleUpdate -> AppIO ()
changeLocale uid conn (LocaleUpdate loc) = do
    Data.updateLocale uid loc
    Intra.onUserEvent uid (Just conn) (localeUpdate uid loc)

--------------------------------------------------------------------------------
-- Change Handle

changeHandle :: UserId -> ConnId -> Handle -> ExceptT ChangeHandleError AppIO ()
changeHandle uid conn hdl = do
    when (isBlacklistedHandle hdl) $
        throwE ChangeHandleInvalid
    usr <- lift $ Data.lookupUser uid
    case usr of
        Nothing -> throwE ChangeHandleNoIdentity
        Just  u -> claim u
  where
    claim u = do
        unless (isJust (userIdentity u)) $
            throwE ChangeHandleNoIdentity
        claimed <- lift $ claimHandle u hdl
        unless claimed $
            throwE ChangeHandleExists
        lift $ Intra.onUserEvent uid (Just conn) (handleUpdated uid hdl)

--------------------------------------------------------------------------------
-- Check Handles

checkHandles :: [Handle] -> Word -> AppIO [Handle]
checkHandles check num = reverse <$> collectFree [] check num
  where
    collectFree free  _     0 = return free
    collectFree free []     _ = return free
    collectFree free (h:hs) n =
        if isBlacklistedHandle h
            then collectFree free hs n
            else do
                owner <- glimpseHandle h
                case owner of
                    Nothing -> collectFree (h:free) hs (n - 1)
                    Just  _ -> collectFree free hs n

-------------------------------------------------------------------------------
-- Change Email

changeEmail :: UserId -> Email -> ExceptT ChangeEmailError AppIO (Activation, Email)
changeEmail u email = do
    em <- maybe (throwE $ InvalidNewEmail email)
                return
                (validateEmail email)
    let ek = userEmailKey em
    blacklisted <- lift $ Blacklist.exists ek
    when blacklisted $
        throwE (ChangeBlacklistedEmail email)
    available <- lift $ Data.keyAvailable ek (Just u)
    unless available $
        throwE $ EmailExists email
    timeout <- setActivationTimeout <$> view settings
    act <- lift $ Data.newActivation ek timeout (Just u)
    return (act, em)

-------------------------------------------------------------------------------
-- Change Phone

changePhone :: UserId -> Phone -> ExceptT ChangePhoneError AppIO (Activation, Phone)
changePhone u phone = do
    ph <- maybe (throwE $ InvalidNewPhone phone)
                return
                =<< lift (validatePhone phone)
    let pk = userPhoneKey ph
    available <- lift $ Data.keyAvailable pk (Just u)
    unless available $
        throwE $ PhoneExists phone
    timeout <- setActivationTimeout <$> view settings
    act <- lift $ Data.newActivation pk timeout (Just u)
    return (act, ph)

-------------------------------------------------------------------------------
-- Remove Email

removeEmail :: UserId -> ConnId -> ExceptT RemoveIdentityError AppIO ()
removeEmail uid conn = do
    ident <- lift $ fetchUserIdentity uid
    case ident of
        Just (FullIdentity e _) -> lift $ do
            deleteKey $ userEmailKey e
            Data.deleteEmail uid
            Intra.onUserEvent uid (Just conn) (emailRemoved uid e)
        Just _  -> throwE LastIdentity
        Nothing -> throwE NoIdentity

-------------------------------------------------------------------------------
-- Remove Phone

removePhone :: UserId -> ConnId -> ExceptT RemoveIdentityError AppIO ()
removePhone uid conn = do
    ident <- lift $ fetchUserIdentity uid
    case ident of
        Just (FullIdentity _ p) -> do
            pw  <- lift $ Data.lookupPassword uid
            unless (isJust pw) $
                throwE NoPassword
            lift $ do
                deleteKey $ userPhoneKey p
                Data.deletePhone uid
                Intra.onUserEvent uid (Just conn) (phoneRemoved uid p)
        Just _  -> throwE LastIdentity
        Nothing -> throwE NoIdentity

-------------------------------------------------------------------------------
-- Forcefully revoke a verified identity

revokeIdentity :: Either Email Phone -> AppIO ()
revokeIdentity key = do
    let uk = either userEmailKey userPhoneKey key
    mu <- Data.lookupKey uk
    case mu of
        Nothing -> return ()
        Just  u -> fetchUserIdentity u >>= \case
            Just (FullIdentity  _ _) -> revokeKey u uk
            Just (EmailIdentity e  ) | Left  e == key -> do
                revokeKey u uk
                Data.deactivateUser u
            Just (PhoneIdentity   p) | Right p == key -> do
                revokeKey u uk
                Data.deactivateUser u
            _ -> return ()
  where
    revokeKey u uk = do
        deleteKey uk
        foldKey (\(_ :: Email) -> Data.deleteEmail u)
                (\(_ :: Phone) -> Data.deletePhone u) uk
        Intra.onUserEvent u Nothing $ foldKey (emailRemoved u)
                                              (phoneRemoved u) uk

-------------------------------------------------------------------------------
-- Change Account Status

changeAccountStatus :: List1 UserId -> AccountStatus -> ExceptT AccountStatusError AppIO ()
changeAccountStatus usrs status = do
    e <- ask
    ev <- case status of
        Active    -> return UserResumed
        Suspended -> liftIO $ mapConcurrently (runAppT e . revokeAllCookies) usrs >> return UserSuspended
        Deleted   -> throwE InvalidAccountStatus
    liftIO $ mapConcurrently_ (runAppT e . (update ev)) usrs
  where
    update :: (UserId -> UserEvent) -> UserId -> AppIO ()
    update ev u = do
        Data.updateStatus u status
        Intra.onUserEvent u Nothing (ev u)

-------------------------------------------------------------------------------
-- Activation

activate :: ActivationTarget
         -> ActivationCode
         -> Maybe UserId -- ^ The user for whom to activate the key.
         -> ExceptT ActivationError AppIO ActivationResult
activate tgt code usr = do
    key <- mkActivationKey tgt
    activateKey key code usr

activateKey :: ActivationKey
         -> ActivationCode
         -> Maybe UserId -- ^ The user for whom to activate the key.
         -> ExceptT ActivationError AppIO ActivationResult
activateKey key code usr = do
    Log.info $ field "activation.key"  (toByteString key)
             . field "activation.code" (toByteString code)
             . msg (val "Activating")
    event <- Data.activateKey key code usr
    case event of
        Nothing -> return ActivationPass
        Just  e -> do
            (ident, first) <- lift $ onActivated e
            return $ ActivationSuccess ident first

preverify :: ActivationTarget -> ActivationCode -> ExceptT ActivationError AppIO ()
preverify tgt code = do
    key <- mkActivationKey tgt
    void $ Data.verifyCode key code

onActivated :: ActivationEvent -> AppIO (Maybe UserIdentity, Bool)
onActivated (AccountActivated account) = do
    let uid = userId (accountUser account)
    Log.info $ field "user" (toByteString uid) . msg (val "User activated")
    Intra.onUserEvent uid Nothing $ UserActivated account
    activateTeam uid
    return (userIdentity (accountUser account), True)
onActivated (EmailActivated uid email) = do
    Intra.onUserEvent uid Nothing (emailUpdated uid email)
    return (Just (EmailIdentity email), False)
onActivated (PhoneActivated uid phone) = do
    Intra.onUserEvent uid Nothing (phoneUpdated uid phone)
    return (Just (PhoneIdentity phone), False)

activateTeam :: UserId -> AppIO ()
activateTeam uid = do
    tid <- Intra.getTeamId uid
    for_ tid $ flip Intra.changeTeamStatus Team.Active

sendActivationCode :: Either Email Phone -> Maybe Locale -> Bool -> ExceptT SendActivationCodeError AppIO ()
sendActivationCode emailOrPhone loc call = case emailOrPhone of
    Left email -> do
        ek <- maybe (throwE $ InvalidRecipient (userEmailKey email))
                    (return . userEmailKey)
                    (validateEmail email)
        exists <- lift $ isJust <$> Data.lookupKey ek
        when exists $
            throwE $ UserKeyInUse ek
        blacklisted <- lift $ Blacklist.exists ek
        when blacklisted $
            throwE (ActivationBlacklistedUserKey ek)
        uc <- lift $ Data.lookupActivationCode ek
        case uc of
            Nothing            -> sendVerificationEmail ek Nothing      -- Fresh code request, no user
            Just (Nothing , c) -> sendVerificationEmail ek (Just c)     -- Re-requesting existing code
            Just (Just uid, c) -> sendActivationEmail   ek c        uid -- User re-requesting activation

    Right phone -> do
        pk <- maybe (throwE $ InvalidRecipient (userPhoneKey phone))
                    (return . userPhoneKey)
                    =<< lift (validatePhone phone)
        exists <- lift $ isJust <$> Data.lookupKey pk
        when exists $
            throwE $ UserKeyInUse pk
        blacklisted <- lift $ Blacklist.exists pk
        when blacklisted $
            throwE (ActivationBlacklistedUserKey pk)
        c <- lift $ fmap snd <$> Data.lookupActivationCode pk
        p <- mkPair pk c Nothing
        void . forPhoneKey pk $ \ph -> lift $
            if call then sendActivationCall ph p loc
                    else sendActivationSms  ph p loc
  where
    notFound = throwM . UserNameNotFound

    mkPair k c u = do
        timeout <- setActivationTimeout <$> view settings
        case c of
            Just  c' -> liftIO $ (,c') <$> Data.mkActivationKey k
            Nothing  -> lift $ do
                dat <- Data.newActivation k timeout u
                return (activationKey dat, activationCode dat)

    sendVerificationEmail ek uc = do
        p <- mkPair ek uc Nothing
        void . forEmailKey ek $ \em -> lift $
            sendVerificationMail em p loc

    sendActivationEmail ek uc uid = do
        u   <- maybe (notFound uid) return =<< lift (Data.lookupUser uid)
        p   <- mkPair ek (Just uc) (Just uid)
        let ident = userIdentity u
        let name  = userName u
        let loc'  = loc <|> Just (userLocale u)
        void . forEmailKey ek $ \em -> lift $
            sendActivationMail em name p loc' ident

mkActivationKey :: ActivationTarget -> ExceptT ActivationError AppIO ActivationKey
mkActivationKey (ActivateKey   k) = return k
mkActivationKey (ActivateEmail e) = do
    ek <- maybe (throwE $ InvalidActivationEmail e)
                (return . userEmailKey)
                (validateEmail e)
    liftIO $ Data.mkActivationKey ek
mkActivationKey (ActivatePhone p) = do
    pk <- maybe (throwE $ InvalidActivationPhone p)
                (return . userPhoneKey)
                =<< lift (validatePhone p)
    liftIO $ Data.mkActivationKey pk

-------------------------------------------------------------------------------
-- Password Management

changePassword :: UserId -> PasswordChange -> ExceptT ChangePasswordError AppIO ()
changePassword u cp = do
    activated <- lift $ Data.isActivated u
    unless activated $
        throwE ChangePasswordNoIdentity
    currpw <- lift $ Data.lookupPassword u
    let newpw = cpNewPassword cp
    case (currpw, cpOldPassword cp) of
        (Nothing,        _) -> lift $ Data.updatePassword u newpw
        (Just  _,  Nothing) -> throwE InvalidCurrentPassword
        (Just pw, Just pw') -> do
            unless (verifyPassword pw' pw) $
                throwE InvalidCurrentPassword
            lift $ Data.updatePassword u newpw >> revokeAllCookies u

beginPasswordReset :: Either Email Phone -> ExceptT PasswordResetError AppIO (UserId, PasswordResetPair)
beginPasswordReset target = do
    let key = either userEmailKey userPhoneKey target
    user <- lift (Data.lookupKey key) >>= maybe (throwE InvalidPasswordResetKey) return
    status <- lift $ Data.lookupStatus user
    unless (status == Just Active) $
        throwE InvalidPasswordResetKey
    code <- lift $ Data.lookupPasswordResetCode user
    when (isJust code) $
        throwE PasswordResetInProgress
    (user,) <$> lift (Data.createPasswordResetCode user target)

completePasswordReset :: PasswordResetIdentity -> PasswordResetCode -> PlainTextPassword -> ExceptT PasswordResetError AppIO ()
completePasswordReset ident code pw = do
    key <- mkPasswordResetKey ident
    usr <- lift $ Data.verifyPasswordResetCode (key, code)
    case usr of
        Nothing -> throwE InvalidPasswordResetCode
        Just  u -> lift $ do
            Data.updatePassword u pw
            Data.deletePasswordResetCode key
            revokeAllCookies u

mkPasswordResetKey :: PasswordResetIdentity -> ExceptT PasswordResetError AppIO PasswordResetKey
mkPasswordResetKey ident = case ident of
    PasswordResetIdentityKey   k -> return k
    PasswordResetEmailIdentity e -> user (userEmailKey e) >>= liftIO . Data.mkPasswordResetKey
    PasswordResetPhoneIdentity p -> user (userPhoneKey p) >>= liftIO . Data.mkPasswordResetKey
  where
    user uk = lift (Data.lookupKey uk) >>= maybe (throwE InvalidPasswordResetKey) return

-------------------------------------------------------------------------------
-- User Deletion

deleteUser :: UserId -> Maybe PlainTextPassword -> ExceptT DeleteUserError AppIO (Maybe Timeout)
deleteUser uid pwd = do
    account <- lift $ Data.lookupAccount uid
    case account of
        Nothing -> throwE DeleteUserInvalid
        Just  a -> case accountStatus a of
            Deleted   -> return Nothing
            Suspended -> ensureNotOnlyOwner >> go a
            Active    -> ensureNotOnlyOwner >> go a
  where
    ensureNotOnlyOwner = lift (Intra.getTeamContacts uid) >>= \case
        Just mems | Team.isOnlyOwner uid (mems^.Team.teamMembers) ->
            throwE DeleteUserOnlyOwner
        _ -> return ()

    go a = maybe (byIdentity a) (byPassword a) pwd

    byIdentity a = case userIdentity (accountUser a) of
        Just (FullIdentity  e _) -> sendCode a (Left e)
        Just (EmailIdentity e  ) -> sendCode a (Left e)
        Just (PhoneIdentity p  ) -> sendCode a (Right p)
        Nothing                  -> case pwd of
            Just  _ -> throwE DeleteUserMissingPassword
            Nothing -> lift $ deleteAccount a >> return Nothing

    byPassword a pw = do
        Log.info $ field "user" (toByteString uid)
                 . msg (val "Attempting account deletion with a password")
        actual <- lift $ Data.lookupPassword uid
        case actual of
            Nothing -> throwE DeleteUserInvalidPassword
            Just  p -> do
                unless (verifyPassword pw p) $
                    throwE DeleteUserInvalidPassword
                lift $ deleteAccount a >> return Nothing

    sendCode a target = do
        gen <- Code.mkGen (either Code.ForEmail Code.ForPhone target)
        pending <- lift $ Code.lookup (Code.genKey gen) Code.AccountDeletion
        case pending of
            Just  c -> throwE $! DeleteUserPendingCode (Code.codeTTL c)
            Nothing -> do
                Log.info $ field "user" (toByteString uid)
                         . msg (val "Sending verification code for account deletion")
                c <- Code.generate gen Code.AccountDeletion (Code.Retries 3)
                                                            (Code.Timeout 600)
                                                            (Just (toUUID uid))
                Code.insert c
                let k = Code.codeKey c
                let v = Code.codeValue c
                let l = userLocale (accountUser a)
                let n = userName (accountUser a)
                either (\e -> lift $ sendDeletionEmail n e k v l)
                       (\p -> lift $ sendDeletionSms p k v l)
                       target
                    `onException`
                       Code.delete k Code.AccountDeletion
                return $! Just $! Code.codeTTL c

verifyDeleteUser :: VerifyDeleteUser -> ExceptT DeleteUserError AppIO ()
verifyDeleteUser d = do
    let key  = verifyDeleteUserKey d
    let code = verifyDeleteUserCode d
    c <- lift $ Code.verify key Code.AccountDeletion code
    a <- maybe (throwE DeleteUserInvalidCode) return (Code.codeAccount =<< c)
    account <- lift $ Data.lookupAccount (Id a)
    for_ account $ lift . deleteAccount
    lift $ Code.delete key Code.AccountDeletion

deleteAccount :: UserAccount -> AppIO ()
deleteAccount account@(accountUser -> user) = do
    let uid = userId user
    Log.info $ field "user" (toByteString uid) . msg (val "Deleting account")
    -- Free unique keys
    for_ (userEmail  user) $ deleteKey . userEmailKey
    for_ (userPhone  user) $ deleteKey . userPhoneKey
    for_ (userHandle user) freeHandle
    -- Wipe data
    Data.clearProperties uid
    tombstone <- mkTombstone
    Data.insertAccount tombstone Nothing False (SearchableStatus False)
    Intra.rmUser uid
    Data.lookupClients uid >>= mapM_ (Data.rmClient uid . clientId)
    Intra.onUserEvent uid Nothing (UserDeleted uid)
    -- Note: Connections can only be deleted afterwards, since
    --       they need to be notified.
    Data.deleteConnections uid
    revokeAllCookies uid
  where
    mkTombstone = do
        defLoc <- setDefaultLocale <$> view settings
        return $ account
            { accountStatus     = Deleted
            , accountUser       = user
                { userName     = Name "default"
                , userAccentId = defaultAccentId
                , userPict     = noPict
                , userAssets   = []
                , userHandle   = Nothing
                , userLocale   = defLoc
                , userIdentity = Nothing
                }
            }

-------------------------------------------------------------------------------
-- Lookups

lookupActivationCode :: Either Email Phone -> AppIO (Maybe ActivationPair)
lookupActivationCode emailOrPhone = do
    let uk = either userEmailKey userPhoneKey emailOrPhone
    k <- liftIO $ Data.mkActivationKey uk
    c <- fmap snd <$> Data.lookupActivationCode uk
    return $ (k,) <$> c

lookupPasswordResetCode :: Either Email Phone -> AppIO (Maybe PasswordResetPair)
lookupPasswordResetCode emailOrPhone = do
    let uk = either userEmailKey userPhoneKey emailOrPhone
    usr <- Data.lookupKey uk
    case usr of
        Nothing -> return Nothing
        Just  u -> do
            k <- liftIO $ Data.mkPasswordResetKey u
            c <- Data.lookupPasswordResetCode u
            return $ (k,) <$> c

lookupProfile :: UserId -> UserId -> AppIO (Maybe UserProfile)
lookupProfile self other = listToMaybe <$> lookupProfiles self [other]

-- | Obtain user profiles for a list of users as they can be seen by
-- a given user 'A'. User 'A' can see the 'FullProfile' of any other user 'B',
-- if the reverse relation (B -> A) is either 'Accepted' or 'Sent'.
-- Otherwise only the 'PublicProfile' is accessible for user 'A'.
lookupProfiles :: UserId   -- ^ User 'A' on whose behalf the profiles are requested.
               -> [UserId] -- ^ The users ('B's) for which to obtain the profiles.
               -> AppIO [UserProfile]
lookupProfiles self others = do
    users <- Data.lookupUsers others
    css   <- toMap <$> Data.lookupConnectionStatus (map userId users) [self]
    return $ map (toProfile css) users
  where
    toMap = Map.fromList . map (csFrom &&& csStatus)
    toProfile css u =
        let cs = Map.lookup (userId u) css
        in if userId u == self || cs == Just Accepted || cs == Just Sent
            then connectedProfile u
            else publicProfile u

-- | Obtain a profile for a user as he can see himself.
lookupSelfProfile :: UserId -> AppIO (Maybe SelfProfile)
lookupSelfProfile = fmap (fmap mk) . Data.lookupAccount
  where
    mk a = SelfProfile (accountUser a)

-- | Find user accounts for a given identity, both activated and those
-- currently pending activation.
lookupAccountsByIdentity :: Either Email Phone -> AppIO [UserAccount]
lookupAccountsByIdentity emailOrPhone = do
    let uk = either userEmailKey userPhoneKey emailOrPhone
    activeUid  <- Data.lookupKey uk
    uidFromKey <- (>>= fst) <$> Data.lookupActivationCode uk
    Data.lookupAccounts (nub $ catMaybes [activeUid, uidFromKey])

isBlacklisted :: Either Email Phone -> AppIO Bool
isBlacklisted emailOrPhone = do
    let uk = either userEmailKey userPhoneKey emailOrPhone
    Blacklist.exists uk

blacklistInsert :: Either Email Phone -> AppIO ()
blacklistInsert emailOrPhone = do
    let uk = either userEmailKey userPhoneKey emailOrPhone
    Blacklist.insert uk

blacklistDelete :: Either Email Phone -> AppIO ()
blacklistDelete emailOrPhone = do
    let uk = either userEmailKey userPhoneKey emailOrPhone
    Blacklist.delete uk

-------------------------------------------------------------------------------
-- Utilities

-- TODO: Move to a util module or similar
fetchUserIdentity :: UserId -> AppIO (Maybe UserIdentity)
fetchUserIdentity uid = lookupSelfProfile uid >>= maybe
    (throwM $ UserProfileNotFound uid)
    (return . userIdentity . selfUser)


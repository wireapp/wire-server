{-# OPTIONS_GHC -Wwarn #-}

module Wire.UserSubsystem.Interpreter where

import Data.Domain
import Data.Id
import Data.LegalHold
import Data.Qualified
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.Team.Member
import Wire.API.User
import Wire.UserStore

data UserSubsystemConfig = UserSubsystemConfig
  { emailVisibilityConfig :: EmailVisibilityConfig,
    defaultDomain :: Domain,
    defaultLocale :: Locale
  }

getUserProfileImpl ::
  ( Member UserStore r,
    Member (Input UserSubsystemConfig) r
  ) =>
  UserId ->
  Qualified UserId ->
  Sem r (Maybe UserProfile)
getUserProfileImpl requestingUser quid = do
  emailVisibilityConfig <- inputs emailVisibilityConfig
  emailVisibilityConfigWithViewer <-
    case emailVisibilityConfig of
      EmailVisibleIfOnTeam -> pure EmailVisibleIfOnTeam
      EmailVisibleToSelf -> pure EmailVisibleToSelf
      EmailVisibleIfOnSameTeam () ->
        EmailVisibleIfOnSameTeam <$> getSelfInfo requestingUser
  domain <- inputs defaultDomain
  locale <- inputs defaultLocale
  user <- runMaybeT $ do
    u <- MaybeT $ getUser (qUnqualified quid)
    maybe pure mempty $ mkUserFromStored domain locale NoPendingInvitations u
  -- user <- (mkUserFromStored domain locale NoPendingInvitations =<<) <$> getUser (qUnqualified quid)
  pure $ (\u -> mkUserProfile emailVisibilityConfigWithViewer u UserLegalHoldDisabled) <$> user
  where
    getSelfInfo :: UserId -> Sem r (Maybe (TeamId, TeamMember))
    getSelfInfo selfId = do
      -- FUTUREWORK: it is an internal error for the two lookups (for 'User' and 'TeamMember')
      -- to return 'Nothing'.  we could throw errors here if that happens, rather than just
      -- returning an empty profile list from 'lookupProfiles'.
      mUser <- getUser selfId
      let mUserNotPending = case mUser of
            Nothing -> Nothing
            Just user -> if user.status /= Just PendingInvitation then Just user else Nothing
      case mUserNotPending.teamId of
        Nothing -> pure Nothing
        Just tid -> (tid,) <$$> liftSem (GalleyProvider.getTeamMember selfId tid)

mkUserFromStored :: Domain -> Locale -> HavePendingInvitations -> StoredUser -> Maybe User
mkUserFromStored domain defaultLocale havePendingInvitations storedUser =
  if havePendingInvitations == NoPendingInvitations && storedUser.status == Just PendingInvitation
    then Nothing
    else
      let ident = toIdentity storedUser.activated storedUser.email storedUser.phone storedUser.ssoid
          deleted = Just Deleted == storedUser.status
          expiration = if status == Just Ephemeral then expires else Nothing
          loc = toLocale defaultLocale (storedUser.language, storedUser.country)
          svc = newServiceRef <$> sid <*> pid
       in User
            (Qualified uid domain)
            ident
            name
            (fromMaybe noPict pict)
            (fromMaybe [] assets)
            accent
            deleted
            loc
            svc
            handle
            expiration
            tid
            (fromMaybe ManagedByWire managed_by)
            (fromMaybe defSupportedProtocols prots)

toLocale :: Locale -> (Maybe Language, Maybe Country) -> Locale
toLocale _ (Just l, c) = Locale l c
toLocale l _ = l

--
-- If the user is not activated, 'toIdentity' will return 'Nothing' as a precaution, because
-- elsewhere we rely on the fact that a non-empty 'UserIdentity' means that the user is
-- activated.
--
-- The reason it's just a "precaution" is that we /also/ have an invariant that having an
-- email or phone in the database means the user has to be activated.
toIdentity ::
  -- | Whether the user is activated
  Bool ->
  Maybe Email ->
  Maybe Phone ->
  Maybe UserSSOId ->
  Maybe UserIdentity
toIdentity True (Just e) (Just p) Nothing = Just $! FullIdentity e p
toIdentity True (Just e) Nothing Nothing = Just $! EmailIdentity e
toIdentity True Nothing (Just p) Nothing = Just $! PhoneIdentity p
toIdentity True email phone (Just ssoid) = Just $! SSOIdentity ssoid email phone
toIdentity True Nothing Nothing Nothing = Nothing
toIdentity False _ _ _ = Nothing

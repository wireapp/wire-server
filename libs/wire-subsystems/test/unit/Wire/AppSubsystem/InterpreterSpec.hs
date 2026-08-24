{-# OPTIONS_GHC -Wno-ambiguous-fields -Wno-incomplete-uni-patterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.AppSubsystem.InterpreterSpec (spec) where

import Data.Default (def)
import Data.Domain
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (..))
import Data.Map qualified as Map
import Data.Misc
import Data.Qualified
import Data.Tagged (Tagged)
import Data.Text qualified as T
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal
import Polysemy.State
import Polysemy.TinyLog (TinyLog)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.Error (ErrorS)
import Wire.API.Error.Galley (GalleyError (TeamMemberNotFound, TeamNotFound))
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.User
import Wire.AppStore hiding (deleteApp, updateApp)
import Wire.AppSubsystem
import Wire.AppSubsystem.Interpreter
import Wire.EmailSubsystem
import Wire.Events (Events)
import Wire.GalleyAPIAccess
import Wire.MockInterpreters
import Wire.NotificationSubsystem
import Wire.Sem.Now (Now)
import Wire.Sem.Random (Random)
import Wire.StoredUser
import Wire.TeamSubsystem
import Wire.TeamSubsystem.GalleyAPI (interpretTeamSubsystemToGalleyAPI)
import Wire.UserKeyStore
import Wire.UserStore

type LowerAppEffects =
  '[ Now,
     Random,
     TinyLog,
     Input AppSubsystemConfig,
     State [Push],
     State [MiniEvent],
     State (Map EmailAddress [SentMail]),
     State [StoredApp],
     State MockAuthenticationState,
     Error AppSubsystemError,
     ErrorS 'TeamMemberNotFound,
     ErrorS 'TeamNotFound
   ]

-- | (fisx) I wonder if we should do this everywhere?
(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = g . f

infixr 9 .>

runLowerAppEffects ::
  [StoredApp] ->
  Sem (LowerAppEffects `Append` r) a ->
  Sem r (Either AppSubsystemError (a, Map EmailAddress [SentMail]))
runLowerAppEffects initialApps =
  interpretNowConst defaultTime
    .> runRandomPure
    .> noopLogger
    .> runInputConst def
    .> evalState @[Push] []
    .> evalState @[MiniEvent] []
    .> runState @(Map EmailAddress [SentMail]) mempty
    .> evalState initialApps
    .> evalState emptyMockAuthenticationState
    .> runError @AppSubsystemError
    .> runError @(Tagged 'TeamMemberNotFound ())
    .> runError @(Tagged 'TeamNotFound ())
    .> fmap (either (error . show) (either (error . show) (fmap swap)))

-- | Run a single AppSubsystem operation and return the emails that were sent.
-- UserSubsystem and AuthenticationSubsystem are stubs: they crash loudly if invoked.
runAppEffects ::
  [StoredUser] ->
  [StoredApp] ->
  Map TeamId [TeamMember] ->
  Sem
    ( '[ AppSubsystem,
         TeamSubsystem,
         GalleyAPIAccess,
         UserStore,
         UserKeyStore,
         AppStore,
         EmailSubsystem,
         NotificationSubsystem,
         Events
       ]
        `Append` LowerAppEffects
    )
    a ->
  Either AppSubsystemError (a, Map EmailAddress [SentMail])
runAppEffects initialUsers initialApps teams action =
  run
    . ( runAppSubsystem inMemoryUserSubsystemInterpreter mockAuthenticationSubsystemInterpreter
          .> interpretTeamSubsystemToGalleyAPI
          .> miniGalleyAPIAccess teams def
          .> runInMemoryUserStoreInterpreter initialUsers mempty
          .> runInMemoryUserKeyStoreIntepreterWithStoredUsers initialUsers
          .> inMemoryAppStoreInterpreter
          .> inMemoryEmailSubsystemInterpreter
          .> inMemoryNotificationSubsystemInterpreter
          .> miniEventInterpreter
          .> runLowerAppEffects initialApps
      )
    $ action

-- | Minimal StoredUser with only the fields we care about set.
mkStoredUser :: UserId -> Name -> Maybe EmailAddress -> Maybe TeamId -> StoredUser
mkStoredUser uid uname email tid =
  StoredUser
    { id = uid,
      userType = Nothing,
      name = uname,
      textStatus = Nothing,
      pict = Nothing,
      email = email,
      emailUnvalidated = Nothing,
      ssoId = Nothing,
      accentId = ColourId 0,
      assets = Nothing,
      activated = True,
      status = Just Active,
      expires = Nothing,
      language = Nothing,
      country = Nothing,
      providerId = Nothing,
      serviceId = Nothing,
      handle = Nothing,
      teamId = tid,
      managedBy = Nothing,
      supportedProtocols = Nothing,
      searchable = Nothing
    }

-- | A team member with full (owner) permissions, so they pass both CreateApp
-- and isAdminOrOwner checks.
mkOwnerMember :: UserId -> TeamMember
mkOwnerMember uid = mkTeamMember uid fullPermissions Nothing UserLegalHoldDisabled

spec :: Spec
spec = describe "AppSubsystem" $ do
  focus . prop "CRUD" . noShrinking {- shrinking takes too long in this test -} $
    \(tid :: TeamId)
     (creatorId :: UserId)
     (memberId :: UserId)
     (mbMemberEmail :: Maybe EmailAddress)
     (newApp :: NewApp) ->
        let mbPassword = plainTextPassword6 "123456aA1!"
            domain = Domain "localdomain"
            Just creatorEmail = emailAddressText "creator@example.com"
            creator = mkStoredUser creatorId (Name "Creator") (Just creatorEmail) (Just tid)
            lCreatorId = toLocalUnsafe domain creatorId
            creatorMember = mkOwnerMember creatorId
            memberUser = mkStoredUser memberId (Name "member") mbMemberEmail (Just tid)
            memberMember = mkTeamMember memberId (rolePermissions RoleMember) Nothing UserLegalHoldDisabled
            team = Map.singleton tid [creatorMember, memberMember]
            putApp = (def :: PutApp) {name = Just (changeName newApp.name)}
              where
                changeName :: Name -> Name
                changeName (Name nm) = Name $ case T.splitAt 3 nm of
                  (a, b) -> (if a == "aaa" then "BBB" else "aaa") <> b

            result = runAppEffects [creator, memberUser] [] team do
              createdApp <- Wire.AppSubsystem.createApp lCreatorId tid newApp
              let appId :: UserId = createdApp.user.profileQualifiedId & qUnqualified
              appInfo <- Wire.AppSubsystem.getApp lCreatorId tid appId
              allApps <- Wire.AppSubsystem.getApps lCreatorId tid
              appUser <- getUser appId
              updateApp lCreatorId tid appId putApp
              eNewCookieResult <- refreshAppCookie lCreatorId tid appId mbPassword
              internalDeleteApp tid appId
              appUser' <- getUser appId
              deleteAppSendEmail tid appId (mkUserFromStored domain def <$> appUser')
              allApps' <- Wire.AppSubsystem.getApps lCreatorId tid
              pure (createdApp, appInfo, allApps, appUser, eNewCookieResult, appUser', allApps')

            checkState (createdApp, appInfo, allApps, appUser, eNewCookieResult, appUser', allApps') =
              let appId = createdApp.user.profileQualifiedId & qUnqualified
               in foldl' (.&&.) (property True) $
                    [ appId === fst (head allApps),
                      appInfo.category === newApp.category,
                      (fst <$> allApps) === [appId],
                      ((.id) <$> appUser) === Just appId,
                      -- Just check that the token's there.  It is fake, so nothing interesting to find inside it.
                      property (either (const False) (const True) eNewCookieResult),
                      ((.id) <$> appUser') === Just appId,
                      (fst <$> allApps') === []
                    ]

            extractEmailEventTypes :: Map EmailAddress [SentMail] -> [String]
            extractEmailEventTypes emails = reverse eventTypes
              where
                eventTypes = xtract <$> fromJust (Map.lookup creatorEmail emails)

                xtract sentMail = case sentMail.content.aeEvent of
                  AppDeleted {} -> "deleted"
                  NewAppCreated {} -> "created"
                  AppMetadataChanged {} -> "metadata-changed"
                  AppTokenChanged {} -> "token-changed"
                  AppAvailabilityChanged {} ->
                    -- FUTUREWORK: https://wearezeta.atlassian.net/browse/WPB-25982
                    "availability-changed"

            expectedEmailEvents :: [String]
            expectedEmailEvents =
              ["created", "metadata-changed", "token-changed", "deleted"]
         in case result of
              Left err -> counterexample (show err) False
              Right (state, sentEmails) ->
                checkState state
                  .&&. (extractEmailEventTypes sentEmails `shouldBe` expectedEmailEvents)

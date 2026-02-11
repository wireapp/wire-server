-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.IdPSubsystem.InterpreterSpec (spec) where

import Control.Lens hiding (elements)
import Data.ByteString.Lazy.UTF8 qualified as BSUTF8
import Data.Default (def)
import Data.Handle
import Data.HavePendingInvitations
import Data.Id
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Qualified
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.State
import SAML2.WebSSO qualified as SAML
import System.Logger.Message qualified as Log
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.QuickCheck.Gen
import Wire.API.Team.Member
import Wire.API.User
import Wire.API.User.IdentityProvider
import Wire.BrigAPIAccess
import Wire.GalleyAPIAccess
import Wire.IdPConfigStore
import Wire.IdPConfigStore.Mem (idPToMem)
import Wire.IdPSubsystem
import Wire.IdPSubsystem.Interpreter
import Wire.MockInterpreters.GalleyAPIAccess
import Wire.Sem.Logger (Logger)
import Wire.Sem.Logger qualified
import Wire.Sem.Logger qualified as Log

type AllEffects =
  [ IdPSubsystem,
    Error IdPSubsystemError,
    Logger (Log.Msg -> Log.Msg),
    BrigAPIAccess,
    GalleyAPIAccess,
    IdPConfigStore,
    State [CapturedLog]
  ]

runAllEffects ::
  Bool ->
  [IdP] ->
  Map TeamId [TeamMember] ->
  ([UserId] -> [Handle] -> [EmailAddress] -> HavePendingInvitations -> [User]) ->
  Sem AllEffects a ->
  (Either IdPSubsystemError a, [CapturedLog])
runAllEffects enableDiscovery idps teams brigAPIMockFn action = swap $ run $ runState [] $ do
  (_idpState, result) <- idPToMem $ do
    -- Insert IdPs into store
    forM_ idps insertConfig
    -- Run the action
    miniGalleyAPIAccess teams def
      . brigAPIAccessMock brigAPIMockFn
      . captureLogger
      . runError
      . interpretIdPSubsystem enableDiscovery
      $ action
  pure result

-- Unfortunately, the implementation of `GetUsersByVariousKeys` is too complex
-- to cover it by a simple state. Thus, we resort to handwritten mock
-- functions. The integration tests provide certainty that
-- `GetUsersByVariousKeys` works for us as expected.
brigAPIAccessMock ::
  ([UserId] -> [Handle] -> [EmailAddress] -> HavePendingInvitations -> [User]) ->
  InterpreterFor BrigAPIAccess r
brigAPIAccessMock mockFn = interpret $ \case
  GetUsersByVariousKeys uids handles emails havePending -> pure $ mockFn uids handles emails havePending
  _ -> error "Not implemented in brigAPIAccessMock"

brigAPIAccessMockFn :: EmailAddress -> User -> [UserId] -> [Handle] -> [EmailAddress] -> HavePendingInvitations -> [User]
brigAPIAccessMockFn expectedEmail resUser [] [] emails NoPendingInvitations | emails == [expectedEmail] = [resUser]
brigAPIAccessMockFn expectedEmail _resUser [] [] emails NoPendingInvitations | emails /= [expectedEmail] = []
brigAPIAccessMockFn expectedEmail resUser uids handles emails havePending =
  error $
    "Exepected call "
      <> show (expectedEmail, resUser, uids, handles, emails, havePending)

type CapturedLog = (Log.Level, LByteString)

captureLogger ::
  (Member (State [CapturedLog]) r) =>
  InterpreterFor (Logger (Log.Msg -> Log.Msg)) r
captureLogger = interpret $ \case
  Wire.Sem.Logger.Log lvl msg -> modify ((lvl, renderMsg msg) :)

renderMsg :: (Log.Msg -> Log.Msg) -> LByteString
renderMsg = Log.render (Log.renderDefault ",")

spec :: Spec
spec = describe "IdPSubsystem.Interpreter" $ do
  describe "getSsoCodeByEmail" $ do
    prop "returns IdP for SCIM user with single IdP" $ \(teamMember :: TeamMember) user userRef email teamId mbDomain -> do
      idp <- generate $ do
        -- Generate IdPs with and without fitting domain
        idp' <- arbitrary <&> SAML.idpExtraInfo . domain .~ mbDomain
        more <- arbitrary
        elements $ idp' : more

      let userWithEmail =
            user
              { userIdentity = Just (SSOIdentity (UserSSOId userRef) (Just email)),
                userEmailUnvalidated = Just email,
                userTeam = Just teamId,
                userManagedBy = ManagedByScim
              }

          idpWithTeam = idp & SAML.idpExtraInfo . team .~ teamId

          teamMember' = teamMember & Wire.API.Team.Member.userId .~ (qUnqualified userWithEmail.userQualifiedId)

          teams = Map.singleton teamId [teamMember']

          (result, logs) =
            runAllEffects
              True
              [idpWithTeam]
              teams
              (brigAPIAccessMockFn email userWithEmail)
              (getSsoCodeByEmail mbDomain email)

      result `shouldBe` Right (Just idp._idpId)
      filter (\(lvl, _msg) -> lvl > Log.Info) logs `shouldBe` mempty

    prop "finds IdP for SCIM user by domain" $ \(teamMember :: TeamMember) user idp userRef email teamId dom -> do
      (otherIdPs :: NE.NonEmpty IdP) <-
        generate $
          arbitrary
            `suchThat` (not . any (\otherIdP -> Just dom == otherIdP._idpExtraInfo._domain))
      let userWithEmail =
            user
              { userIdentity = Just (SSOIdentity (UserSSOId userRef) (Just email)),
                userEmailUnvalidated = Just email,
                userTeam = Just teamId,
                userManagedBy = ManagedByScim
              }

          -- Set the target multi-ingress domain for one IdP
          patchedIdp = idp & SAML.idpExtraInfo . domain ?~ dom
          patchedIdpsHead = patchedIdp NE.<| otherIdPs
          idpsWithTeamHead = patchedIdpsHead <&> SAML.idpExtraInfo . team .~ teamId

          teamMember' = teamMember & Wire.API.Team.Member.userId .~ (qUnqualified userWithEmail.userQualifiedId)

          teams = Map.singleton teamId [teamMember']

          (resultHead, logsHead) =
            runAllEffects
              True
              (NE.toList idpsWithTeamHead)
              teams
              (brigAPIAccessMockFn email userWithEmail)
              (getSsoCodeByEmail (Just dom) email)

      resultHead `shouldBe` Right (Just idp._idpId)
      filter (\(lvl, _msg) -> lvl > Log.Info) logsHead `shouldBe` mempty

      -- Same game, but this time the IdP to find is at the end of the list.
      -- This ensures that the IdP is really looked up and not just taken from
      -- the head of the list.
      let patchedIdpsTail = NE.reverse $ patchedIdp NE.<| (NE.reverse otherIdPs)
          idpsWithTeamTail = patchedIdpsTail <&> SAML.idpExtraInfo . team .~ teamId

          (resultTail, logsTail) =
            runAllEffects
              True
              (NE.toList idpsWithTeamTail)
              teams
              (brigAPIAccessMockFn email userWithEmail)
              (getSsoCodeByEmail (Just dom) email)

      resultTail `shouldBe` Right (Just idp._idpId)
      filter (\(lvl, _msg) -> lvl > Log.Info) logsTail `shouldBe` mempty

    prop "returns any IdP if there are multiple" $ \(teamMember :: TeamMember) user userRef email teamId mbDomain -> do
      -- This should not happen, because the IdP management API allows to
      -- create only one IdP per domain. However, better not have undefined
      -- behaviour - Just in case...

      idps :: NE.NonEmpty IdP <- generate $ (resize 2 arbitrary) `suchThat` ((>= 2) . length)
      let userWithEmail =
            user
              { userIdentity = Just (SSOIdentity (UserSSOId userRef) (Just email)),
                userEmailUnvalidated = Just email,
                userTeam = Just teamId,
                userManagedBy = ManagedByScim
              }

          idpsWithTeam =
            idps
              <&> SAML.idpExtraInfo . domain .~ mbDomain
              <&> SAML.idpExtraInfo . team .~ teamId

          teamMember' = teamMember & Wire.API.Team.Member.userId .~ (qUnqualified userWithEmail.userQualifiedId)

          teams = Map.singleton teamId [teamMember']

          (result, logs) =
            runAllEffects
              True
              (NE.toList idpsWithTeam)
              teams
              (brigAPIAccessMockFn email userWithEmail)
              (getSsoCodeByEmail mbDomain email)

          expectedIdPIds :: [SAML.IdPId] = NE.toList $ ((SAML._idpId) <$> idpsWithTeam)

      result
        `shouldSatisfy` ( \case
                            Right (Just x) -> x `elem` expectedIdPIds
                            e -> error $ "Unexpected result " <> show e
                        )

      let warnLogs = filter (\(lvl, _msg) -> lvl > Log.Info) logs
      length warnLogs `shouldBe` 1
      (fst . head) warnLogs `shouldBe` Log.Warn
      (BSUTF8.toString . snd . head) warnLogs `shouldStartWith` "Found more than one IdP config for domain"

    prop "returns Nothing for an unknown email" $ \(teamMember :: TeamMember) user idp userRef email anotherEmail teamId mbDomain -> do
      let userWithEmail =
            user
              { userIdentity = Just (SSOIdentity (UserSSOId userRef) (Just email)),
                userEmailUnvalidated = Just email,
                userTeam = Just teamId,
                userManagedBy = ManagedByScim
              }

          idpWithTeam = idp & SAML.idpExtraInfo . team .~ teamId

          teamMember' = teamMember & Wire.API.Team.Member.userId .~ (qUnqualified userWithEmail.userQualifiedId)

          teams = Map.singleton teamId [teamMember']

          (result, logs) =
            runAllEffects
              True
              [idpWithTeam]
              teams
              (brigAPIAccessMockFn email userWithEmail)
              (getSsoCodeByEmail mbDomain anotherEmail)

      result `shouldBe` Right Nothing
      filter (\(lvl, _msg) -> lvl > Log.Info) logs `shouldBe` mempty

    prop "returns Nothing for SCIM user with no IdP" $ \(teamMember :: TeamMember) user userRef email teamId -> do
      let userWithEmail =
            user
              { userIdentity = Just (SSOIdentity (UserSSOId userRef) (Just email)),
                userEmailUnvalidated = Just email,
                userTeam = Just teamId,
                userManagedBy = ManagedByScim
              }

          teamMember' = teamMember & Wire.API.Team.Member.userId .~ (qUnqualified userWithEmail.userQualifiedId)

          teams = Map.singleton teamId [teamMember']

          (result, logs) =
            runAllEffects
              True
              mempty
              teams
              (brigAPIAccessMockFn email userWithEmail)
              (getSsoCodeByEmail Nothing email)

      result `shouldBe` Right Nothing
      filter (\(lvl, _msg) -> lvl > Log.Info) logs `shouldBe` mempty

    prop "returns Nothing when the user does not belong to a team" $ \user idp userRef email -> do
      let userWithEmail =
            user
              { userIdentity = Just (SSOIdentity (UserSSOId userRef) (Just email)),
                userEmailUnvalidated = Just email,
                userTeam = Nothing,
                userManagedBy = ManagedByScim
              }

          (result, logs) =
            runAllEffects
              True
              [idp]
              mempty
              (brigAPIAccessMockFn email userWithEmail)
              (getSsoCodeByEmail Nothing email)

      result `shouldBe` Right Nothing
      filter (\(lvl, _msg) -> lvl > Log.Info) logs `shouldBe` mempty

    prop "returns Nothing for non SCIM/SSO user" $ \(teamMember :: TeamMember) user idp userRef email teamId -> do
      (userIdentity, userManagedBy) <-
        generate $
          ( do
              ui <- Test.QuickCheck.Gen.elements [Just (SSOIdentity (UserSSOId userRef) (Just email)), Nothing]
              mngtBy :: ManagedBy <- arbitrary
              pure (ui, mngtBy)
          )
            `suchThat` (\(ui, mngtBy) -> isNothing ui || mngtBy == ManagedByWire)

      let userWithEmail =
            user
              { userIdentity = userIdentity,
                userEmailUnvalidated = Just email,
                userTeam = Just teamId,
                userManagedBy = userManagedBy
              }

          idpWithTeam = idp & SAML.idpExtraInfo . team .~ teamId

          teamMember' = teamMember & Wire.API.Team.Member.userId .~ (qUnqualified userWithEmail.userQualifiedId)

          teams = Map.singleton teamId [teamMember']

          (result, logs) =
            runAllEffects
              True
              [idpWithTeam]
              teams
              (brigAPIAccessMockFn email userWithEmail)
              (getSsoCodeByEmail Nothing email)

      result `shouldBe` Right Nothing
      filter (\(lvl, _msg) -> lvl > Log.Info) logs `shouldBe` mempty

    prop "returns Nothing when the feature is disabled" $ \(teamMember :: TeamMember) user idp userRef email teamId -> do
      let userWithEmail =
            user
              { userIdentity = Just (SSOIdentity (UserSSOId userRef) (Just email)),
                userEmailUnvalidated = Just email,
                userTeam = Just teamId,
                userManagedBy = ManagedByScim
              }

          idpWithTeam = idp & SAML.idpExtraInfo . team .~ teamId

          teamMember' = teamMember & Wire.API.Team.Member.userId .~ (qUnqualified userWithEmail.userQualifiedId)

          teams = Map.singleton teamId [teamMember']

          (result, logs) =
            runAllEffects
              False
              [idpWithTeam]
              teams
              (brigAPIAccessMockFn email userWithEmail)
              (getSsoCodeByEmail Nothing email)

      result `shouldBe` Right Nothing
      filter (\(lvl, _msg) -> lvl > Log.Info) logs `shouldBe` mempty

    prop "returns Nothing if there are multiple, but none for this domain" $ \(teamMember :: TeamMember) user userRef email teamId mbDomain -> do
      mbAnotherDomain <- generate $ arbitrary `suchThat` (\mbD -> mbD /= mbDomain)
      idps :: NE.NonEmpty IdP <- generate $ (resize 2 arbitrary) `suchThat` ((>= 2) . length)
      let userWithEmail =
            user
              { userIdentity = Just (SSOIdentity (UserSSOId userRef) (Just email)),
                userEmailUnvalidated = Just email,
                userTeam = Just teamId,
                userManagedBy = ManagedByScim
              }

          idpsWithTeam =
            idps
              <&> SAML.idpExtraInfo . domain .~ mbDomain
              <&> SAML.idpExtraInfo . team .~ teamId

          teamMember' = teamMember & Wire.API.Team.Member.userId .~ (qUnqualified userWithEmail.userQualifiedId)

          teams = Map.singleton teamId [teamMember']

          (result, logs) =
            runAllEffects
              True
              (NE.toList idpsWithTeam)
              teams
              (brigAPIAccessMockFn email userWithEmail)
              (getSsoCodeByEmail mbAnotherDomain email)

      result `shouldBe` Right Nothing
      filter (\(lvl, _msg) -> lvl > Log.Info) logs `shouldBe` mempty

    prop "getting multiple users for an email leads to exception" $ \(teamMember :: TeamMember) user anotherUser userRef email teamId mbDomain -> do
      idp' <- generate $ do
        idp'' <- arbitrary <&> SAML.idpExtraInfo . domain .~ mbDomain
        more <- arbitrary
        elements $ idp'' : more

      let userWithEmail =
            user
              { userIdentity = Just (SSOIdentity (UserSSOId userRef) (Just email)),
                userEmailUnvalidated = Just email,
                userTeam = Just teamId,
                userManagedBy = ManagedByScim
              }

          idpWithTeam = idp' & SAML.idpExtraInfo . team .~ teamId

          teamMember' = teamMember & Wire.API.Team.Member.userId .~ (qUnqualified userWithEmail.userQualifiedId)

          teams = Map.singleton teamId [teamMember']

          brigAPIAccessMockFnMultipleUser :: [UserId] -> [Handle] -> [EmailAddress] -> HavePendingInvitations -> [User]
          brigAPIAccessMockFnMultipleUser _userIds _handles _emails _pendingInvitations = [userWithEmail, anotherUser]

          (result, logs) =
            runAllEffects
              True
              [idpWithTeam]
              teams
              brigAPIAccessMockFnMultipleUser
              (getSsoCodeByEmail mbDomain email)

      result `shouldBe` Left InconsistentUsers
      let errorLogs = filter (\(lvl, _msg) -> lvl > Log.Info) logs
      length errorLogs `shouldBe` 1
      (fst . head) errorLogs `shouldBe` Log.Warn
      (BSUTF8.toString . snd . head) errorLogs `shouldStartWith` "Multiple users found for email address in getSsoCodeByEmail"

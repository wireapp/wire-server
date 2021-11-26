-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.Generator where

import Data.Id
import Imports
import System.IO (Handle, hPutStr, hPutStrLn, openFile)
import Test.Tasty.QuickCheck (Arbitrary (..), generate)
import Type.Reflection (typeRep)
import qualified Wire.API.Asset as Asset
import qualified Wire.API.Asset.V3.Resumable as Asset.Resumable
import qualified Wire.API.Call.Config as Call.Config
import qualified Wire.API.Connection as Connection
import qualified Wire.API.Conversation as Conversation
import qualified Wire.API.Conversation.Bot as Conversation.Bot
import qualified Wire.API.Conversation.Code as Conversation.Code
import qualified Wire.API.Conversation.Member as Conversation.Member
import qualified Wire.API.Conversation.Role as Conversation.Role
import qualified Wire.API.Conversation.Typing as Conversation.Typing
import qualified Wire.API.CustomBackend as CustomBackend
import qualified Wire.API.Event.Conversation as Event.Conversation
import qualified Wire.API.Event.Team as Event.Team
import qualified Wire.API.Message as Message
import qualified Wire.API.Notification as Notification
import qualified Wire.API.Properties as Properties
import qualified Wire.API.Provider as Provider
import qualified Wire.API.Provider.Bot as Provider.Bot
import qualified Wire.API.Provider.External as Provider.External
import qualified Wire.API.Provider.Service as Provider.Service
import qualified Wire.API.Provider.Service.Tag as Provider.Service.Tag
import qualified Wire.API.Push.Token as Push.Token
import qualified Wire.API.Team as Team
import qualified Wire.API.Team.Conversation as Team.Conversation
import qualified Wire.API.Team.Feature as Team.Feature
import qualified Wire.API.Team.Invitation as Team.Invitation
import qualified Wire.API.Team.LegalHold as Team.LegalHold
import qualified Wire.API.Team.LegalHold.External as Team.LegalHold.External
import qualified Wire.API.Team.Member as Team.Member
import qualified Wire.API.Team.Permission as Team.Permission
import qualified Wire.API.Team.Role as Team.Role
import qualified Wire.API.Team.SearchVisibility as Team.SearchVisibility
import qualified Wire.API.User as User
import qualified Wire.API.User.Activation as User.Activation
import qualified Wire.API.User.Auth as User.Auth
import qualified Wire.API.User.Client as User.Client
import qualified Wire.API.User.Client.Prekey as User.Client.Prekey
import qualified Wire.API.User.Handle as User.Handle
import qualified Wire.API.User.Identity as User.Identity
import qualified Wire.API.User.Password as User.Password
import qualified Wire.API.User.Profile as User.Profile
import qualified Wire.API.User.RichInfo as User.RichInfo
import qualified Wire.API.User.Search as User.Search
import qualified Wire.API.Wrapped as Wrapped

type Ref = IORef [(FilePath, [(String, FilePath)])]

-- NOTE: this will generate broken haskell code
--
-- To make the generated code compile, this needs to be run with
-- patched Show instances for certain types. Furthermore, a
-- substitution must be run on the generated code. This is done
-- automatically by the gentests.sh script.
generateBindingModule' ::
  forall a.
  (Arbitrary a, Show a) =>
  String ->
  String ->
  Ref ->
  IO ()
generateBindingModule' typeName section ref = do
  tmpdir <- getEnv "GOLDEN_TMPDIR"
  objects <- replicateM 20 (generate (arbitrary @a))
  let escape c
        | isAlphaNum c = [c]
        | ord c < 256 =
          [ '_',
            intToDigit (ord c `div` 16),
            intToDigit (ord c `mod` 16)
          ]
        | otherwise = ""
      moduleName = (typeName >>= escape) <> "_" <> section
      varName n = "testObject_" <> moduleName <> "_" <> show n
      fileName n = varName n <> ".json"
      numberedObjs = zip [1 :: Int ..] objects
      generateBinding h n o = do
        hPutStrLn h $ varName n <> " :: " <> typeName
        hPutStrLn h $ varName n <> " = " <> show o
      varNames = map (\(n, _) -> (varName n, fileName n)) numberedObjs
  h <- openFile (tmpdir <> "/" <> moduleName <> ".hs") WriteMode
  traverse_ (uncurry (generateBinding h)) numberedObjs
  modifyIORef ref (<> [(moduleName, varNames)])
  hClose h
  putStrLn (moduleName <> " " <> section)

generateBindingModule ::
  forall a.
  (Arbitrary a, Show a, Typeable a) =>
  String ->
  Ref ->
  IO ()
generateBindingModule = generateBindingModule' @a (show (typeRep @a))

generateImports :: Handle -> (FilePath, [(String, FilePath)]) -> IO ()
generateImports h (module_, _) = hPutStrLn h $ "import qualified Test.Wire.API.Golden.Generated." <> module_

generateTestCase :: Handle -> Int -> (FilePath, [(String, FilePath)]) -> IO ()
generateTestCase h index (module_, objs) = do
  hPutStr h "  "
  when (index > 0) $ hPutStr h ","
  hPutStrLn h $ " testCase (\"Golden: " <> module_ <> "\") $ "
  hPutStrLn h $ "   testObjects [" <> intercalate ", " (map objTuple objs) <> "]"
  where
    objTuple (var, path) = "(" <> "Test.Wire.API.Golden.Generated." <> module_ <> "." <> var <> ", " <> show path <> ")"

generateTestModule :: IO ()
generateTestModule = do
  ref <- newIORef mempty

  generateBindingModule @Asset.AssetToken "user" ref
  generateBindingModule @Asset.NewAssetToken "user" ref
  generateBindingModule @Asset.AssetRetention "user" ref
  generateBindingModule @Asset.AssetSettings "user" ref
  generateBindingModule @Asset.AssetKey "user" ref
  generateBindingModule @Asset.Resumable.ResumableSettings "user" ref
  generateBindingModule @Asset.Resumable.TotalSize "user" ref
  generateBindingModule @Asset.Resumable.ChunkSize "user" ref
  generateBindingModule @Asset.Resumable.Offset "user" ref
  generateBindingModule @Asset.Resumable.ResumableAsset "user" ref
  generateBindingModule @Call.Config.TurnHost "user" ref
  generateBindingModule @Call.Config.Scheme "user" ref
  generateBindingModule @Call.Config.Transport "user" ref
  generateBindingModule @Call.Config.TurnURI "user" ref
  generateBindingModule @Call.Config.TurnUsername "user" ref
  generateBindingModule @Call.Config.RTCIceServer "user" ref
  generateBindingModule @Call.Config.RTCConfiguration "user" ref
  generateBindingModule @Call.Config.SFTServer "user" ref
  generateBindingModule @Connection.ConnectionRequest "user" ref
  generateBindingModule @Connection.Relation "user" ref
  generateBindingModule @Connection.UserConnection "user" ref
  generateBindingModule @Connection.UserConnectionList "user" ref
  generateBindingModule @Connection.ConnectionUpdate "user" ref
  generateBindingModule @Conversation.Conversation "user" ref
  generateBindingModule @Conversation.NewConvUnmanaged "user" ref
  generateBindingModule @Conversation.NewConvManaged "user" ref
  generateBindingModule @(Conversation.ConversationList ConvId) "user" ref
  generateBindingModule @(Conversation.ConversationList Conversation.Conversation) "user" ref
  generateBindingModule @Conversation.Access "user" ref
  generateBindingModule @Conversation.AccessRole "user" ref
  generateBindingModule @Conversation.ConvType "user" ref
  generateBindingModule @Conversation.ReceiptMode "user" ref
  generateBindingModule @Conversation.ConvTeamInfo "user" ref
  generateBindingModule @Conversation.Invite "user" ref
  generateBindingModule @Conversation.ConversationRename "user" ref
  generateBindingModule @Conversation.ConversationAccessData "user" ref
  generateBindingModule @Conversation.ConversationReceiptModeUpdate "user" ref
  generateBindingModule @Conversation.ConversationMessageTimerUpdate "user" ref
  generateBindingModule @Conversation.Bot.AddBot "user" ref
  generateBindingModule @Conversation.Bot.AddBotResponse "user" ref
  generateBindingModule @Conversation.Bot.RemoveBotResponse "user" ref
  generateBindingModule @Conversation.Bot.UpdateBotPrekeys "user" ref
  generateBindingModule @Conversation.Code.ConversationCode "user" ref
  generateBindingModule @Conversation.Member.MemberUpdate "user" ref
  generateBindingModule @Conversation.Member.MutedStatus "user" ref
  generateBindingModule @Conversation.Member.Member "user" ref
  generateBindingModule @Conversation.Member.OtherMember "user" ref
  generateBindingModule @Conversation.Member.ConvMembers "user" ref
  generateBindingModule @Conversation.Member.OtherMemberUpdate "user" ref
  generateBindingModule @Conversation.Role.RoleName "user" ref
  generateBindingModule @Conversation.Role.Action "user" ref
  generateBindingModule @Conversation.Role.ConversationRole "user" ref
  generateBindingModule @Conversation.Role.ConversationRolesList "user" ref
  generateBindingModule @Conversation.Typing.TypingStatus "user" ref
  generateBindingModule @Conversation.Typing.TypingData "user" ref
  generateBindingModule @CustomBackend.CustomBackend "user" ref
  generateBindingModule @Event.Conversation.Event "user" ref
  generateBindingModule @Event.Conversation.EventType "user" ref
  generateBindingModule @Event.Conversation.SimpleMember "user" ref
  generateBindingModule @Event.Conversation.SimpleMembers "user" ref
  generateBindingModule @Event.Conversation.Connect "user" ref
  generateBindingModule @Event.Conversation.MemberUpdateData "user" ref
  generateBindingModule @Event.Conversation.OtrMessage "user" ref
  generateBindingModule @Message.Priority "user" ref
  generateBindingModule @Message.OtrRecipients "user" ref
  generateBindingModule @Message.NewOtrMessage "user" ref
  generateBindingModule @Message.ClientMismatch "user" ref
  generateBindingModule @Notification.QueuedNotification "user" ref
  generateBindingModule @Notification.QueuedNotificationList "user" ref
  generateBindingModule @Properties.PropertyKey "user" ref
  generateBindingModule @Properties.PropertyValue "user" ref
  generateBindingModule' @Push.Token.Transport "Push.Token.Transport" "user" ref
  generateBindingModule @Push.Token.Token "user" ref
  generateBindingModule @Push.Token.AppName "user" ref
  generateBindingModule @Push.Token.PushToken "user" ref
  generateBindingModule @Push.Token.PushTokenList "user" ref
  generateBindingModule @User.NameUpdate "user" ref
  generateBindingModule @User.NewUser "user" ref
  generateBindingModule @User.NewUserPublic "user" ref
  generateBindingModule @User.UserIdList "user" ref
  generateBindingModule @(User.LimitedQualifiedUserIdList 20) "user" ref
  generateBindingModule @User.UserProfile "user" ref
  generateBindingModule @User.User "user" ref
  generateBindingModule @User.SelfProfile "user" ref
  generateBindingModule @User.InvitationCode "user" ref
  generateBindingModule @User.BindingNewTeamUser "user" ref
  generateBindingModule @User.UserUpdate "user" ref
  generateBindingModule @User.PasswordChange "user" ref
  generateBindingModule @User.LocaleUpdate "user" ref
  generateBindingModule @User.EmailUpdate "user" ref
  generateBindingModule @User.PhoneUpdate "user" ref
  generateBindingModule @User.HandleUpdate "user" ref
  generateBindingModule @User.DeleteUser "user" ref
  generateBindingModule @User.VerifyDeleteUser "user" ref
  generateBindingModule @User.DeletionCodeTimeout "user" ref
  generateBindingModule @User.Activation.ActivationKey "user" ref
  generateBindingModule @User.Activation.ActivationCode "user" ref
  generateBindingModule @User.Activation.Activate "user" ref
  generateBindingModule @User.Activation.ActivationResponse "user" ref
  generateBindingModule @User.Activation.SendActivationCode "user" ref
  generateBindingModule @User.Auth.LoginId "user" ref
  generateBindingModule @User.Auth.LoginCode "user" ref
  generateBindingModule @User.Auth.PendingLoginCode "user" ref
  generateBindingModule @User.Auth.SendLoginCode "user" ref
  generateBindingModule @User.Auth.LoginCodeTimeout "user" ref
  generateBindingModule @User.Auth.CookieLabel "user" ref
  generateBindingModule @User.Auth.Login "user" ref
  generateBindingModule @User.Auth.CookieId "user" ref
  generateBindingModule @User.Auth.CookieType "user" ref
  generateBindingModule @(User.Auth.Cookie ()) "user" ref
  generateBindingModule @User.Auth.CookieList "user" ref
  generateBindingModule @User.Auth.RemoveCookies "user" ref
  generateBindingModule @User.Auth.TokenType "user" ref
  generateBindingModule @User.Auth.AccessToken "user" ref
  generateBindingModule @(User.Client.UserClientMap Int) "user" ref
  generateBindingModule @User.Client.UserClients "user" ref
  generateBindingModule @User.Client.ClientType "user" ref
  generateBindingModule @User.Client.ClientClass "user" ref
  generateBindingModule @User.Client.PubClient "user" ref
  generateBindingModule @User.Client.Client "user" ref
  generateBindingModule @User.Client.NewClient "user" ref
  generateBindingModule @User.Client.UpdateClient "user" ref
  generateBindingModule @User.Client.RmClient "user" ref
  generateBindingModule @User.Client.Prekey.LastPrekey "user" ref
  generateBindingModule @User.Client.Prekey.PrekeyId "user" ref
  generateBindingModule @User.Client.Prekey.Prekey "user" ref
  generateBindingModule @User.Client.Prekey.ClientPrekey "user" ref
  generateBindingModule @User.Client.Prekey.PrekeyBundle "user" ref
  generateBindingModule @User.Handle.UserHandleInfo "user" ref
  generateBindingModule @User.Handle.CheckHandles "user" ref
  generateBindingModule @User.Identity.Email "user" ref
  generateBindingModule @User.Identity.Phone "user" ref
  generateBindingModule @User.Identity.UserSSOId "user" ref
  generateBindingModule @User.Identity.UserIdentity "user" ref
  generateBindingModule @User.Password.NewPasswordReset "user" ref
  generateBindingModule @User.Password.PasswordResetKey "user" ref
  generateBindingModule @User.Password.PasswordResetCode "user" ref
  generateBindingModule @User.Password.CompletePasswordReset "user" ref
  generateBindingModule @User.Profile.Pict "user" ref
  generateBindingModule @User.Profile.Name "user" ref
  generateBindingModule @User.Profile.ColourId "user" ref
  generateBindingModule @User.Profile.AssetSize "user" ref
  generateBindingModule' @User.Profile.Asset "User.Profile.Asset" "user" ref
  generateBindingModule @User.Profile.Locale "user" ref
  generateBindingModule @User.Profile.ManagedBy "user" ref
  generateBindingModule @User.RichInfo.RichField "user" ref
  generateBindingModule @User.RichInfo.RichInfoAssocList "user" ref
  generateBindingModule @User.RichInfo.RichInfoMapAndList "user" ref
  generateBindingModule @User.RichInfo.RichInfo "user" ref
  generateBindingModule @(User.Search.SearchResult User.Search.Contact) "user" ref
  generateBindingModule @User.Search.Contact "user" ref
  generateBindingModule @(User.Search.SearchResult User.Search.TeamContact) "user" ref
  generateBindingModule @User.Search.TeamContact "user" ref
  generateBindingModule @(Wrapped.Wrapped "some_int" Int) "user" ref
  generateBindingModule @Asset.Asset "asset" ref
  generateBindingModule @Event.Team.Event "team" ref
  generateBindingModule @Event.Team.EventType "team" ref
  generateBindingModule @Provider.Provider "provider" ref
  generateBindingModule @Provider.ProviderProfile "provider" ref
  generateBindingModule @Provider.NewProvider "provider" ref
  generateBindingModule @Provider.NewProviderResponse "provider" ref
  generateBindingModule @Provider.UpdateProvider "provider" ref
  generateBindingModule @Provider.ProviderActivationResponse "provider" ref
  generateBindingModule @Provider.ProviderLogin "provider" ref
  generateBindingModule @Provider.DeleteProvider "provider" ref
  generateBindingModule @Provider.PasswordReset "provider" ref
  generateBindingModule @Provider.CompletePasswordReset "provider" ref
  generateBindingModule @Provider.PasswordChange "provider" ref
  generateBindingModule @Provider.EmailUpdate "provider" ref
  generateBindingModule @Provider.Bot.BotConvView "provider" ref
  generateBindingModule @Provider.Bot.BotUserView "provider" ref
  generateBindingModule @Provider.External.NewBotRequest "provider" ref
  generateBindingModule @Provider.External.NewBotResponse "provider" ref
  generateBindingModule @Provider.Service.ServiceRef "provider" ref
  generateBindingModule @Provider.Service.ServiceKeyPEM "provider" ref
  generateBindingModule @Provider.Service.ServiceKeyType "provider" ref
  generateBindingModule @Provider.Service.ServiceKey "provider" ref
  generateBindingModule @Provider.Service.ServiceToken "provider" ref
  generateBindingModule @Provider.Service.Service "provider" ref
  generateBindingModule @Provider.Service.ServiceProfile "provider" ref
  generateBindingModule @Provider.Service.ServiceProfilePage "provider" ref
  generateBindingModule @Provider.Service.NewService "provider" ref
  generateBindingModule @Provider.Service.NewServiceResponse "provider" ref
  generateBindingModule @Provider.Service.UpdateService "provider" ref
  generateBindingModule @Provider.Service.UpdateServiceConn "provider" ref
  generateBindingModule @Provider.Service.DeleteService "provider" ref
  generateBindingModule @Provider.Service.UpdateServiceWhitelist "provider" ref
  generateBindingModule @Provider.Service.Tag.ServiceTag "provider" ref
  generateBindingModule @Provider.Service.Tag.ServiceTagList "provider" ref
  generateBindingModule @Team.BindingNewTeam "team" ref
  generateBindingModule @Team.TeamBinding "team" ref
  generateBindingModule @Team.Team "team" ref
  generateBindingModule @Team.TeamList "team" ref
  generateBindingModule @Team.TeamUpdateData "team" ref
  generateBindingModule @Team.TeamDeleteData "team" ref
  generateBindingModule @Team.Conversation.TeamConversation "team" ref
  generateBindingModule @Team.Conversation.TeamConversationList "team" ref
  generateBindingModule @(Team.Feature.TeamFeatureStatus 'Team.Feature.TeamFeatureLegalHold) "team" ref
  generateBindingModule @(Team.Feature.TeamFeatureStatus 'Team.Feature.TeamFeatureAppLock) "team" ref
  generateBindingModule @Team.Feature.TeamFeatureStatusValue "team" ref
  generateBindingModule @Team.Invitation.InvitationRequest "team" ref
  generateBindingModule @Team.Invitation.Invitation "team" ref
  generateBindingModule @Team.Invitation.InvitationList "team" ref
  generateBindingModule @Team.LegalHold.NewLegalHoldService "team" ref
  generateBindingModule @Team.LegalHold.ViewLegalHoldServiceInfo "team" ref
  generateBindingModule @Team.LegalHold.ViewLegalHoldService "team" ref
  generateBindingModule @Team.LegalHold.UserLegalHoldStatusResponse "team" ref
  generateBindingModule @Team.LegalHold.RemoveLegalHoldSettingsRequest "team" ref
  generateBindingModule @Team.LegalHold.DisableLegalHoldForUserRequest "team" ref
  generateBindingModule @Team.LegalHold.ApproveLegalHoldForUserRequest "team" ref
  generateBindingModule @Team.LegalHold.External.RequestNewLegalHoldClient "team" ref
  generateBindingModule @Team.LegalHold.External.NewLegalHoldClient "team" ref
  generateBindingModule @Team.LegalHold.External.LegalHoldServiceConfirm "team" ref
  generateBindingModule @Team.LegalHold.External.LegalHoldServiceRemove "team" ref
  generateBindingModule @Team.Member.TeamMember "team" ref
  generateBindingModule @Team.Member.ListType "team" ref
  generateBindingModule @Team.Member.TeamMemberList "team" ref
  generateBindingModule @Team.Member.NewTeamMember "team" ref
  generateBindingModule @Team.Member.TeamMemberDeleteData "team" ref
  generateBindingModule @Team.Permission.Permissions "team" ref
  generateBindingModule @Team.Role.Role "team" ref
  generateBindingModule @Team.SearchVisibility.TeamSearchVisibility "team" ref
  generateBindingModule @Team.SearchVisibility.TeamSearchVisibilityView "team" ref

  bindings <- readIORef ref

  testmain <- getEnv "GOLDEN_TESTDIR"
  h <- openFile (testmain <> ".hs") WriteMode
  hPutStrLn h "module Test.Wire.API.Golden.Generated where\n"
  hPutStrLn h "import Imports"
  hPutStrLn h "import Test.Wire.API.Golden.Runner"
  hPutStrLn h "import Test.Tasty"
  hPutStrLn h "import Test.Tasty.HUnit"
  traverse_ (generateImports h) bindings
  hPutStrLn h "tests :: TestTree"
  hPutStrLn h "tests = testGroup \"Golden tests\" ["
  traverse_ (uncurry (generateTestCase h)) (zip [0 :: Int ..] bindings)
  hPutStrLn h "  ]"
  hClose h

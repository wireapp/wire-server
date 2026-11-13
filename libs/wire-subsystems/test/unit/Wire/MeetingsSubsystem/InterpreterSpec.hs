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

module Wire.MeetingsSubsystem.InterpreterSpec (spec) where

import Control.Exception (throwIO)
import Data.Aeson (Result (..), Value (Object), fromJSON)
import Data.ByteString.Char8 qualified as C
import Data.Code (Key, Timeout (..))
import Data.Default (def)
import Data.Domain (Domain (..))
import Data.IP (IP)
import Data.Id
import Data.LanguageCodes (ISO639_1 (DE))
import Data.LegalHold (UserLegalHoldStatus (..))
import Data.Map qualified as Map
import Data.Misc (HttpsUrl (..), IpAddr (..), plainTextPassword8Unsafe)
import Data.Qualified
import Data.Range (Range, checked, unsafeRange)
import Data.Set qualified as Set
import Data.Tagged (Tagged)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock
import Data.UUID (nil)
import Data.Vector qualified as V
import Imports
import Polysemy
import Polysemy.Async (Async, asyncToIOFinal)
import Polysemy.Error
import Polysemy.Input
import Polysemy.State
import Polysemy.TinyLog (TinyLog)
import System.IO.Error (userError)
import System.Random (StdGen, mkStdGen)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonNegative, arbitrary, counterexample, generate, getNonNegative, ioProperty, (.&&.), (===), (==>))
import URI.ByteString (parseURI, strictURIParserOptions)
import Wire.API.Conversation (Access (CodeAccess, InviteAccess), AccessRole (GuestAccessRole), Conversation (metadata, qualifiedId), ConversationMetadata (cnvmAccess, cnvmAccessRoles), defRole)
import Wire.API.Conversation.Role (pattern DeleteConversation)
import Wire.API.Error (ErrorS)
import Wire.API.Error.Galley (GalleyError (TeamMemberNotFound, TeamNotFound), InvalidTimesReason (..), MeetingError (..), pattern ActionDenied)
import Wire.API.Event.Meeting qualified as MeetingEvent
import Wire.API.Meeting qualified as API
import Wire.API.Password (Password)
import Wire.API.PostgresMarshall (PostgresUnmarshall (postgresUnmarshall))
import Wire.API.Push.V2 qualified as PushV2
import Wire.API.Routes.Internal.Brig (MeetingEmailKind (..), SendMeetingEmails (..))
import Wire.API.Team.Feature
import Wire.API.Team.Member (TeamMember, mkTeamMember)
import Wire.API.Team.Permission (fullPermissions)
import Wire.API.User
import Wire.BrigAPIAccess
import Wire.BrigAPIAccess qualified as BrigAPIAccess
import Wire.CodeStore (CodeStore)
import Wire.CodeStore qualified as CodeStore
import Wire.CodeStore.Code (Code (..), CodeReferent (..))
import Wire.ConversationSubsystem
import Wire.FeaturesConfigSubsystem
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.HashPassword (HashPassword, verifyPassword)
import Wire.Licensing.Effect (Licensing)
import Wire.Licensing.License (License (..), Meetings (..), fullLicense, missingLicense)
import Wire.Licensing.Mock (runLicenseMocked)
import Wire.MeetingNotifier (MeetingNotifier, notifyMeetingEvent)
import Wire.MeetingNotifier.Interpreter (interpretMeetingNotifier)
import Wire.MeetingsStore qualified as Store
import Wire.MeetingsSubsystem
import Wire.MeetingsSubsystem.Interpreter
import Wire.MockInterpreters
import Wire.NotificationSubsystem (NotificationSubsystem, Push (..), Recipient (recipientUserId))
import Wire.ParseException (ParseException (..))
import Wire.RateLimit (RateLimit, RateLimitExceeded, RateLimitKey (RateLimitUser))
import Wire.RpcException (RpcException (..))
import Wire.Sem.Logger.TinyLog (discardTinyLogs)
import Wire.Sem.Now (Now)
import Wire.Sem.Random (Random)
import Wire.StoredConversation
import Wire.TeamSubsystem (TeamSubsystem)
import Wire.TeamSubsystem.GalleyAPI

type TestStack =
  '[ MeetingsSubsystem,
     Async,
     BrigAPIAccess,
     Error ParseException,
     Error RpcException,
     Licensing,
     RateLimit,
     HashPassword,
     Error RateLimitExceeded,
     MeetingNotifier,
     Store.MeetingsStore,
     ConversationSubsystem,
     TeamSubsystem,
     FeaturesConfigSubsystem,
     NotificationSubsystem,
     TinyLog,
     Error MeetingError,
     ErrorS ('ActionDenied 'DeleteConversation),
     State (Map ConvId (Set UserId)),
     State (Map ConvId StoredConversation),
     State (Map MeetingId Store.StoredMeeting),
     State [Push],
     GalleyAPIAccess,
     Now,
     State UTCTime,
     Input (Local ()),
     Random,
     State StdGen,
     ErrorS 'TeamMemberNotFound,
     ErrorS 'TeamNotFound,
     CodeStore,
     State (Map Key (Code, Maybe Password)),
     Input (Either HttpsUrl (Map Domain HttpsUrl)),
     State (Map UserId User),
     State Bool,
     State FailSendMeetingEmails,
     State FailSendMeetingEmailsIO,
     State (IORef [SendMeetingEmails]),
     Embed IO,
     Final IO
   ]

testCodeURIBase :: HttpsUrl
testCodeURIBase = HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://code.example/conversation-join/"))

-- | The expected 'API.link' value for a stored code: the base rewritten by
-- 'API.mkMeetingLink' with the code's key and (rotating) value.
expectedLink :: HttpsUrl -> Domain -> Code -> HttpsUrl
expectedLink base dom c = API.mkMeetingLink base dom c.codeKey c.codeValue

interpretFeaturesConfigSubsystemPure :: AllTeamFeatures -> InterpreterFor FeaturesConfigSubsystem r
interpretFeaturesConfigSubsystemPure configs = interpret $ \case
  GetDbFeatureRawInternal _tid -> pure def
  GetFeature _uid _tid -> pure def
  GetFeatureForTeam _tid -> pure $ npProject configs
  GetFeatureForServer -> pure def
  GetFeatureForTeamUser _uid _mTid -> pure def
  GetAllTeamFeaturesForTeamMember _luid _tid -> pure def
  GetAllTeamFeaturesForTeam _tid -> pure def
  GetAllTeamFeaturesForServer -> pure def
  GuardSecondFactorDisabled _ _ -> error "not implemented"
  FeatureEnabledForTeam _ _ -> error "not implemented"
  GetAllTeamFeaturesForUser _ -> error "not implemented"
  GetSingleFeatureForUser _ -> error "not implemented"
  GetFeatureInternal _ -> error "not implemented"

runTestStack ::
  UTCTime ->
  StdGen ->
  Map TeamId [TeamMember] ->
  AllTeamFeatures ->
  Sem TestStack a ->
  IO (Either MeetingError a)
runTestStack now gen teams configs sem = do
  result <- runTestStackWithURI now gen (Left testCodeURIBase) teams configs fullLicense sem
  case result of
    Left _denied -> error "unexpected CheckDeleteLocalConversation denial"
    Right meetingResult -> pure meetingResult

-- | Like 'runTestStack', but surfaces a denied @CheckDeleteLocalConversation@
-- as a typed @Left@ instead of crashing, so tests can exercise the
-- conversation-deletion preflight failure path (WPB-29046).
runTestStackWithURI ::
  UTCTime ->
  StdGen ->
  Either HttpsUrl (Map Domain HttpsUrl) ->
  Map TeamId [TeamMember] ->
  AllTeamFeatures ->
  License ->
  Sem TestStack a ->
  IO (Either (Tagged ('ActionDenied 'DeleteConversation) ()) (Either MeetingError a))
runTestStackWithURI now gen convCodeURI teams configs lic =
  runTestStackWithURICfg
    now
    gen
    convCodeURI
    teams
    configs
    lic
    MeetingSystemConfig
      { legacyTimeZone = API.defaultLegacyTimeZone,
        validityPeriod = 3600,
        pastEditPeriod = configuredPastEditPeriod,
        emailsEnabled = True
      }

-- | 'runTestStackWithURI' with an explicit 'MeetingSystemConfig', for tests
-- that exercise alternative configuration (e.g. meeting emails disabled).
runTestStackWithURICfg ::
  UTCTime ->
  StdGen ->
  Either HttpsUrl (Map Domain HttpsUrl) ->
  Map TeamId [TeamMember] ->
  AllTeamFeatures ->
  License ->
  MeetingSystemConfig ->
  Sem TestStack a ->
  IO (Either (Tagged ('ActionDenied 'DeleteConversation) ()) (Either MeetingError a))
runTestStackWithURICfg now gen convCodeURI teams configs lic cfg sem = do
  rpcRef <- newIORef []
  runFinal @IO
    . embedToFinal @IO
    . evalState rpcRef
    . evalState (FailSendMeetingEmailsIO False)
    . evalState (FailSendMeetingEmails False)
    . evalState False
    . evalState (Map.empty :: Map UserId User)
    . fmap (either (error . show) (either (error . show) Imports.id))
    . runInputConst convCodeURI
    . evalState (Map.empty :: Map Key (Code, Maybe Password))
    . interpretCodeStorePure
    . runError @(Tagged 'TeamNotFound ())
    . runError @(Tagged 'TeamMemberNotFound ())
    . evalState gen
    . randomToStatefulStdGen
    . runInputConst (toLocalUnsafe (Domain "my-domain") ())
    . evalState now
    . interpretNowAsState
    . miniGalleyAPIAccess teams configs
    . evalState ([] :: [Push])
    . evalState Map.empty
    . evalState Map.empty
    . evalState Map.empty
    . runError @(Tagged ('ActionDenied 'DeleteConversation) ())
    . runError @MeetingError
    . discardTinyLogs
    . inMemoryNotificationSubsystemInterpreter
    . interpretFeaturesConfigSubsystemPure configs
    . interpretTeamSubsystemToGalleyAPI
    . inMemoryConversationSubsystemInterpreter
    . inMemoryMeetingsStoreInterpreter
    . interpretMeetingNotifier
    . runErrorUnsafe @RateLimitExceeded
    . staticHashPasswordInterpreter
    . noRateLimit
    . runLicenseMocked lic
    -- All pure error eliminators sit outside 'asyncToIOFinal', so a typed
    -- error thrown inside a spawned child collapses to 'Nothing' instead of
    -- propagating (same topology as Galley.App).
    . runErrorUnsafe @RpcException
    . runErrorUnsafe @ParseException
    . mockBrigAPIAccess
    . asyncToIOFinal
    . interpretMeetingsSubsystem cfg
    $ sem

-- | An update that sets no fields (rejected with 'EmptyUpdate'); use record
-- update on it to set individual fields.
emptyUpdate :: API.UpdateMeeting
emptyUpdate = API.UpdateMeeting Nothing Nothing Nothing Nothing Nothing

-- | Decode all 'Push' payloads that are meeting lifecycle events. Any push that
-- decodes as a 'MeetingEvent.Event' is one: conversation events use distinct
-- @type@ tags that the meeting 'EventType' enum rejects.
extractMeetingEvents :: [Push] -> [MeetingEvent.Event]
extractMeetingEvents pushes =
  [e | push <- pushes, Success e <- [fromJSON (Object push.json)]]

-- | An update that only sets the meeting title (email tests).
titleUpdate :: Range 1 256 Text -> API.UpdateMeeting
titleUpdate t = API.UpdateMeeting Nothing Nothing (Just t) Nothing Nothing

-- | Brig mock that only answers 'GetUsers' (used for meeting-email host
-- resolution) and 'SendMeetingEmails' (recorded into an 'IORef', since the
-- dispatch runs in a detached async child whose pure state writes are lost).
-- Every other operation is a programming error. The 'Bool' knob makes
-- 'GetUsers' throw an untyped 'RpcException' IO exception, exactly the way
-- "Wire.Rpc" surfaces a failing brig call (@runHttpRpc@ runs via 'embed',
-- so non-2xx responses and transport failures are IO exceptions);
-- the 'FailSendMeetingEmails' knob does the same for 'SendMeetingEmails'
-- (typed, as the real brig interpreter raises it). The 'FailSendMeetingEmails'
-- knob must be set before the operation that forks the child: the child sees
-- a snapshot of the pure state at fork time; the 'GetUsers' knob is read
-- synchronously, so timing does not matter for it.
mockBrigAPIAccess ::
  ( Member (State (Map UserId User)) r,
    Member (State Bool) r,
    Member (State FailSendMeetingEmails) r,
    Member (State FailSendMeetingEmailsIO) r,
    Member (State (IORef [SendMeetingEmails])) r,
    Member (Embed IO) r,
    Member (Error RpcException) r
  ) =>
  InterpreterFor BrigAPIAccess r
mockBrigAPIAccess = interpret \case
  BrigAPIAccess.GetUsers uids -> do
    failing <- get @Bool
    when failing . embed $
      throwIO (RpcExceptionInternal {service = "brig", status = 502, message = "mock: host profile lookup failed"})
    gets (Map.elems . (`Map.restrictKeys` Set.fromList uids))
  BrigAPIAccess.SendMeetingEmails payload -> do
    FailSendMeetingEmails failing <- get @FailSendMeetingEmails
    when failing $
      throw (RpcExceptionInternal {service = "brig", status = 502, message = "mock: send meeting emails failed"})
    FailSendMeetingEmailsIO failingIO <- get @FailSendMeetingEmailsIO
    when failingIO . void . embed $
      throwIO (userError "mock: brig unreachable")
    ref <- get @(IORef [SendMeetingEmails])
    embed $ atomicModifyIORef' ref (\ps -> (payload : ps, ()))
  _ -> error "mockBrigAPIAccess: unimplemented operation"

-- | Set via 'State FailSendMeetingEmails'; makes the mock's
-- 'SendMeetingEmails' handler throw an 'RpcException' (failing brig RPC).
newtype FailSendMeetingEmails = FailSendMeetingEmails Bool
  deriving stock (Eq, Show)

-- | Set via 'State FailSendMeetingEmailsIO'; makes the mock's
-- 'SendMeetingEmails' handler throw a raw IO exception (brig entirely
-- unreachable). Must be set before the operation that forks the child: the
-- child sees a snapshot of the pure state at fork time.
newtype FailSendMeetingEmailsIO = FailSendMeetingEmailsIO Bool
  deriving stock (Eq, Show)

-- | A brig user record for the meeting host (creator) with the given name,
-- email and locale, everything else generated.
mkHostUser :: UserId -> Text -> Maybe EmailAddress -> Locale -> IO User
mkHostUser uid name email loc = do
  base <- generate arbitrary
  pure
    base
      { userQualifiedId = Qualified uid (Domain "wire.com"),
        userDisplayName = Name name,
        -- Model a verified email: 'meetingHostProfile' reads 'userEmail'
        -- (identity-derived), not 'userEmailUnvalidated'.
        userEmailUnvalidated = email,
        userIdentity = EmailIdentity <$> email,
        userLocale = loc
      }

spec :: Spec
spec = describe "MeetingsSubsystem.Interpreter" $ do
  it "creates a meeting and can retrieve it" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Test Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              tzid = API.defaultLegacyTimeZone,
              mtype = API.Scheduled,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              password = Nothing,
              recurrence = Nothing,
              invitedEmails = []
            }

    result <- runTestStack now gen Map.empty def $ do
      meeting <- createMeeting zUser (Just (ConnId "test-conn")) Nothing newMeeting
      fetched <- getMeeting zUser meeting.meeting.id
      pure (meeting, fetched)

    case result of
      Left err -> fail $ "Error: " <> show err
      Right (meeting, fetched) -> do
        meeting.meeting.title `shouldBe` fromJust (checked "Test Meeting")
        meeting.conversation.qualifiedId `shouldBe` meeting.meeting.conversationId
        fetched `shouldBe` Just meeting.meeting

  it "returns a join link and stores a join code for a new meeting" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Linked Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              tzid = API.defaultLegacyTimeZone,
              mtype = API.Scheduled,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              password = Nothing,
              recurrence = Nothing,
              invitedEmails = []
            }

    result <- runTestStack now gen Map.empty def $ do
      meeting <- createMeeting zUser (Just (ConnId "test-conn")) Nothing newMeeting
      let mid = qUnqualified meeting.meeting.id
      key <- CodeStore.makeKey (CodeReferentMeeting mid)
      mCode <- gets @(Map Key (Code, Maybe Password)) (Map.lookup key)
      pure (meeting, mid, mCode)

    case result of
      Left err -> fail $ "Error: " <> show err
      Right (meeting, _mid, mCode) -> do
        code <- maybe (fail "expected join code") (pure . fst) mCode
        meeting.meeting.link `shouldBe` expectedLink testCodeURIBase (Domain "wire.com") code
        meeting.meeting.hasPassword `shouldBe` False

  it "uses the multi-ingress URI mapped for the user's domain" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        multiBase = HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://multi.example.com/conversation-join/"))
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Multi Ingress Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              tzid = API.defaultLegacyTimeZone,
              mtype = API.Scheduled,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              password = Nothing,
              recurrence = Nothing,
              invitedEmails = []
            }

    result <-
      runTestStackWithURI
        now
        gen
        (Right (Map.fromList [(Domain "wire.com", multiBase)]))
        Map.empty
        def
        fullLicense
        ( do
            meeting <- createMeeting zUser (Just (ConnId "test-conn")) Nothing newMeeting
            let mid = qUnqualified meeting.meeting.id
            key <- CodeStore.makeKey (CodeReferentMeeting mid)
            mCode <- gets @(Map Key (Code, Maybe Password)) (Map.lookup key)
            pure (meeting, mid, mCode)
        )
        >>= either (error . show) pure

    case result of
      Left err -> fail $ "Error: " <> show err
      Right (meeting, _mid, mCode) -> do
        code <- maybe (fail "expected join code") (pure . fst) mCode
        meeting.meeting.link `shouldBe` expectedLink multiBase (Domain "wire.com") code

  it "fails with LinkUnavailable when the user's domain has no configured URI" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        otherBase = HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://other.example.com/conversation-join/"))
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Unmapped Domain Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              tzid = API.defaultLegacyTimeZone,
              mtype = API.Scheduled,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              password = Nothing,
              recurrence = Nothing,
              invitedEmails = []
            }

    result <-
      runTestStackWithURI
        now
        gen
        (Right (Map.fromList [(Domain "other.example.com", otherBase)]))
        Map.empty
        def
        fullLicense
        ( do
            -- Catch inside so the post-state (conversation store) remains
            -- observable: proves the link precondition runs before any
            -- committed side effect (no orphaned conversation).
            r <- catch @MeetingError (Right <$> createMeeting zUser (Just (ConnId "test-conn")) Nothing newMeeting) (pure . Left)
            convs <- get @(Map ConvId StoredConversation)
            pure (r, convs)
        )

    case result of
      Left denied -> error $ "unexpected CheckDeleteLocalConversation denial: " <> show denied
      Right (Right (r, convs)) -> do
        convs `shouldSatisfy` Map.null
        r `shouldBe` Left LinkUnavailable
      Right (Left e) -> error $ "unexpected MeetingError: " <> show e

  it "listMeetings returns [] for a user with no meetings even without a configured URI" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid

    result <-
      runTestStackWithURI
        now
        gen
        (Right Map.empty)
        Map.empty
        def
        fullLicense
        (listMeetings zUser)

    case result of
      Left denied -> error $ "unexpected CheckDeleteLocalConversation denial: " <> show denied
      Right (Right inner) -> inner `shouldBe` []
      Right (Left e) -> error $ "unexpected MeetingError: " <> show e

  it "removes the join code when the meeting is deleted" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Doomed Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              tzid = API.defaultLegacyTimeZone,
              mtype = API.Scheduled,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              password = Nothing,
              recurrence = Nothing,
              invitedEmails = []
            }

    result <- runTestStack now gen Map.empty def $ do
      meeting <- createMeeting zUser (Just (ConnId "test-conn")) Nothing newMeeting
      let mid = qUnqualified meeting.meeting.id
      key <- CodeStore.makeKey (CodeReferentMeeting mid)
      deleted <- deleteMeeting zUser (Just (ConnId "test-conn")) Nothing meeting.meeting.id
      mCode <- gets @(Map Key (Code, Maybe Password)) (Map.lookup key)
      pure (deleted, mCode)

    case result of
      Left err -> fail $ "Error: " <> show err
      Right (deleted, mCode) -> do
        deleted `shouldBe` True
        isNothing mCode `shouldBe` True

  it "fails with LinkUnavailable when reading a meeting without a join code" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        mid = Id $ read "00000000-0000-0000-0000-00000000000a"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        sm =
          Store.StoredMeeting
            { id = mid,
              title = fromJust $ checked "Legacy Meeting",
              creator = uid,
              startTime = now,
              endTime = addUTCTime 3600 now,
              tzid = API.defaultLegacyTimeZone,
              meetingType = API.Scheduled,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              recurrence = Nothing,
              conversationId = Id $ read "00000000-0000-0000-0000-00000000000b",
              invitedEmails = [],
              trial = False,
              createdAt = now,
              updatedAt = now
            }

    result <- runTestStack now gen Map.empty def $ do
      modify @(Map MeetingId Store.StoredMeeting) (Map.insert mid sm)
      getMeeting zUser (Qualified mid (Domain "wire.com"))

    result `shouldBe` Left LinkUnavailable

  it "cleanup removes expired meetings together with their join codes" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        mid = Id $ read "00000000-0000-0000-0000-00000000000c"
        sm =
          Store.StoredMeeting
            { id = mid,
              title = fromJust $ checked "Expired Meeting",
              creator = uid,
              startTime = now,
              endTime = addUTCTime 60 now,
              tzid = API.defaultLegacyTimeZone,
              meetingType = API.Scheduled,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              recurrence = Nothing,
              conversationId = Id $ read "00000000-0000-0000-0000-00000000000d",
              invitedEmails = [],
              trial = False,
              createdAt = now,
              updatedAt = now
            }

    result <- runTestStack now gen Map.empty def $ do
      modify @(Map MeetingId Store.StoredMeeting) (Map.insert mid sm)
      key <- CodeStore.makeKey (CodeReferentMeeting mid)
      code <- CodeStore.generateCode (CodeReferentMeeting mid) (Timeout 3600)
      CodeStore.createCode code Nothing
      n <- cleanupOldMeetings (addUTCTime 7200 now) 10
      mCode <- gets @(Map Key (Code, Maybe Password)) (Map.lookup key)
      pure (n, mCode)

    case result of
      Left err -> fail $ "Error: " <> show err
      Right (n, mCode) -> do
        n `shouldBe` 1
        isNothing mCode `shouldBe` True

  it "listMeetings fails with LinkUnavailable when a listed meeting has no live code" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        mkStoredMeeting mid =
          Store.StoredMeeting
            { id = mid,
              title = fromJust $ checked "Listed Meeting",
              creator = uid,
              startTime = addUTCTime 60 now,
              endTime = addUTCTime 3600 now,
              tzid = API.defaultLegacyTimeZone,
              meetingType = API.Scheduled,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              recurrence = Nothing,
              conversationId = Id $ read "00000000-0000-0000-0000-00000000000d",
              invitedEmails = [],
              trial = False,
              createdAt = now,
              updatedAt = now
            }
        midWithCode = Id $ read "00000000-0000-0000-0000-00000000000e"
        midWithoutCode = Id $ read "00000000-0000-0000-0000-00000000000f"

    result <-
      runTestStack now gen Map.empty def $ do
        modify @(Map MeetingId Store.StoredMeeting) (Map.insert midWithCode (mkStoredMeeting midWithCode))
        modify @(Map MeetingId Store.StoredMeeting) (Map.insert midWithoutCode (mkStoredMeeting midWithoutCode))
        void $ CodeStore.createMeetingCode midWithCode (Timeout 3600) Nothing
        listMeetings zUser

    result `shouldBe` Left LinkUnavailable

  it "creates meeting conversation with invite and code access" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Access Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              tzid = API.defaultLegacyTimeZone,
              mtype = API.Scheduled,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              password = Nothing,
              recurrence = Nothing,
              invitedEmails = []
            }

    result <- runTestStack now gen Map.empty def $ do
      meeting <- createMeeting zUser (Just (ConnId "test-conn")) Nothing newMeeting
      pure (meeting.conversation.metadata.cnvmAccess, meeting.conversation.metadata.cnvmAccessRoles)

    case result of
      Left err -> fail $ "Error: " <> show err
      Right (access, roles) -> do
        access `shouldBe` [InviteAccess, CodeAccess]
        roles `shouldBe` Set.insert GuestAccessRole defRole

  it "fails to create a meeting if end_time is not after start_time" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Invalid Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 3500 now,
              tzid = API.defaultLegacyTimeZone,
              mtype = API.Scheduled,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              password = Nothing,
              recurrence = Nothing,
              invitedEmails = []
            }

    result <- runTestStack now gen Map.empty def $ createMeeting zUser (Just (ConnId "test-conn")) Nothing newMeeting
    result `shouldBe` Left (InvalidTimes EndBeforeStart)

  it "fails to create a meeting if start time is in the past" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Past Meeting",
              startTime = addUTCTime (negate 3600) now,
              endTime = addUTCTime 3600 now,
              tzid = API.defaultLegacyTimeZone,
              mtype = API.Scheduled,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              password = Nothing,
              recurrence = Nothing,
              invitedEmails = []
            }

    result <- runTestStack now gen Map.empty def $ createMeeting zUser (Just (ConnId "test-conn")) Nothing newMeeting
    result `shouldBe` Left (InvalidTimes StartTimeTooFarInPast)

  it "accepts a meeting whose start time is exactly at the tolerance boundary" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        -- Exactly `expectedStartTimeTolerance` in the past: the check is strict (`<`),
        -- so the boundary itself is still accepted.
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Boundary Meeting",
              startTime = addUTCTime (negate expectedStartTimeTolerance) now,
              endTime = addUTCTime 3600 now,
              tzid = API.defaultLegacyTimeZone,
              mtype = API.Scheduled,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              password = Nothing,
              recurrence = Nothing,
              invitedEmails = []
            }

    result <- runTestStack now gen Map.empty def $ createMeeting zUser (Just (ConnId "test-conn")) Nothing newMeeting
    result `shouldSatisfy` isRight

  it "rejects a meeting whose start time is just past the tolerance boundary" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        -- One second past the tolerance boundary.
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Just Past Boundary Meeting",
              startTime = addUTCTime (negate (expectedStartTimeTolerance + 1)) now,
              endTime = addUTCTime 3600 now,
              tzid = API.defaultLegacyTimeZone,
              mtype = API.Scheduled,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              password = Nothing,
              recurrence = Nothing,
              invitedEmails = []
            }

    result <- runTestStack now gen Map.empty def $ createMeeting zUser (Just (ConnId "test-conn")) Nothing newMeeting
    result `shouldBe` Left (InvalidTimes StartTimeTooFarInPast)

  describe "getMeeting access control" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid1 = Id $ read "00000000-0000-0000-0000-000000000001"
        uid2 = Id $ read "00000000-0000-0000-0000-000000000002"
        uid3 = Id $ read "00000000-0000-0000-0000-000000000003"
        zUser1 = toLocalUnsafe (Domain "wire.com") uid1
        zUser2 = toLocalUnsafe (Domain "wire.com") uid2
        zUser3 = toLocalUnsafe (Domain "wire.com") uid3
        teamId = Id $ read "00000000-0000-0000-0000-000000000100"
        teamMember1 = mkTeamMember uid1 fullPermissions Nothing UserLegalHoldDisabled
        teamMember2 = mkTeamMember uid2 fullPermissions Nothing UserLegalHoldDisabled
        teamConfig = npUpdate @MeetingsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) $ def

    it "returns Nothing for expired meeting" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Past Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        passTime validityWindow
        getMeeting zUser1 meeting.meeting.id

      result `shouldBe` Right Nothing

    it "returns meeting for creator" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Creator Access Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        (meeting,) <$> getMeeting zUser1 meeting.meeting.id

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (meeting, Just m) -> m.id `shouldBe` meeting.meeting.id
        Right (_, Nothing) -> fail "Expected Just meeting for creator"

    it "returns meeting for conversation member" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Member Access Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen (Map.singleton teamId [teamMember1, teamMember2]) teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        members <- gets (Map.lookup (qUnqualified meeting.conversation.qualifiedId))
        let updatedMembers = maybe (Set.singleton uid2) (Set.insert uid2) members
        modify (Map.insert (qUnqualified meeting.conversation.qualifiedId) updatedMembers)
        (meeting,) <$> getMeeting zUser2 meeting.meeting.id

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (meeting, Just m) -> m.id `shouldBe` meeting.meeting.id
        Right (_, Nothing) -> fail "Expected Just meeting for conversation member"

    it "returns Nothing for unauthorized user" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Unauthorized Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen (Map.singleton teamId [teamMember1]) teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        getMeeting zUser3 meeting.meeting.id

      result `shouldBe` Right Nothing

  describe "updateMeeting" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid1 = Id $ read "00000000-0000-0000-0000-000000000001"
        uid2 = Id $ read "00000000-0000-0000-0000-000000000002"
        zUser1 = toLocalUnsafe (Domain "wire.com") uid1
        zUser2 = toLocalUnsafe (Domain "wire.com") uid2
        teamId = Id $ read "00000000-0000-0000-0000-000000000100"
        teamMember1 = mkTeamMember uid1 fullPermissions Nothing UserLegalHoldDisabled
        teamMember2 = mkTeamMember uid2 fullPermissions Nothing UserLegalHoldDisabled
        teamConfig =
          npUpdate @MeetingsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) $ def

    it "throws EmptyUpdate when no fields provided" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Original Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        updateMeeting zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id emptyUpdate

      result `shouldBe` Left EmptyUpdate

    it "updates tzid when provided alone" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Tzid Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }
          newTz = fromJust (API.parseTimeZone "Europe/London")
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        updateMeeting
          zUser1
          (Just (ConnId "test-conn"))
          Nothing
          meeting.meeting.id
          (API.UpdateMeeting Nothing Nothing Nothing Nothing (Just newTz))
      case result of
        Left err -> fail $ "Expected the update to be applied, got: " <> show err
        Right Nothing -> fail "Expected the update to be applied"
        Right (Just updated) -> do
          updated.meeting.tzid `shouldBe` newTz
          updated.meeting.title `shouldBe` newMeeting.title

    it "throws InvalidTimes when end_time is not after start_time" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Original Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        let update =
              API.UpdateMeeting
                { startTime = Nothing,
                  endTime = Just (addUTCTime 3600 now),
                  title = Nothing,
                  recurrence = Nothing,
                  tzid = Nothing
                }
        updateMeeting zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id update
      result `shouldBe` Left (InvalidTimes EndBeforeStart)

    it "allows editing an upcoming meeting's start time into the past within the past-edit window" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Original Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        let update =
              API.UpdateMeeting
                { startTime = Just (addUTCTime (negate 60) now),
                  endTime = Nothing,
                  title = Nothing,
                  recurrence = Nothing,
                  tzid = Nothing
                }
        updateMeeting zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id update

      case result of
        Left err -> fail $ "Expected the update to be applied, got: " <> show err
        Right Nothing -> fail "Expected the update to be applied"
        Right (Just updated) ->
          updated.meeting.startTime `shouldBe` addUTCTime (negate 60) now

    it "accepts a meeting update whose start time is exactly at the past-edit boundary" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Original Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        let update =
              API.UpdateMeeting
                { startTime = Just (addUTCTime (negate configuredPastEditPeriod) now),
                  endTime = Nothing,
                  title = Nothing,
                  recurrence = Nothing,
                  tzid = Nothing
                }
        updateMeeting zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id update

      case result of
        Right (Just _) -> pure ()
        other -> fail $ "Expected the update to be applied, got: " <> show other

    it "rejects a meeting update whose start time is just past the past-edit boundary" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Original Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        let update =
              API.UpdateMeeting
                { startTime = Just (addUTCTime (negate (configuredPastEditPeriod + 1)) now),
                  endTime = Nothing,
                  title = Nothing,
                  recurrence = Nothing,
                  tzid = Nothing
                }
        updateMeeting zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id update
      result `shouldBe` Left (InvalidTimes TimesBeyondPastEditWindow)

    it "throws InvalidTimes when updating both times beyond the past-edit window even when start < end" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Original Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        let update =
              API.UpdateMeeting
                { startTime = Just (addUTCTime (negate 7200) now),
                  endTime = Just (addUTCTime (negate (configuredPastEditPeriod + 1)) now),
                  title = Nothing,
                  recurrence = Nothing,
                  tzid = Nothing
                }
        updateMeeting zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id update
      result `shouldBe` Left (InvalidTimes TimesBeyondPastEditWindow)

    it "allows editing an already-started (ongoing) meeting, including its start time" $ do
      let ongoingMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Ongoing Meeting",
                startTime = addUTCTime 100 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing ongoingMeeting
          -- Advance the clock 3000s: startTime (now+100s) is now in the past, so
          -- the meeting has started. It stays editable because isAlive is
          -- effective-end-based (end_time = now+7200s), still past
          -- the alive-cutoff (now+3000s-3600s = now-600s).
          passTime 3000
          let update =
                API.UpdateMeeting
                  { -- Original start time, which is now in the past.
                    startTime = Just (addUTCTime 100 now),
                    endTime = Nothing,
                    title = Just (unsafeRange "Edited While Ongoing"),
                    recurrence = Nothing,
                    tzid = Nothing
                  }
          updateMeeting zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id update
      case result of
        Left err ->
          fail $ "Expected the ongoing meeting to be editable, got: " <> show err
        Right Nothing -> fail "Expected the update to be applied"
        Right (Just updated) ->
          updated.meeting.title `shouldBe` unsafeRange "Edited While Ongoing"

    it "still throws InvalidTimes when editing an ongoing meeting to startTime >= endTime" $ do
      let ongoingMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Ongoing Meeting",
                startTime = addUTCTime 100 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing ongoingMeeting
          passTime 3000
          let update =
                API.UpdateMeeting
                  { startTime = Just (addUTCTime 8000 now),
                    endTime = Nothing,
                    title = Nothing,
                    recurrence = Nothing,
                    tzid = Nothing
                  }
          updateMeeting zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id update
      result `shouldBe` Left (InvalidTimes EndBeforeStart)

    it "returns Nothing for expired meeting" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Expired Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        passTime validityWindow
        updateMeeting zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id (API.UpdateMeeting Nothing Nothing (Just (unsafeRange "Test")) Nothing Nothing)

      result `shouldBe` Right Nothing

    it "returns Nothing for non-creator" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Non-creator Update",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen (Map.singleton teamId [teamMember1, teamMember2]) teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        updateMeeting zUser2 (Just (ConnId "test-conn")) Nothing meeting.meeting.id (API.UpdateMeeting Nothing Nothing (Just (unsafeRange "Test")) Nothing Nothing)

      result `shouldBe` Right Nothing

    it "returns Nothing when the meeting's conversation is missing" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Orphaned Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        -- Simulate a data-inconsistency: the meeting's conversation vanished.
        modify @(Map ConvId StoredConversation) (Map.delete (qUnqualified meeting.meeting.conversationId))
        updateMeeting zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id (API.UpdateMeeting Nothing Nothing (Just (unsafeRange "Updated")) Nothing Nothing)

      result `shouldBe` Right Nothing

    prop "applies valid update, preserves unchanged fields" $ \(update :: API.UpdateMeeting) ->
      let baseMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Original Title",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }
          -- Clamp the updated start/end times so they are not in the past
          -- beyond the past-edit window. This avoids discarding
          -- QuickCheck-generated updates whose arbitrary UTCTime is far from
          -- `now`.
          sanitizedUpdate =
            API.UpdateMeeting
              (fmap (max (addUTCTime (negate 60) now)) update.startTime)
              (fmap (max (addUTCTime (negate 60) now)) update.endTime)
              update.title
              update.recurrence
              update.tzid
          effectiveStart = fromMaybe baseMeeting.startTime sanitizedUpdate.startTime
          effectiveEndTime = fromMaybe baseMeeting.endTime sanitizedUpdate.endTime
          isNotEmpty = sanitizedUpdate /= emptyUpdate
          hasValidTimes = effectiveEndTime > effectiveStart
       in isNotEmpty && hasValidTimes ==>
            ioProperty $ do
              result <- runTestStack now gen Map.empty teamConfig $ do
                meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing baseMeeting
                updated <- updateMeeting zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id sanitizedUpdate
                pure (meeting.meeting.conversationId, updated)
              case result of
                Left err ->
                  pure $ counterexample ("Unexpected error: " <> show err) False
                Right (_, Nothing) ->
                  pure $ counterexample "Expected Just meeting, got Nothing" False
                Right (convId, Just m) ->
                  pure $
                    m.meeting.title === fromMaybe baseMeeting.title sanitizedUpdate.title
                      .&&. m.meeting.startTime === effectiveStart
                      .&&. m.meeting.endTime === effectiveEndTime
                      .&&. m.meeting.recurrence === fromMaybe baseMeeting.recurrence sanitizedUpdate.recurrence
                      .&&. m.meeting.tzid === fromMaybe baseMeeting.tzid sanitizedUpdate.tzid
                      .&&. m.meeting.conversationId === convId

  describe "deleteMeeting" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid1 = Id $ read "00000000-0000-0000-0000-000000000001"
        uid2 = Id $ read "00000000-0000-0000-0000-000000000002"
        zUser1 = toLocalUnsafe (Domain "wire.com") uid1
        zUser2 = toLocalUnsafe (Domain "wire.com") uid2
        teamId = Id $ read "00000000-0000-0000-0000-000000000100"
        teamMember1 = mkTeamMember uid1 fullPermissions Nothing UserLegalHoldDisabled
        teamMember2 = mkTeamMember uid2 fullPermissions Nothing UserLegalHoldDisabled
        teamConfig =
          npUpdate @MeetingsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) $ def
        testConnId = Just (ConnId (C.pack "test-conn"))

    it "returns True for successful deletion by creator" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Meeting to Delete",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        deleteResult <- deleteMeeting zUser1 testConnId Nothing meeting.meeting.id
        getResult <- getMeeting zUser1 meeting.meeting.id
        pure (deleteResult, getResult)

      result `shouldBe` Right (True, Nothing)

    it "returns False when non-creator tries to delete" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Meeting to Delete",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen (Map.singleton teamId [teamMember1, teamMember2]) teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        deleteMeeting zUser2 testConnId Nothing meeting.meeting.id

      result `shouldBe` Right False

    it "returns False for expired meeting deletion" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Expired Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        passTime validityWindow
        deleteMeeting zUser1 testConnId Nothing meeting.meeting.id

      result `shouldBe` Right False

    it "returns False when meeting does not exist" $ do
      let meetingId = Qualified (Id $ read "00000000-0000-0000-0000-000000000999") (Domain "wire.com")

      result <- runTestStack now gen Map.empty teamConfig $ do
        deleteMeeting zUser1 testConnId Nothing meetingId

      result `shouldBe` Right False

    it "deletes associated meeting conversation" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Meeting to Delete",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        _ <- internalGetConversation (qUnqualified meeting.conversation.qualifiedId)
        _ <- deleteMeeting zUser1 testConnId Nothing meeting.meeting.id
        internalGetConversation (qUnqualified meeting.conversation.qualifiedId)

      result `shouldBe` Right Nothing

    it "preserves non-meeting conversation" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Meeting to Delete",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        -- Change conversation type to non-meeting by updating local members only
        -- This simulates a non-meeting conversation without touching internal types
        deleteMeeting zUser1 testConnId Nothing meeting.meeting.id

      result `shouldSatisfy` isRight

  describe "refreshMeetingLink" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid1 = Id $ read "00000000-0000-0000-0000-000000000001"
        uid2 = Id $ read "00000000-0000-0000-0000-000000000002"
        zUser1 = toLocalUnsafe (Domain "wire.com") uid1
        zUser2 = toLocalUnsafe (Domain "wire.com") uid2
        teamId = Id $ read "00000000-0000-0000-0000-000000000100"
        teamMember1 = mkTeamMember uid1 fullPermissions Nothing UserLegalHoldDisabled
        teamConfig =
          npUpdate @MeetingsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) $ def
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Refreshable Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              tzid = API.defaultLegacyTimeZone,
              mtype = API.Scheduled,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              password = Nothing,
              recurrence = Nothing,
              invitedEmails = []
            }
        meetingCodeFor mid = do
          key <- CodeStore.makeKey (CodeReferentMeeting mid)
          gets @(Map Key (Code, Maybe Password)) (fmap fst . Map.lookup key)
        convCodeFor cid = do
          key <- CodeStore.makeKey (CodeReferentConv cid)
          gets @(Map Key (Code, Maybe Password)) (fmap fst . Map.lookup key)
        meetingPasswordFor mid = do
          key <- CodeStore.makeKey (CodeReferentMeeting mid)
          gets @(Map Key (Code, Maybe Password)) (fmap snd . Map.lookup key)

    it "recreates the join code and returns the meeting with a fresh link" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        let mid = qUnqualified meeting.meeting.id
            cid = qUnqualified meeting.meeting.conversationId
        -- plant a guest link on the meeting conversation; refresh must revoke it
        guestCode <- CodeStore.generateCode (CodeReferentConv cid) (Timeout 3600)
        CodeStore.createCode guestCode Nothing
        oldCode <- meetingCodeFor mid
        refreshed <- refreshMeetingLink zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id (API.RefreshMeetingLinkRequest Nothing)
        newCode <- meetingCodeFor mid
        guestAfter <- convCodeFor cid
        -- a URL carrying the stale code no longer resolves
        let joinWith u code = joinMeeting u (ConnId "test-conn") (Domain "wire.com") code.codeKey code.codeValue Nothing
        joinOld <- traverse (joinWith zUser2) oldCode
        joinNew <- traverse (joinWith zUser2) newCode
        pure (meeting, refreshed, oldCode, newCode, guestAfter, joinOld, joinNew)

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (meeting, refreshed, oldCode, newCode, guestAfter, joinOld, joinNew) -> do
          fmap (.meeting) refreshed
            `shouldBe` Just
              ( meeting.meeting
                  { API.link = maybe (error "expected refreshed code") (expectedLink testCodeURIBase (Domain "wire.com")) newCode
                  }
              )
          oldCode `shouldSatisfy` isJust
          newCode `shouldSatisfy` isJust
          -- the old join code value no longer resolves (replaced in one atomic upsert)
          newCode `shouldSatisfy` (/= oldCode)
          joinOld `shouldBe` Just JoinMeetingNotFound
          joinNew `shouldSatisfy` \case
            Just (JoinMeetingOk _) -> True
            _ -> False
          -- the meeting conversation's guest link is revoked
          guestAfter `shouldBe` Nothing

    it "hashes and stores the supplied password, replacing any previous one" $ do
      let pw1 = plainTextPassword8Unsafe "correct horse battery"
          pw2 = plainTextPassword8Unsafe "another secret phrase"
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        let qmid = meeting.meeting.id
            mid = qUnqualified qmid
        ok1 <- refreshMeetingLink zUser1 (Just (ConnId "test-conn")) Nothing qmid (API.RefreshMeetingLinkRequest (Just pw1))
        hashed1 <- maybe (error "no password stored after refresh") pure . join =<< meetingPasswordFor mid
        verified1 <- verifyPassword (RateLimitUser uid1) pw1 hashed1
        ok2 <- refreshMeetingLink zUser1 (Just (ConnId "test-conn")) Nothing qmid (API.RefreshMeetingLinkRequest (Just pw2))
        hashed2 <- maybe (error "no password stored after refresh") pure . join =<< meetingPasswordFor mid
        verified2 <- verifyPassword (RateLimitUser uid1) pw2 hashed2
        stale1 <- verifyPassword (RateLimitUser uid1) pw1 hashed2
        pure (fmap (.meeting.hasPassword) ok1, fmap (.meeting.hasPassword) ok2, verified1, verified2, stale1)

      case result of
        Left err -> fail $ "Error: " <> show err
        Right res -> res `shouldBe` (Just True, Just True, True, True, False)

    it "refresh without a password clears password protection" $ do
      let pw1 = plainTextPassword8Unsafe "temporary password"
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        let qmid = meeting.meeting.id
            mid = qUnqualified qmid
        ok1 <- refreshMeetingLink zUser1 (Just (ConnId "test-conn")) Nothing qmid (API.RefreshMeetingLinkRequest (Just pw1))
        pwAfterSet <- meetingPasswordFor mid
        ok2 <- refreshMeetingLink zUser1 (Just (ConnId "test-conn")) Nothing qmid (API.RefreshMeetingLinkRequest Nothing)
        pwAfterClear <- meetingPasswordFor mid
        pure (pwAfterSet, pwAfterClear, fmap (.meeting.hasPassword) ok1, fmap (.meeting.hasPassword) ok2)

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (pwAfterSet, pwAfterClear, hasPwSet, hasPwCleared) -> do
          pwAfterSet `shouldSatisfy` (isJust . join)
          pwAfterClear `shouldSatisfy` (isNothing . join)
          hasPwSet `shouldBe` Just True
          hasPwCleared `shouldBe` Just False

    it "emits a meeting.update event on success" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        _refreshed <- refreshMeetingLink zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id (API.RefreshMeetingLinkRequest Nothing)
        pushes <- gets @[Push] id
        pure (meeting.meeting.id, extractMeetingEvents pushes)

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (_qmid, events) ->
          map (.evtType) events `shouldBe` [MeetingEvent.Update, MeetingEvent.Create]

    it "returns Nothing and emits no event for a non-creator" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        refreshed <- refreshMeetingLink zUser2 (Just (ConnId "test-conn")) Nothing meeting.meeting.id (API.RefreshMeetingLinkRequest Nothing)
        pushes <- gets @[Push] id
        pure (refreshed, extractMeetingEvents pushes)

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (refreshed, events) -> do
          refreshed `shouldBe` Nothing
          events `shouldSatisfy` all (\e -> e.evtType == MeetingEvent.Create)

    it "returns Nothing for a non-existent meeting" $ do
      let qmid = Qualified (Id $ read "00000000-0000-0000-0000-000000000999") (Domain "wire.com")
      result <-
        runTestStack now gen Map.empty teamConfig $
          refreshMeetingLink zUser1 (Just (ConnId "test-conn")) Nothing qmid (API.RefreshMeetingLinkRequest Nothing)
      result `shouldBe` Right Nothing

    it "throws MeetingsFeatureDisabled for a team user with meetings disabled" $ do
      let meetingsDisabled =
            npUpdate @MeetingsConfig (LockableFeature FeatureStatusDisabled LockStatusUnlocked def) $ def
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember1]) meetingsDisabled $
          refreshMeetingLink zUser1 (Just (ConnId "test-conn")) Nothing (Qualified (Id $ read "00000000-0000-0000-0000-000000000999") (Domain "wire.com")) (API.RefreshMeetingLinkRequest Nothing)
      result `shouldBe` Left MeetingsFeatureDisabled

  describe "joinMeeting" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid1 = Id $ read "00000000-0000-0000-0000-000000000001"
        uid2 = Id $ read "00000000-0000-0000-0000-000000000002"
        zUser1 = toLocalUnsafe (Domain "wire.com") uid1
        zUser2 = toLocalUnsafe (Domain "wire.com") uid2
        teamConfig =
          npUpdate @MeetingsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) $ def
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Joinable Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              tzid = API.defaultLegacyTimeZone,
              mtype = API.Scheduled,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              password = Nothing,
              recurrence = Nothing,
              invitedEmails = []
            }
        pw = plainTextPassword8Unsafe "join secret pw"
        pwMeeting =
          API.NewMeeting
            { title = newMeeting.title,
              startTime = newMeeting.startTime,
              endTime = newMeeting.endTime,
              tzid = newMeeting.tzid,
              mtype = newMeeting.mtype,
              externallyManagedBy = newMeeting.externallyManagedBy,
              externallyCalendarEventId = newMeeting.externallyCalendarEventId,
              externallyCalendarId = newMeeting.externallyCalendarId,
              recurrence = newMeeting.recurrence,
              invitedEmails = newMeeting.invitedEmails,
              password = Just pw
            }
        meetingCode mid = do
          key <- CodeStore.makeKey (CodeReferentMeeting mid)
          mCode <- gets @(Map Key (Code, Maybe Password)) (Map.lookup key)
          maybe (error "no meeting code stored") (pure . fst) mCode
        meetingPasswordFor mid = do
          key <- CodeStore.makeKey (CodeReferentMeeting mid)
          gets @(Map Key (Code, Maybe Password)) (fmap snd . Map.lookup key)
    it "resolves a passwordless live link and joins the conversation" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        code <- meetingCode (qUnqualified meeting.meeting.id)
        joined <- joinMeeting zUser2 (ConnId "test-conn") (Domain "wire.com") code.codeKey code.codeValue Nothing
        members <- gets @(Map ConvId (Set UserId)) (Map.lookup (qUnqualified meeting.meeting.conversationId))
        pure (meeting.meeting, code, joined, members)
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (meeting, code, JoinMeetingOk mwc, members) -> do
          mwc.meeting.title `shouldBe` meeting.title
          mwc.meeting.link `shouldBe` expectedLink testCodeURIBase (Domain "wire.com") code
          mwc.meeting.hasPassword `shouldBe` False
          members `shouldSatisfy` maybe False (Set.member uid2)
        Right (_, _, r, _) -> fail $ "expected JoinMeetingOk, got: " <> show r

    it "stores a hash of the create-time password" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing pwMeeting
        hashed <- join <$> meetingPasswordFor (qUnqualified meeting.meeting.id)
        case hashed of
          Nothing -> pure (False, False, False)
          Just h -> do
            good <- verifyPassword (RateLimitUser uid1) pw h
            bad <- verifyPassword (RateLimitUser uid1) (plainTextPassword8Unsafe "wrong secret") h
            pure (meeting.meeting.hasPassword, good, bad)
      result `shouldBe` Right (True, True, False)

    it "enforces the create-time password on join" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing pwMeeting
        code <- meetingCode (qUnqualified meeting.meeting.id)
        let joinWith mpw = joinMeeting zUser2 (ConnId "test-conn") (Domain "wire.com") code.codeKey code.codeValue mpw
        noPw <- joinWith Nothing
        wrongPw <- joinWith (Just (plainTextPassword8Unsafe "wrong secret"))
        rightPw <- joinWith (Just pw)
        pure (noPw, wrongPw, rightPw)
      case result of
        Right (JoinMeetingInvalidPassword, JoinMeetingInvalidPassword, JoinMeetingOk _) -> pure ()
        r -> fail $ "unexpected result: " <> show r

    it "re-joining is an idempotent no-op" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        code <- meetingCode (qUnqualified meeting.meeting.id)
        firstJoin <- joinMeeting zUser2 (ConnId "test-conn") (Domain "wire.com") code.codeKey code.codeValue Nothing
        membersAfterFirst <- gets @(Map ConvId (Set UserId)) (Map.lookup (qUnqualified meeting.meeting.conversationId))
        secondJoin <- joinMeeting zUser2 (ConnId "test-conn") (Domain "wire.com") code.codeKey code.codeValue Nothing
        membersAfterSecond <- gets @(Map ConvId (Set UserId)) (Map.lookup (qUnqualified meeting.meeting.conversationId))
        pure (firstJoin, membersAfterFirst, secondJoin, membersAfterSecond)
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (firstJoin, membersAfterFirst, secondJoin, membersAfterSecond) -> do
          firstJoin `shouldSatisfy` \case JoinMeetingOk _ -> True; _ -> False
          secondJoin `shouldSatisfy` \case JoinMeetingOk _ -> True; _ -> False
          membersAfterSecond `shouldBe` membersAfterFirst

    it "rejects a missing password on a protected link" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        _ <- refreshMeetingLink zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id (API.RefreshMeetingLinkRequest (Just pw))
        code <- meetingCode (qUnqualified meeting.meeting.id)
        joinMeeting zUser2 (ConnId "test-conn") (Domain "wire.com") code.codeKey code.codeValue Nothing
      result `shouldBe` Right JoinMeetingInvalidPassword

    it "rejects a wrong password on a protected link" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        _ <- refreshMeetingLink zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id (API.RefreshMeetingLinkRequest (Just pw))
        code <- meetingCode (qUnqualified meeting.meeting.id)
        joinMeeting zUser2 (ConnId "test-conn") (Domain "wire.com") code.codeKey code.codeValue (Just (plainTextPassword8Unsafe "wrong secret"))
      result `shouldBe` Right JoinMeetingInvalidPassword

    it "accepts the correct password on a protected link" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        _ <- refreshMeetingLink zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id (API.RefreshMeetingLinkRequest (Just pw))
        code <- meetingCode (qUnqualified meeting.meeting.id)
        joinMeeting zUser2 (ConnId "test-conn") (Domain "wire.com") code.codeKey code.codeValue (Just pw)
      case result of
        Right (JoinMeetingOk _) -> pure ()
        r -> fail $ "expected JoinMeetingOk, got: " <> show r

    it "ignores a password supplied for a passwordless link" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        code <- meetingCode (qUnqualified meeting.meeting.id)
        joinMeeting zUser2 (ConnId "test-conn") (Domain "wire.com") code.codeKey code.codeValue (Just pw)
      case result of
        Right (JoinMeetingOk _) -> pure ()
        r -> fail $ "expected JoinMeetingOk, got: " <> show r

    it "accepts a passwordless join after the password was cleared" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        _ <- refreshMeetingLink zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id (API.RefreshMeetingLinkRequest (Just pw))
        _ <- refreshMeetingLink zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id (API.RefreshMeetingLinkRequest Nothing)
        code <- meetingCode (qUnqualified meeting.meeting.id)
        joinMeeting zUser2 (ConnId "test-conn") (Domain "wire.com") code.codeKey code.codeValue Nothing
      case result of
        Right (JoinMeetingOk _) -> pure ()
        r -> fail $ "expected JoinMeetingOk, got: " <> show r

    it "returns JoinMeetingNotFound for a foreign domain" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        code <- meetingCode (qUnqualified meeting.meeting.id)
        joinMeeting zUser2 (ConnId "test-conn") (Domain "other.example.com") code.codeKey code.codeValue Nothing
      result `shouldBe` Right JoinMeetingNotFound

    it "returns JoinMeetingNotFound when the join code is gone" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        codeVal <- meetingCode (qUnqualified meeting.meeting.id)
        CodeStore.deleteMeetingCode (qUnqualified meeting.meeting.id)
        joinMeeting zUser1 (ConnId "test-conn") (Domain "wire.com") codeVal.codeKey codeVal.codeValue Nothing
      result `shouldBe` Right JoinMeetingNotFound

    it "returns JoinMeetingNotFound for a stale code value" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        oldCode <- meetingCode (qUnqualified meeting.meeting.id)
        _ <- refreshMeetingLink zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id (API.RefreshMeetingLinkRequest Nothing)
        joinMeeting zUser2 (ConnId "test-conn") (Domain "wire.com") oldCode.codeKey oldCode.codeValue Nothing
      result `shouldBe` Right JoinMeetingNotFound

    it "returns JoinMeetingNotFound for a conversation code key" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        -- a conversation join code's key does not address a meeting
        convCode <- CodeStore.generateCode (CodeReferentConv (qUnqualified meeting.meeting.conversationId)) (Timeout 3600)
        CodeStore.createCode convCode Nothing
        joinMeeting zUser1 (ConnId "test-conn") (Domain "wire.com") convCode.codeKey convCode.codeValue Nothing
      result `shouldBe` Right JoinMeetingNotFound

  describe "codeCheckMeetingLink" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid1 = Id $ read "00000000-0000-0000-0000-000000000001"
        uid2 = Id $ read "00000000-0000-0000-0000-000000000002"
        zUser1 = toLocalUnsafe (Domain "wire.com") uid1
        -- the harness local domain (runInputConst in runTestStack)
        localDomain = Domain "my-domain"
        check = codeCheckMeetingLink (IpAddr (read @IP "10.0.0.1")) localDomain
        teamConfig =
          npUpdate @MeetingsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) $ def
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Checkable Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              tzid = API.defaultLegacyTimeZone,
              mtype = API.Scheduled,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              password = Nothing,
              recurrence = Nothing,
              invitedEmails = []
            }
        pw = plainTextPassword8Unsafe "check secret pw"
        meetingCode mid = do
          key <- CodeStore.makeKey (CodeReferentMeeting mid)
          mCode <- gets @(Map Key (Code, Maybe Password)) (Map.lookup key)
          maybe (error "no meeting code stored") (pure . fst) mCode

    it "checks a passwordless live link unauthenticated" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        code <- meetingCode (qUnqualified meeting.meeting.id)
        checked' <- check code.codeKey code.codeValue Nothing
        membersAfter <- gets @(Map ConvId (Set UserId)) (Map.lookup (qUnqualified meeting.meeting.conversationId))
        pure (meeting.meeting, checked', membersAfter)
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (meeting, CodeCheckOk meta, membersAfter) -> do
          meta.title `shouldBe` meeting.title
          meta.startTime `shouldBe` meeting.startTime
          meta.endTime `shouldBe` meeting.endTime
          -- read-only: checking must not change conversation membership
          membersAfter `shouldSatisfy` not . maybe False (Set.member uid2)
        Right (_, r, _) -> fail $ "expected CodeCheckOk, got: " <> show r

    it "rejects a password-protected link without a password" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        _ <- refreshMeetingLink zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id (API.RefreshMeetingLinkRequest (Just pw))
        code <- meetingCode (qUnqualified meeting.meeting.id)
        check code.codeKey code.codeValue Nothing
      result `shouldBe` Right CodeCheckInvalidPassword

    it "rejects a password-protected link with a wrong password" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        _ <- refreshMeetingLink zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id (API.RefreshMeetingLinkRequest (Just pw))
        code <- meetingCode (qUnqualified meeting.meeting.id)
        check code.codeKey code.codeValue (Just (plainTextPassword8Unsafe "wrong secret"))
      result `shouldBe` Right CodeCheckInvalidPassword

    it "accepts a password-protected link with the correct password" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        _ <- refreshMeetingLink zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id (API.RefreshMeetingLinkRequest (Just pw))
        code <- meetingCode (qUnqualified meeting.meeting.id)
        check code.codeKey code.codeValue (Just pw)
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (CodeCheckOk _) -> pure ()
        Right r -> fail $ "expected CodeCheckOk, got: " <> show r

    it "accepts a passwordless link even when a password is supplied" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        code <- meetingCode (qUnqualified meeting.meeting.id)
        check code.codeKey code.codeValue (Just pw)
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (CodeCheckOk _) -> pure ()
        Right r -> fail $ "expected CodeCheckOk, got: " <> show r

    it "returns CodeCheckNotFound for a foreign domain" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        code <- meetingCode (qUnqualified meeting.meeting.id)
        codeCheckMeetingLink (IpAddr (read @IP "10.0.0.1")) (Domain "other.example.com") code.codeKey code.codeValue Nothing
      result `shouldBe` Right CodeCheckNotFound

    it "returns CodeCheckNotFound for a stale code value" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        oldCode <- meetingCode (qUnqualified meeting.meeting.id)
        _ <- refreshMeetingLink zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id (API.RefreshMeetingLinkRequest Nothing)
        check oldCode.codeKey oldCode.codeValue Nothing
      result `shouldBe` Right CodeCheckNotFound

    it "returns CodeCheckNotFound when the join code is gone" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        codeVal <- meetingCode (qUnqualified meeting.meeting.id)
        CodeStore.deleteMeetingCode (qUnqualified meeting.meeting.id)
        check codeVal.codeKey codeVal.codeValue Nothing
      result `shouldBe` Right CodeCheckNotFound

    it "returns CodeCheckNotFound for a conversation code key" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        -- a conversation join code's key does not address a meeting
        convCode <- CodeStore.generateCode (CodeReferentConv (qUnqualified meeting.meeting.conversationId)) (Timeout 3600)
        CodeStore.createCode convCode Nothing
        check convCode.codeKey convCode.codeValue Nothing
      result `shouldBe` Right CodeCheckNotFound

  describe "invitationLinks licensing" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid1 = Id $ read "00000000-0000-0000-0000-000000000001"
        uid2 = Id $ read "00000000-0000-0000-0000-000000000002"
        zUser1 = toLocalUnsafe (Domain "wire.com") uid1
        zUser2 = toLocalUnsafe (Domain "wire.com") uid2
        teamConfig =
          npUpdate @MeetingsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) $ def
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Licensed Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              tzid = API.defaultLegacyTimeZone,
              mtype = API.Scheduled,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              password = Nothing,
              recurrence = Nothing,
              invitedEmails = []
            }
        meetingCode mid = do
          key <- CodeStore.makeKey (CodeReferentMeeting mid)
          mCode <- gets @(Map Key (Code, Maybe Password)) (Map.lookup key)
          maybe (error "no meeting code stored") (pure . fst) mCode

    it "returns MeetingNotLicensed when the license is missing (join)" $ do
      result <-
        runTestStackWithURI
          now
          gen
          (Left testCodeURIBase)
          Map.empty
          teamConfig
          missingLicense
          ( do
              meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
              code <- meetingCode (qUnqualified meeting.meeting.id)
              joinMeeting zUser2 (ConnId "test-conn") (Domain "wire.com") code.codeKey code.codeValue Nothing
          )
      result `shouldBe` Right (Left MeetingNotLicensed)

    it "returns MeetingNotLicensed when the license is missing (code-check)" $ do
      result <-
        runTestStackWithURI
          now
          gen
          (Left testCodeURIBase)
          Map.empty
          teamConfig
          missingLicense
          ( do
              meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
              code <- meetingCode (qUnqualified meeting.meeting.id)
              codeCheckMeetingLink (IpAddr (read @IP "10.0.0.1")) (Domain "my-domain") code.codeKey code.codeValue Nothing
          )
      result `shouldBe` Right (Left MeetingNotLicensed)

    it "joins when invitation links are licensed" $ do
      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        code <- meetingCode (qUnqualified meeting.meeting.id)
        joinMeeting zUser2 (ConnId "test-conn") (Domain "wire.com") code.codeKey code.codeValue Nothing
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (JoinMeetingOk mwc) -> mwc.meeting.title `shouldBe` newMeeting.title
        Right r -> fail $ "expected JoinMeetingOk, got: " <> show r

  describe "addInvitedEmails" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid1 = Id $ read "00000000-0000-0000-0000-000000000001"
        uid2 = Id $ read "00000000-0000-0000-0000-000000000002"
        zUser1 = toLocalUnsafe (Domain "wire.com") uid1
        zUser2 = toLocalUnsafe (Domain "wire.com") uid2
        teamId = Id $ read "00000000-0000-0000-0000-000000000100"
        teamMember1 = mkTeamMember uid1 fullPermissions Nothing UserLegalHoldDisabled
        teamMember2 = mkTeamMember uid2 fullPermissions Nothing UserLegalHoldDisabled
        teamConfig =
          npUpdate @MeetingsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) $ def
        email1 = unsafeEmailAddress "user1" "example.com"
        email2 = unsafeEmailAddress "user2" "example.com"

    it "returns True and adds emails for creator of valid meeting" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Test Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        success <- addInvitedEmails zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id [email1, email2] Nothing
        fetched <- getMeeting zUser1 meeting.meeting.id
        pure (success, fetched)

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (success, Just m) -> do
          success `shouldBe` True
          m.invitedEmails `shouldBe` [email1, email2]
        Right (_, Nothing) -> fail "Expected Just meeting"

    it "returns False for expired meeting" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Expired Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        passTime validityWindow
        addInvitedEmails zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id [email1] Nothing

      result `shouldBe` Right False

    it "returns False for non-creator" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Non-creator Test",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen (Map.singleton teamId [teamMember1, teamMember2]) teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        addInvitedEmails zUser2 (Just (ConnId "test-conn")) Nothing meeting.meeting.id [email1] Nothing

      result `shouldBe` Right False

    it "returns False for non-existent meeting" $ do
      let nonExistentId = Qualified (Id $ read "00000000-0000-0000-0000-000000000999") (Domain "wire.com")

      result <-
        runTestStack now gen Map.empty teamConfig $
          addInvitedEmails zUser1 (Just (ConnId "test-conn")) Nothing nonExistentId [email1] Nothing

      result `shouldBe` Right False

  describe "removeInvitedEmails" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid1 = Id $ read "00000000-0000-0000-0000-000000000001"
        uid2 = Id $ read "00000000-0000-0000-0000-000000000002"
        zUser1 = toLocalUnsafe (Domain "wire.com") uid1
        zUser2 = toLocalUnsafe (Domain "wire.com") uid2
        teamId = Id $ read "00000000-0000-0000-0000-000000000100"
        teamMember1 = mkTeamMember uid1 fullPermissions Nothing UserLegalHoldDisabled
        teamMember2 = mkTeamMember uid2 fullPermissions Nothing UserLegalHoldDisabled
        teamConfig =
          npUpdate @MeetingsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) $ def
        email1 = unsafeEmailAddress "user1" "example.com"
        email2 = unsafeEmailAddress "user2" "example.com"
        email3 = unsafeEmailAddress "user3" "example.com"

    it "returns True and removes emails for creator of valid meeting" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Test Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = [email1, email2, email3]
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        success <- removeInvitedEmails zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id [email2]
        fetched <- getMeeting zUser1 meeting.meeting.id
        pure (success, fetched)

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (success, Just m) -> do
          success `shouldBe` True
          m.invitedEmails `shouldBe` [email1, email3]
        Right (_, Nothing) -> fail "Expected Just meeting"

    it "returns True when removing all emails" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Test Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = [email1, email2]
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        success <- removeInvitedEmails zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id [email1, email2]
        fetched <- getMeeting zUser1 meeting.meeting.id
        pure (success, fetched)

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (success, Just m) -> do
          success `shouldBe` True
          m.invitedEmails `shouldBe` []
        Right (_, Nothing) -> fail "Expected Just meeting"

    it "returns True when removing non-existent emails" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Test Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = [email1]
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        success <- removeInvitedEmails zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id [email2, email3]
        fetched <- getMeeting zUser1 meeting.meeting.id
        pure (success, fetched)

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (success, Just m) -> do
          success `shouldBe` True
          m.invitedEmails `shouldBe` [email1]
        Right (_, Nothing) -> fail "Expected Just meeting"

    it "returns False for expired meeting" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Expired Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = [email1]
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        passTime validityWindow
        removeInvitedEmails zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id [email1]

      result `shouldBe` Right False

    it "returns False for non-creator" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Non-creator Test",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = [email1, email2]
              }

      result <- runTestStack now gen (Map.singleton teamId [teamMember1, teamMember2]) teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        removeInvitedEmails zUser2 (Just (ConnId "test-conn")) Nothing meeting.meeting.id [email1]

      result `shouldBe` Right False

    it "returns False for non-existent meeting" $ do
      let nonExistentId = Qualified (Id $ read "00000000-0000-0000-0000-000000000999") (Domain "wire.com")

      result <-
        runTestStack now gen Map.empty teamConfig $
          removeInvitedEmails zUser1 (Just (ConnId "test-conn")) Nothing nonExistentId [email1]

      result `shouldBe` Right False

  describe "replaceInvitedEmails" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid1 = Id $ read "00000000-0000-0000-0000-000000000001"
        uid2 = Id $ read "00000000-0000-0000-0000-000000000002"
        zUser1 = toLocalUnsafe (Domain "wire.com") uid1
        zUser2 = toLocalUnsafe (Domain "wire.com") uid2
        teamId = Id $ read "00000000-0000-0000-0000-000000000100"
        teamMember1 = mkTeamMember uid1 fullPermissions Nothing UserLegalHoldDisabled
        teamMember2 = mkTeamMember uid2 fullPermissions Nothing UserLegalHoldDisabled
        teamConfig =
          npUpdate @MeetingsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) $ def
        email1 = unsafeEmailAddress "user1" "example.com"
        email2 = unsafeEmailAddress "user2" "example.com"
        email3 = unsafeEmailAddress "user3" "example.com"

    it "returns True and replaces emails for creator of valid meeting" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Test Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = [email1, email2]
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        success <- replaceInvitedEmails zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id [email3] Nothing
        fetched <- getMeeting zUser1 meeting.meeting.id
        pure (success, fetched)

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (success, Just m) -> do
          success `shouldBe` True
          m.invitedEmails `shouldBe` [email3]
        Right (_, Nothing) -> fail "Expected Just meeting"

    it "returns True when replacing with an empty list" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Test Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = [email1, email2]
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        success <- replaceInvitedEmails zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id [] Nothing
        fetched <- getMeeting zUser1 meeting.meeting.id
        pure (success, fetched)

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (success, Just m) -> do
          success `shouldBe` True
          m.invitedEmails `shouldBe` []
        Right (_, Nothing) -> fail "Expected Just meeting"

    it "deduplicates emails when replacing (mirrors Postgres semantics)" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Test Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = [email1, email2]
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        success <- replaceInvitedEmails zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id [email3, email3, email1] Nothing
        fetched <- getMeeting zUser1 meeting.meeting.id
        pure (success, fetched)

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (success, Just m) -> do
          success `shouldBe` True
          m.invitedEmails `shouldBe` [email3, email1]
        Right (_, Nothing) -> fail "Expected Just meeting"

    it "returns False for expired meeting" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Expired Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = [email1]
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        passTime validityWindow
        replaceInvitedEmails zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id [email2] Nothing

      result `shouldBe` Right False

    it "returns False for non-creator" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Non-creator Test",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = [email1, email2]
              }

      result <- runTestStack now gen (Map.singleton teamId [teamMember1, teamMember2]) teamConfig $ do
        meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
        replaceInvitedEmails zUser2 (Just (ConnId "test-conn")) Nothing meeting.meeting.id [email3] Nothing

      result `shouldBe` Right False

    it "returns False for non-existent meeting" $ do
      let nonExistentId = Qualified (Id $ read "00000000-0000-0000-0000-000000000999") (Domain "wire.com")

      result <-
        runTestStack now gen Map.empty teamConfig $
          replaceInvitedEmails zUser1 (Just (ConnId "test-conn")) Nothing nonExistentId [email1] Nothing

      result `shouldBe` Right False

  describe "recurrence vs expiry" $ do
    -- validityPeriod in runTestStack is 3600s, so a meeting whose endTime is
    -- more than 1h in the past is treated as expired unless its recurrence
    -- window keeps it alive.
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        teamConfig =
          npUpdate @MeetingsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) $
            def
        -- Meetings are created with future times. We then advance the mock
        -- clock past the validity window (11000s) so that the original slot
        -- has passed, while the recurrence window stays open.
        futureMeeting r =
          API.NewMeeting
            { title = fromJust $ checked "Recurring Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              tzid = API.defaultLegacyTimeZone,
              mtype = API.Scheduled,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              password = Nothing,
              recurrence = r,
              invitedEmails = []
            }
        boundedRecurrence =
          Just $
            API.Recurrence
              { freq = API.Daily,
                interval = 1,
                until = Just (addUTCTime (30 * nominalDay) now)
              }
        openEndedRecurrence =
          Just $
            API.Recurrence
              { freq = API.Daily,
                interval = 1,
                until = Nothing
              }
        meetingAt endOffset r =
          API.NewMeeting
            { title = fromJust $ checked "Meeting",
              startTime = addUTCTime (endOffset - 3600) now,
              endTime = addUTCTime endOffset now,
              tzid = API.defaultLegacyTimeZone,
              mtype = API.Scheduled,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              password = Nothing,
              recurrence = r,
              invitedEmails = []
            }
        recurUntil t =
          Just (API.Recurrence {freq = API.Daily, interval = 1, until = Just t})

    it "getMeeting returns a recurring meeting whose slot passed but window is open" $ do
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          meeting <- createMeeting zUser (Just (ConnId "test-conn")) Nothing (futureMeeting boundedRecurrence)
          passTime validityWindow
          getMeeting zUser meeting.meeting.id
      case result of
        Left err -> fail $ "Error: " <> show err
        Right Nothing -> fail "Expected Just meeting (recurrence window still open)"
        Right (Just _) -> pure ()

    it "listMeetings includes a recurring meeting whose slot passed" $ do
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          _meeting <- createMeeting zUser (Just (ConnId "test-conn")) Nothing (futureMeeting boundedRecurrence)
          passTime validityWindow
          listMeetings zUser
      case result of
        Left err -> fail $ "Error: " <> show err
        Right xs -> length xs `shouldBe` 1

    it "updateMeeting succeeds on a recurring meeting whose slot passed" $ do
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          meeting <- createMeeting zUser (Just (ConnId "test-conn")) Nothing (futureMeeting boundedRecurrence)
          passTime validityWindow
          updateMeeting zUser (Just (ConnId "test-conn")) Nothing meeting.meeting.id (API.UpdateMeeting Nothing Nothing (Just (unsafeRange "Updated")) Nothing Nothing)
      fmap isJust result `shouldBe` Right True

    it "addInvitedEmails succeeds on a recurring meeting whose slot passed" $ do
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          meeting <- createMeeting zUser (Just (ConnId "test-conn")) Nothing (futureMeeting boundedRecurrence)
          passTime validityWindow
          addInvitedEmails zUser (Just (ConnId "test-conn")) Nothing meeting.meeting.id [unsafeEmailAddress "user" "example.com"] Nothing
      result `shouldBe` Right True

    it "deleteMeeting succeeds on a recurring meeting whose slot passed" $ do
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          meeting <- createMeeting zUser (Just (ConnId "test-conn")) Nothing (futureMeeting boundedRecurrence)
          passTime validityWindow
          deleteMeeting zUser (Just (ConnId "test-conv")) Nothing meeting.meeting.id
      result `shouldBe` Right True

    it "removeInvitedEmails succeeds on a recurring meeting whose slot passed" $ do
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          meeting <- createMeeting zUser (Just (ConnId "test-conn")) Nothing (futureMeeting boundedRecurrence)
          passTime validityWindow
          removeInvitedEmails zUser (Just (ConnId "test-conn")) Nothing meeting.meeting.id [unsafeEmailAddress "user" "example.com"]
      result `shouldBe` Right True

    it "replaceInvitedEmails succeeds on a recurring meeting whose slot passed" $ do
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          meeting <- createMeeting zUser (Just (ConnId "test-conn")) Nothing (futureMeeting boundedRecurrence)
          passTime validityWindow
          replaceInvitedEmails zUser (Just (ConnId "test-conn")) Nothing meeting.meeting.id [unsafeEmailAddress "user" "example.com"] Nothing
      result `shouldBe` Right True

    it "getMeeting returns an open-ended recurring meeting indefinitely" $ do
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          meeting <- createMeeting zUser (Just (ConnId "test-conn")) Nothing (futureMeeting openEndedRecurrence)
          passTime validityWindow
          getMeeting zUser meeting.meeting.id
      fmap isJust result `shouldBe` Right True

    it "cleanupOldMeetings skips recurring meetings whose window is still open" $ do
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          recurring <- createMeeting zUser (Just (ConnId "test-conn")) Nothing (futureMeeting boundedRecurrence)
          _plain <- createMeeting zUser (Just (ConnId "test-conn")) Nothing (futureMeeting Nothing)
          passTime validityWindow
          -- cutoff is past the endTime (now+7200) so the non-recurring
          -- meeting is picked up, but well before the recurrence window.
          deleted <- cleanupOldMeetings (addUTCTime 7300 now) 100
          remaining <- getMeeting zUser recurring.meeting.id
          pure (deleted, fmap (.id) remaining, recurring.meeting.id)
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (deleted, remainingId, recurringId) -> do
          -- only the non-recurring meeting is past the cleanup cutoff
          deleted `shouldBe` 1
          remainingId `shouldBe` Just recurringId

    it "cleanupOldMeetings never picks up open-ended recurring meetings" $ do
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          meeting <- createMeeting zUser (Just (ConnId "test-conn")) Nothing (futureMeeting openEndedRecurrence)
          passTime validityWindow
          -- Even with a cutoff well past the endTime, open-ended
          -- recurrence is never picked up.
          deleted <- cleanupOldMeetings (addUTCTime validityWindow now) 100
          remaining <- getMeeting zUser meeting.meeting.id
          pure (deleted, fmap (.id) remaining, meeting.meeting.id)
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (deleted, remainingId, meetingId) -> do
          deleted `shouldBe` 0
          remainingId `shouldBe` Just meetingId

    it "cleanupOldMeetings deletes in effectiveEndTime order, not endTime order" $ do
      result <-
        runTestStack now gen Map.empty teamConfig $ do
          -- endTime now+8000, no recurrence -> effectiveEndTime now+8000 (earliest)
          plain <- createMeeting zUser (Just (ConnId "test-conn")) Nothing (meetingAt 8000 Nothing)
          -- endTime now+4000, until now+10000 -> effectiveEndTime now+10000 (later)
          recur <- createMeeting zUser (Just (ConnId "test-conn")) Nothing (meetingAt 4000 (recurUntil (addUTCTime 10000 now)))
          _deleted <- cleanupOldMeetings (addUTCTime 11000 now) 1
          plainRemains <- isJust <$> getMeeting zUser plain.meeting.id
          recurRemains <- isJust <$> getMeeting zUser recur.meeting.id
          pure (plainRemains, recurRemains)
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (plainRemains, recurRemains) -> do
          -- plain has the earlier effectiveEndTime, so it is deleted first;
          -- recur survives. With the old endTime sort, recur (endTime now+4000)
          -- would be deleted first instead.
          plainRemains `shouldBe` False
          recurRemains `shouldBe` True

    prop "aliveness follows effectiveEndTime across get/list/cleanup" $
      \(recurrence :: Maybe API.Recurrence) (advance :: NonNegative Int) ->
        let startTime = addUTCTime 3600 now
            endTime = addUTCTime 7200 now
            nm =
              API.NewMeeting
                { title = fromJust $ checked "Recurring Meeting",
                  startTime = startTime,
                  endTime = addUTCTime 3600 startTime,
                  tzid = API.defaultLegacyTimeZone,
                  mtype = API.Scheduled,
                  externallyManagedBy = Nothing,
                  externallyCalendarEventId = Nothing,
                  externallyCalendarId = Nothing,
                  password = Nothing,
                  recurrence = recurrence,
                  invitedEmails = []
                }
            advanceTime = fromIntegral (getNonNegative advance) :: NominalDiffTime
            -- After advancing the clock, the validity cutoff moves forward.
            cutoff = addUTCTime (advanceTime - 3600) now
            effEnd = maybe (Just endTime) (\r -> max endTime <$> r.until) recurrence
            alive = maybe True (>= cutoff) effEnd
         in ioProperty $ do
              result <-
                runTestStack now gen Map.empty teamConfig $ do
                  meeting <- createMeeting zUser (Just (ConnId "test-conn")) Nothing nm
                  passTime advanceTime
                  fetched <- isJust <$> getMeeting zUser meeting.meeting.id
                  listedCount <- length <$> listMeetings zUser
                  deleted <- cleanupOldMeetings cutoff 100
                  remains <- isJust <$> getMeeting zUser meeting.meeting.id
                  pure (fetched, listedCount, deleted, remains)
              pure $ case result of
                Left err -> counterexample ("Unexpected error: " <> show err) False
                Right (fetched, listedCount, deleted, remains) ->
                  (fetched === alive)
                    .&&. (listedCount === if alive then 1 else 0)
                    .&&. (deleted === if alive then 0 else 1)
                    .&&. (remains === alive)

  describe "checkMeetingsEnabled" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uidPersonal = Id $ read "00000000-0000-0000-0000-000000000001"
        uidTeam = Id $ read "00000000-0000-0000-0000-000000000002"
        zUserPersonal = toLocalUnsafe (Domain "wire.com") uidPersonal
        zUserTeam = toLocalUnsafe (Domain "wire.com") uidTeam
        teamId = Id $ read "00000000-0000-0000-0000-000000000100"
        teamMember = mkTeamMember uidTeam fullPermissions Nothing UserLegalHoldDisabled
        meetingsEnabled =
          npUpdate @MeetingsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) def
        meetingsDisabled =
          npUpdate @MeetingsConfig (LockableFeature FeatureStatusDisabled LockStatusUnlocked def) def
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Test Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              tzid = API.defaultLegacyTimeZone,
              mtype = API.Scheduled,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              password = Nothing,
              recurrence = Nothing,
              invitedEmails = []
            }

    it "allows operations for personal user even when meetings disabled" $ do
      result <-
        runTestStack now gen Map.empty meetingsDisabled $
          createMeeting zUserPersonal (Just (ConnId "test-conn")) Nothing newMeeting

      result `shouldSatisfy` isRight

    it "allows operations for team user with meetings enabled" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember]) meetingsEnabled $
          createMeeting zUserTeam (Just (ConnId "test-conn")) Nothing newMeeting

      result `shouldSatisfy` isRight

    it "throws MeetingsFeatureDisabled on createMeeting for team user with meetings disabled" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember]) meetingsDisabled $
          createMeeting zUserTeam (Just (ConnId "test-conn")) Nothing newMeeting

      result `shouldBe` Left MeetingsFeatureDisabled

    it "returns Nothing on getMeeting for team user with meetings disabled" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember]) meetingsEnabled $
          createMeeting zUserTeam (Just (ConnId "test-conn")) Nothing newMeeting

      case result of
        Left err -> fail $ "Failed to create meeting: " <> show err
        Right meeting -> do
          result2 <-
            runTestStack now gen (Map.singleton teamId [teamMember]) meetingsDisabled $
              getMeeting zUserTeam meeting.meeting.id

          result2 `shouldBe` Right Nothing

    it "throws MeetingsFeatureDisabled on updateMeeting for team user with meetings disabled" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember]) meetingsEnabled $
          createMeeting zUserTeam (Just (ConnId "test-conn")) Nothing newMeeting

      case result of
        Left err -> fail $ "Failed to create meeting: " <> show err
        Right meeting -> do
          result2 <-
            runTestStack now gen (Map.singleton teamId [teamMember]) meetingsDisabled $
              updateMeeting zUserTeam (Just (ConnId "test-conn")) Nothing meeting.meeting.id (API.UpdateMeeting Nothing Nothing (Just (unsafeRange "Updated")) Nothing Nothing)

          result2 `shouldBe` Left MeetingsFeatureDisabled

    it "throws MeetingsFeatureDisabled on deleteMeeting for team user with meetings disabled" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember]) meetingsEnabled $
          createMeeting zUserTeam (Just (ConnId "test-conn")) Nothing newMeeting

      case result of
        Left err -> fail $ "Failed to create meeting: " <> show err
        Right meeting -> do
          result2 <-
            runTestStack now gen (Map.singleton teamId [teamMember]) meetingsDisabled $
              deleteMeeting zUserTeam (Just (ConnId "test-conn")) Nothing meeting.meeting.id

          result2 `shouldBe` Left MeetingsFeatureDisabled

    it "returns [] on listMeetings for team user with meetings disabled" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember]) meetingsDisabled $
          listMeetings zUserTeam

      result `shouldBe` Right []

    it "throws MeetingsFeatureDisabled on addInvitedEmails for team user with meetings disabled" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember]) meetingsEnabled $
          createMeeting zUserTeam (Just (ConnId "test-conn")) Nothing newMeeting

      case result of
        Left err -> fail $ "Failed to create meeting: " <> show err
        Right meeting -> do
          result2 <-
            runTestStack now gen (Map.singleton teamId [teamMember]) meetingsDisabled $
              addInvitedEmails zUserTeam (Just (ConnId "test-conn")) Nothing meeting.meeting.id [unsafeEmailAddress "test" "example.com"] Nothing

          result2 `shouldBe` Left MeetingsFeatureDisabled

    it "throws MeetingsFeatureDisabled on removeInvitedEmails for team user with meetings disabled" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember]) meetingsEnabled $
          createMeeting zUserTeam (Just (ConnId "test-conn")) Nothing newMeeting

      case result of
        Left err -> fail $ "Failed to create meeting: " <> show err
        Right meeting -> do
          result2 <-
            runTestStack now gen (Map.singleton teamId [teamMember]) meetingsDisabled $
              removeInvitedEmails zUserTeam (Just (ConnId "test-conn")) Nothing meeting.meeting.id [unsafeEmailAddress "test" "example.com"]

          result2 `shouldBe` Left MeetingsFeatureDisabled

    it "throws MeetingsFeatureDisabled on replaceInvitedEmails for team user with meetings disabled" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember]) meetingsEnabled $
          createMeeting zUserTeam (Just (ConnId "test-conn")) Nothing newMeeting

      case result of
        Left err -> fail $ "Failed to create meeting: " <> show err
        Right meeting -> do
          result2 <-
            runTestStack now gen (Map.singleton teamId [teamMember]) meetingsDisabled $
              replaceInvitedEmails zUserTeam (Just (ConnId "test-conn")) Nothing meeting.meeting.id [unsafeEmailAddress "test" "example.com"] Nothing

          result2 `shouldBe` Left MeetingsFeatureDisabled

  describe "meeting events" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid1 = Id $ read "00000000-0000-0000-0000-000000000001"
        uid2 = Id $ read "00000000-0000-0000-0000-000000000002"
        zUser1 = toLocalUnsafe (Domain "wire.com") uid1
        zUser2 = toLocalUnsafe (Domain "wire.com") uid2
        teamId = Id $ read "00000000-0000-0000-0000-000000000100"
        teamMember1 = mkTeamMember uid1 fullPermissions Nothing UserLegalHoldDisabled
        teamMember2 = mkTeamMember uid2 fullPermissions Nothing UserLegalHoldDisabled
        teamConfig =
          npUpdate @MeetingsConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) def
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Event Test Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              tzid = API.defaultLegacyTimeZone,
              mtype = API.Scheduled,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              password = Nothing,
              recurrence = Nothing,
              invitedEmails = []
            }

    it "emits a meeting.create event on successful create" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember1]) teamConfig $ do
          meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
          pushes <- get @[Push]
          pure (meeting, pushes)

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (meeting, pushes) -> do
          let events = extractMeetingEvents pushes
          length events `shouldBe` 1
          (head events).evtType `shouldBe` MeetingEvent.Create
          (head events).evtMeeting `shouldBe` meeting.meeting.id

    it "emits a meeting.update event on successful update" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember1]) teamConfig $ do
          meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
          put @[Push] []
          _ <- updateMeeting zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id (API.UpdateMeeting Nothing Nothing (Just (unsafeRange "Updated")) Nothing Nothing)
          get @[Push]

      case result of
        Left err -> fail $ "Error: " <> show err
        Right pushes -> do
          let events = extractMeetingEvents pushes
          length events `shouldBe` 1
          (head events).evtType `shouldBe` MeetingEvent.Update

    it "emits a meeting.delete event on successful delete" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember1]) teamConfig $ do
          meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
          put @[Push] []
          _ <- deleteMeeting zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id
          get @[Push]

      case result of
        Left err -> fail $ "Error: " <> show err
        Right pushes -> do
          let events = extractMeetingEvents pushes
          length events `shouldBe` 1
          (head events).evtType `shouldBe` MeetingEvent.Delete

    it "does not emit meeting.delete when the conversation-deletion preflight fails" $ do
      -- The creator is removed from the conversation members, so the mock
      -- conversation subsystem denies CheckDeleteLocalConversation and
      -- deleteMeeting must abort before publishing any event (WPB-29046).
      -- The event ordering itself (meeting.delete before
      -- conversation.delete-meeting) is asserted end-to-end by the
      -- integration test in integration/test/Test/Meetings.hs.
      result <-
        runTestStackWithURI now gen (Left testCodeURIBase) (Map.singleton teamId [teamMember1]) teamConfig fullLicense $ do
          meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
          -- Force the mock to deny the preflight (mock-only semantics: the
          -- creator is not in the conversation members set)
          modify @(Map ConvId (Set UserId)) $
            Map.adjust (Set.delete uid1) (qUnqualified meeting.meeting.conversationId)
          put @[Push] []
          void $ deleteMeeting zUser1 (Just (ConnId "test-conn")) Nothing meeting.meeting.id

      case result of
        Left _denied -> pure ()
        Right (Left err) -> fail $ "unexpected MeetingError: " <> show err
        Right (Right _) -> fail "deleteMeeting succeeded despite the denied preflight"

    it "does not emit an event when updateMeeting fails (non-creator)" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember1, teamMember2]) teamConfig $ do
          meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
          put @[Push] []
          _ <- updateMeeting zUser2 (Just (ConnId "test-conn")) Nothing meeting.meeting.id (titleUpdate (unsafeRange "Hijack"))
          get @[Push]

      case result of
        Left err -> fail $ "Error: " <> show err
        Right pushes -> extractMeetingEvents pushes `shouldBe` []

    it "does not emit an event when deleteMeeting fails (non-creator)" $ do
      result <-
        runTestStack now gen (Map.singleton teamId [teamMember1, teamMember2]) teamConfig $ do
          meeting <- createMeeting zUser1 (Just (ConnId "test-conn")) Nothing newMeeting
          put @[Push] []
          _ <- deleteMeeting zUser2 (Just (ConnId "test-conn")) Nothing meeting.meeting.id
          get @[Push]

      case result of
        Left err -> fail $ "Error: " <> show err
        Right pushes -> extractMeetingEvents pushes `shouldBe` []

    it "delivers lifecycle events to all members and propagates conn" $ do
      let members =
            [ newMember uid1,
              newMember uid2
            ]
          meetingId = Id $ read "00000000-0000-0000-0000-000000000070"
          convId = Id $ read "00000000-0000-0000-0000-000000000071"
          qMeetingId = Qualified meetingId (Domain "wire.com")
          qConvId = Qualified convId (Domain "wire.com")
          originConn = ConnId "origin"
      result <-
        runTestStack now gen Map.empty def $ do
          _ <- notifyMeetingEvent zUser1 (Just originConn) members qConvId Nothing MeetingEvent.Create qMeetingId
          get @[Push]
      case result of
        Left err -> fail $ "Error: " <> show err
        Right pushes -> do
          let push = head pushes
          -- Recipient order is preserved: 'members' is a literal list and
          -- mkMeetingEventPush maps 'localMemberToRecipient' over it.
          map (.recipientUserId) push.recipients `shouldBe` [uid1, uid2]
          push.conn `shouldBe` Just originConn
          push.route `shouldBe` PushV2.RouteAny

  describe "V16 operations" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        startT = addUTCTime 3600 now
        endT = addUTCTime 7200 now
        newLegacy =
          API.NewMeetingV16
            { startTime = startT,
              endTime = endT,
              recurrence = Nothing,
              title = fromJust $ checked "Legacy Meeting",
              invitedEmails = []
            }

    it "createMeetingV16 then getMeetingV16 returns the requested end_time" $ do
      result <-
        runTestStack now gen Map.empty def $ do
          mwc <- createMeetingV16 zUser (ConnId "test-conn") newLegacy
          getMeetingV16 zUser mwc.meeting.id
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (Just lm) -> lm.endTime `shouldBe` endT
        Right Nothing -> fail "expected a meeting"

    it "a meeting created via the new shape is visible to legacy clients with the equivalent end_time" $ do
      let nm =
            API.NewMeeting
              { title = fromJust $ checked "New Meeting",
                startTime = startT,
                endTime = addUTCTime 3600 startT,
                tzid = API.defaultLegacyTimeZone,
                mtype = API.Scheduled,
                externallyManagedBy = Nothing,
                externallyCalendarEventId = Nothing,
                externallyCalendarId = Nothing,
                password = Nothing,
                recurrence = Nothing,
                invitedEmails = []
              }
      result <-
        runTestStack now gen Map.empty def $ do
          mwc <- createMeeting zUser (Just (ConnId "test-conn")) Nothing nm
          getMeetingV16 zUser mwc.meeting.id
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (Just lm) -> lm.endTime `shouldBe` endT
        Right Nothing -> fail "expected a meeting"

    it "a meeting created via the legacy shape is visible to new clients with end_time and the default tzid" $ do
      result <-
        runTestStack now gen Map.empty def $ do
          mwc <- createMeetingV16 zUser (ConnId "test-conn") newLegacy
          getMeeting zUser mwc.meeting.id
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (Just m) -> do
          m.endTime `shouldBe` endT
          m.tzid `shouldBe` API.defaultLegacyTimeZone
        Right Nothing -> fail "expected a meeting"

    it "updateMeetingV16 moving both start and end keeps the requested end_time" $ do
      let newStart = addUTCTime 8000 now
          newEnd = addUTCTime 9000 now
          upd =
            API.UpdateMeeting
              { startTime = Just newStart,
                endTime = Just newEnd,
                title = Nothing,
                recurrence = Nothing,
                tzid = Nothing
              }
      result <-
        runTestStack now gen Map.empty def $ do
          mwc <- createMeetingV16 zUser (ConnId "test-conn") newLegacy
          updateMeetingV16 zUser (ConnId "test-conn") mwc.meeting.id upd
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (Just mwc') -> mwc'.meeting.endTime `shouldBe` newEnd
        Right Nothing -> fail "expected the update to apply"

  describe "meeting type (V19)" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        newMeeting mty =
          API.NewMeeting
            { title = fromJust $ checked "Typed Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              tzid = API.defaultLegacyTimeZone,
              mtype = mty,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              recurrence = Nothing,
              invitedEmails = [],
              password = Nothing
            }
        newMeetingV18 =
          API.NewMeetingV18
            { title = fromJust $ checked "V18 Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              tzid = API.defaultLegacyTimeZone,
              recurrence = Nothing,
              invitedEmails = []
            }

    it "createMeeting with immediate stores and returns the type" $ do
      result <-
        runTestStack now gen Map.empty def $ do
          mwc <- createMeeting zUser (Just (ConnId "test-conn")) Nothing (newMeeting API.Immediate)
          getMeeting zUser mwc.meeting.id
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (Just m) -> m.mtype `shouldBe` API.Immediate
        Right Nothing -> fail "expected the meeting to be retrievable"

    it "createMeetingV18 defaults the stored type to scheduled" $ do
      result <-
        runTestStack now gen Map.empty def $ do
          mwc <- createMeetingV18 zUser (ConnId "test-conn") newMeetingV18
          getMeeting zUser mwc.meeting.id
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (Just m) -> m.mtype `shouldBe` API.Scheduled
        Right Nothing -> fail "expected the meeting to be retrievable"

    it "updateMeeting preserves the stored type" $ do
      result <-
        runTestStack now gen Map.empty def $ do
          mwc <- createMeeting zUser (Just (ConnId "test-conn")) Nothing (newMeeting API.Immediate)
          _ <- updateMeeting zUser (Just (ConnId "test-conn")) Nothing mwc.meeting.id (API.UpdateMeeting Nothing Nothing (Just (fromJust $ checked "Renamed")) Nothing Nothing)
          getMeeting zUser mwc.meeting.id
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (Just m) -> do
          m.mtype `shouldBe` API.Immediate
          m.title `shouldBe` fromJust (checked "Renamed")
        Right Nothing -> fail "expected the meeting to be retrievable"

  specStoredMeetingUnmarshall
  specMeetingEmails
  specCalendarManagedMeetings

specStoredMeetingUnmarshall :: Spec
specStoredMeetingUnmarshall =
  describe "StoredMeeting postgres unmarshall" $ do
    -- A backfilled row (tzid NOT NULL, end_time set): end_time is the truth
    -- and tzid is read straight off the column.
    it "parses end_time and tzid for a backfilled row" $ do
      let t0 = UTCTime (fromGregorian 2026 1 1) 0
          t1 = addUTCTime 3600 t0
          legacyRow :: Store.StoredMeetingTuple
          legacyRow =
            ( nil,
              "t",
              nil,
              t0,
              t1,
              "Europe/Berlin",
              "scheduled",
              Nothing,
              Nothing,
              Nothing,
              nil,
              V.empty,
              False,
              t0,
              t0,
              Nothing,
              Nothing,
              Nothing
            )
      let result :: Either Text Store.StoredMeeting
          result = postgresUnmarshall legacyRow
      case result of
        Left e -> expectationFailure $ "unmarshall failed: " <> show e
        Right sm -> do
          sm.endTime `shouldBe` t1
          sm.tzid `shouldBe` API.defaultLegacyTimeZone
          sm.meetingType `shouldBe` API.Scheduled

    it "rejects a non-null but unparseable tzid instead of swallowing it" $ do
      let t0 = UTCTime (fromGregorian 2026 1 1) 0
          t1 = addUTCTime 3600 t0
          badTzRow :: Store.StoredMeetingTuple
          badTzRow =
            ( nil,
              "t",
              nil,
              t0,
              t1,
              "not-a-zone",
              "scheduled",
              Nothing,
              Nothing,
              Nothing,
              nil,
              V.empty,
              False,
              t0,
              t0,
              Nothing,
              Nothing,
              Nothing
            )
      let result :: Either Text Store.StoredMeeting
          result = postgresUnmarshall badTzRow
      case result of
        Left msg -> msg `shouldBe` ("invalid tzid" :: Text)
        Right _ -> expectationFailure "expected unmarshall to reject an invalid tzid"

    it "rejects an unparseable mtype instead of swallowing it" $ do
      let t0 = UTCTime (fromGregorian 2026 1 1) 0
          t1 = addUTCTime 3600 t0
          badMtypeRow :: Store.StoredMeetingTuple
          badMtypeRow =
            ( nil,
              "t",
              nil,
              t0,
              t1,
              "Europe/Berlin",
              "not-a-type",
              Nothing,
              Nothing,
              Nothing,
              nil,
              V.empty,
              False,
              t0,
              t0,
              Nothing,
              Nothing,
              Nothing
            )
      let result :: Either Text Store.StoredMeeting
          result = postgresUnmarshall badMtypeRow
      case result of
        Left msg -> msg `shouldBe` ("invalid mtype" :: Text)
        Right _ -> expectationFailure "expected unmarshall to reject an invalid mtype"

    it "parses calendar columns and the outlook externally_managed_by value" $ do
      let t0 = UTCTime (fromGregorian 2026 1 1) 0
          t1 = addUTCTime 3600 t0
          row :: Store.StoredMeetingTuple
          row =
            ( nil,
              "t",
              nil,
              t0,
              t1,
              "Europe/Berlin",
              "scheduled",
              Nothing,
              Nothing,
              Nothing,
              nil,
              V.empty,
              False,
              t0,
              t0,
              Just "outlook",
              Just "outlook-evt-1",
              Just "outlook-cal-1"
            )
      let result :: Either Text Store.StoredMeeting
          result = postgresUnmarshall row
      case result of
        Left e -> expectationFailure $ "unmarshall failed: " <> show e
        Right sm -> do
          sm.externallyManagedBy `shouldBe` Just (API.ExternalManager "outlook")
          sm.externallyCalendarEventId `shouldBe` Just "outlook-evt-1"
          sm.externallyCalendarId `shouldBe` Just "outlook-cal-1"

specMeetingEmails :: Spec
specMeetingEmails =
  describe "meeting emails" $ do
    let now0 = UTCTime (fromGregorian 2026 1 1) 0
        gen0 = mkStdGen 42
        uid1 = Id $ read "00000000-0000-0000-0000-000000000001"
        uid2 = Id $ read "00000000-0000-0000-0000-000000000002"
        dom = Domain "wire.com"
        zUser1 = toLocalUnsafe dom uid1
        zUser2 = toLocalUnsafe dom uid2
        testConnId = ConnId "test-conn"
        addrA = unsafeEmailAddress "a" "example.com"
        addrB = unsafeEmailAddress "b" "example.com"
        addrC = unsafeEmailAddress "c" "example.com"
        addrD = unsafeEmailAddress "d" "example.com"
        hostEmail = unsafeEmailAddress "host" "example.com"
        hostLocale = def :: Locale

        mkNewMeeting :: API.MeetingType -> [EmailAddress] -> API.NewMeeting
        mkNewMeeting mtype invited =
          API.NewMeeting
            { title = fromJust $ checked "Email Meeting",
              startTime = addUTCTime 3600 now0,
              endTime = addUTCTime 7200 now0,
              tzid = API.defaultLegacyTimeZone,
              mtype = mtype,
              externallyManagedBy = Nothing,
              externallyCalendarEventId = Nothing,
              externallyCalendarId = Nothing,
              password = Nothing,
              recurrence = Nothing,
              invitedEmails = invited
            }

        meetingCode mid = do
          key <- CodeStore.makeKey (CodeReferentMeeting mid)
          mCode <- gets @(Map Key (Code, Maybe Password)) (Map.lookup key)
          maybe (error "no meeting code stored") (pure . fst) mCode

    it "sends one invitation RPC to brig covering all invited addresses on create" $ do
      result <- runTestStack now0 gen0 Map.empty def $ do
        plantHost uid1 (Just hostEmail) hostLocale
        meeting <- createMeeting zUser1 (Just testConnId) Nothing (mkNewMeeting API.Scheduled [addrA, addrB])
        rpcs <- sentRpcs
        pure (rpcs, meeting.meeting)
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (rpcs, m) -> do
          length rpcs `shouldBe` 1
          case rpcs of
            [rpc] -> do
              rpc.kind `shouldBe` MeetingEmailInvitation
              rpc.changeSummary `shouldBe` Nothing
              rpc.recipients `shouldBe` [addrA, addrB]
              rpc.meeting `shouldBe` m
              rpc.hostName `shouldBe` "Alice Host"
              rpc.hostEmail `shouldBe` hostEmail
              rpc.locale `shouldBe` Just hostLocale
            _ -> expectationFailure "expected exactly one RPC"

    it "sends no RPC for immediate meetings" $ do
      result <- runTestStack now0 gen0 Map.empty def $ do
        plantHost uid1 (Just hostEmail) hostLocale
        _ <- createMeeting zUser1 (Just testConnId) Nothing (mkNewMeeting API.Immediate [addrA])
        totalRecipients
      case result of
        Left err -> fail $ "Error: " <> show err
        Right total -> total `shouldBe` 0

    it "sends an update RPC with change summary on a real change and none on a no-op update" $ do
      result <- runTestStack now0 gen0 Map.empty def $ do
        plantHost uid1 (Just hostEmail) hostLocale
        meeting <- createMeeting zUser1 (Just testConnId) Nothing (mkNewMeeting API.Scheduled [addrA, addrB])
        updated <- updateMeeting zUser1 (Just testConnId) Nothing meeting.meeting.id (titleUpdate (unsafeRange "Renamed"))
        rpcsAfterChange <- sentRpcs
        _ <- updateMeeting zUser1 (Just testConnId) Nothing meeting.meeting.id (titleUpdate (unsafeRange "Renamed"))
        rpcsAfterNoop <- sentRpcs
        pure (rpcsAfterChange, rpcsAfterNoop, updated, meeting.meeting)
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (rpcsAfterChange, rpcsAfterNoop, Just updated, createdMeeting) -> do
          length rpcsAfterChange `shouldBe` 2
          case drop 1 rpcsAfterChange of
            [rpc] -> do
              rpc.kind `shouldBe` MeetingEmailUpdate
              rpc.changeSummary `shouldBe` Just "Title changed"
              rpc.recipients `shouldBe` [addrA, addrB]
              rpc.meeting `shouldBe` updated.meeting
            _ -> expectationFailure "expected exactly one update RPC"
          -- The invitation RPC carries the meeting at creation time.
          head rpcsAfterChange `shouldSatisfy` \rpc -> rpc.meeting == createdMeeting
          rpcsAfterNoop `shouldBe` rpcsAfterChange
        Right (_, _, Nothing, _) -> expectationFailure "update unexpectedly returned Nothing"

    it "sends a \'Join link changed\' update RPC on refreshMeetingLink" $ do
      result <- runTestStack now0 gen0 Map.empty def $ do
        plantHost uid1 (Just hostEmail) hostLocale
        meeting <- createMeeting zUser1 (Just testConnId) Nothing (mkNewMeeting API.Scheduled [addrA])
        refreshed <- refreshMeetingLink zUser1 (Just testConnId) Nothing meeting.meeting.id (API.RefreshMeetingLinkRequest Nothing)
        rpcs <- sentRpcs
        pure (rpcs, refreshed)
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (rpcs, Just refreshed) -> do
          length rpcs `shouldBe` 2
          case drop 1 rpcs of
            [rpc] -> do
              rpc.kind `shouldBe` MeetingEmailUpdate
              rpc.changeSummary `shouldBe` Just "Join link changed"
              rpc.recipients `shouldBe` [addrA]
              -- The update RPC carries the fresh (rotated) link.
              rpc.meeting.link `shouldBe` refreshed.meeting.link
            _ -> expectationFailure "expected exactly one update RPC"
        Right (_, Nothing) -> expectationFailure "refresh unexpectedly returned Nothing"

    it "sends a cancellation RPC on delete" $ do
      result <- runTestStack now0 gen0 Map.empty def $ do
        plantHost uid1 (Just hostEmail) hostLocale
        meeting <- createMeeting zUser1 (Just testConnId) Nothing (mkNewMeeting API.Scheduled [addrA, addrB])
        deleted <- deleteMeeting zUser1 (Just testConnId) Nothing meeting.meeting.id
        rpcs <- sentRpcs
        pure (deleted, rpcs, meeting.meeting)
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (deleted, rpcs, m) -> do
          deleted `shouldBe` True
          length rpcs `shouldBe` 2
          case drop 1 rpcs of
            [rpc] -> do
              rpc.kind `shouldBe` MeetingEmailCancel
              rpc.changeSummary `shouldBe` Nothing
              rpc.recipients `shouldBe` [addrA, addrB]
              rpc.meeting `shouldBe` m
            _ -> expectationFailure "expected exactly one cancel RPC"

    it "sends invitations only for newly added addresses (add/replace/remove)" $ do
      result <- runTestStack now0 gen0 Map.empty def $ do
        plantHost uid1 (Just hostEmail) hostLocale
        meeting <- createMeeting zUser1 (Just testConnId) Nothing (mkNewMeeting API.Scheduled [addrA])
        _ <- addInvitedEmails zUser1 (Just testConnId) Nothing meeting.meeting.id [addrB, addrC] Nothing
        _ <- replaceInvitedEmails zUser1 (Just testConnId) Nothing meeting.meeting.id [addrC, addrD] Nothing
        _ <- removeInvitedEmails zUser1 (Just testConnId) Nothing meeting.meeting.id [addrD]
        rpcs <- sentRpcs
        pure (rpcs, meeting.meeting)
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (rpcs, m) -> do
          length rpcs `shouldBe` 3
          -- addrA only in the create invitation; addrB added by add; addrC by
          -- add; addrD only ever invited by replace.
          concatMap (.recipients) rpcs `shouldBe` [addrA, addrB, addrC, addrD]
          all (\rpc -> rpc.kind == MeetingEmailInvitation) rpcs `shouldBe` True
          all (\rpc -> rpc.meeting.id == m.id) rpcs `shouldBe` True

    it "uses the caller-provided locale for invitation emails" $ do
      let customLocale = Locale (Language DE) Nothing
      result <- runTestStack now0 gen0 Map.empty def $ do
        plantHost uid1 (Just hostEmail) hostLocale
        meeting <- createMeeting zUser1 (Just testConnId) Nothing (mkNewMeeting API.Scheduled [addrA])
        _ <- addInvitedEmails zUser1 (Just testConnId) Nothing meeting.meeting.id [addrB] (Just customLocale)
        sentRpcs
      case result of
        Left err -> fail $ "Error: " <> show err
        Right rpcs -> case drop 1 rpcs of
          [rpc] -> rpc.locale `shouldBe` Just customLocale
          _ -> expectationFailure "expected exactly one invitation RPC after create"

    it "falls back to the creator's locale when the request carries none" $ do
      result <- runTestStack now0 gen0 Map.empty def $ do
        plantHost uid1 (Just hostEmail) hostLocale
        meeting <- createMeeting zUser1 (Just testConnId) Nothing (mkNewMeeting API.Scheduled [addrA])
        _ <- addInvitedEmails zUser1 (Just testConnId) Nothing meeting.meeting.id [addrB] Nothing
        sentRpcs
      case result of
        Left err -> fail $ "Error: " <> show err
        Right rpcs -> case drop 1 rpcs of
          [rpc] -> rpc.locale `shouldBe` Just hostLocale
          _ -> expectationFailure "expected exactly one invitation RPC after create"

    it "does not re-invite an address that is already invited (add)" $ do
      result <- runTestStack now0 gen0 Map.empty def $ do
        plantHost uid1 (Just hostEmail) hostLocale
        meeting <- createMeeting zUser1 (Just testConnId) Nothing (mkNewMeeting API.Scheduled [addrA])
        _ <- addInvitedEmails zUser1 (Just testConnId) Nothing meeting.meeting.id [addrA, addrA, addrB] Nothing
        rpcs <- sentRpcs
        pure (rpcs, meeting.meeting)
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (rpcs, m) -> do
          length rpcs `shouldBe` 2
          -- addrA only sees the invitation from createMeeting.
          map (.recipients) rpcs `shouldBe` [[addrA], [addrB]]
          all (\rpc -> rpc.meeting == m) rpcs `shouldBe` True

    it "sends no RPC when the creator has no email address" $ do
      result <- runTestStack now0 gen0 Map.empty def $ do
        plantHost uid1 Nothing hostLocale
        _ <- createMeeting zUser1 (Just testConnId) Nothing (mkNewMeeting API.Scheduled [addrA])
        totalRecipients
      case result of
        Left err -> fail $ "Error: " <> show err
        Right total -> total `shouldBe` 0

    it "sends no RPC for operations by a non-creator" $ do
      result <- runTestStack now0 gen0 Map.empty def $ do
        plantHost uid1 (Just hostEmail) hostLocale
        plantHost uid2 (Just hostEmail) hostLocale
        meeting <- createMeeting zUser1 (Just testConnId) Nothing (mkNewMeeting API.Scheduled [addrA])
        _ <- updateMeeting zUser2 (Just testConnId) Nothing meeting.meeting.id (titleUpdate (unsafeRange "Hijack"))
        deleted <- deleteMeeting zUser2 (Just testConnId) Nothing meeting.meeting.id
        rpcs <- sentRpcs
        pure (deleted, rpcs)
      case result of
        Left err -> fail $ "Error: " <> show err
        Right (deleted, rpcs) -> do
          deleted `shouldBe` False
          -- The create still sent its invitation; the denied update/delete
          -- must not have added any RPC.
          length rpcs `shouldBe` 1

    it "sends no RPC when the license lacks the email flag" $ do
      let noEmailLicense = fullLicense {meetings = (meetings fullLicense) {emailsSending = False}}
      result <- runTestStackWithURI now0 gen0 (Left testCodeURIBase) Map.empty def noEmailLicense $ do
        plantHost uid1 (Just hostEmail) hostLocale
        _ <- createMeeting zUser1 (Just testConnId) Nothing (mkNewMeeting API.Scheduled [addrA, addrB])
        totalRecipients
      case result of
        Right (Right total) -> total `shouldBe` 0
        Right (Left err) -> fail $ "Error: " <> show err
        Left _denied -> expectationFailure "unexpected CheckDeleteLocalConversation denial"

    it "sends one invitation per unique address even with duplicated recipients" $ do
      result <- runTestStack now0 gen0 Map.empty def $ do
        plantHost uid1 (Just hostEmail) hostLocale
        _ <- createMeeting zUser1 (Just testConnId) Nothing (mkNewMeeting API.Scheduled [addrA, addrA, addrB])
        sentRpcs
      case result of
        Right [rpc] ->
          rpc.recipients `shouldBe` [addrA, addrB]
        Right other -> expectationFailure $ "expected exactly one RPC, got: " <> show (length (other :: [SendMeetingEmails]))
        Left err -> fail $ "Error: " <> show err

    it "sends no RPC when meeting email settings are unconfigured" $ do
      let cfg =
            MeetingSystemConfig
              { legacyTimeZone = API.defaultLegacyTimeZone,
                validityPeriod = 3600,
                pastEditPeriod = configuredPastEditPeriod,
                emailsEnabled = False
              }
      result <- runTestStackWithURICfg now0 gen0 (Left testCodeURIBase) Map.empty def fullLicense cfg $ do
        plantHost uid1 (Just hostEmail) hostLocale
        _ <- createMeeting zUser1 (Just testConnId) Nothing (mkNewMeeting API.Scheduled [addrA, addrB])
        totalRecipients
      case result of
        Right (Right total) -> total `shouldBe` 0
        Right (Left err) -> fail $ "Error: " <> show err
        Left _denied -> expectationFailure "unexpected CheckDeleteLocalConversation denial"

    it "sends no RPC when the host profile lookup fails (brig error)" $ do
      result <- runTestStack now0 gen0 Map.empty def $ do
        plantHost uid1 (Just hostEmail) hostLocale
        put True -- brig mock fails GetUsers
        _ <- createMeeting zUser1 (Just testConnId) Nothing (mkNewMeeting API.Scheduled [addrA, addrB])
        totalRecipients
      case result of
        Right total -> do
          -- The create must succeed despite the failing brig lookup, and no
          -- RPC may be issued.
          total `shouldBe` 0
        Left err -> fail $ "Error: " <> show err

    it "still deletes the meeting when the join code is already gone (no cancel RPC)" $ do
      result <- runTestStack now0 gen0 Map.empty def $ do
        plantHost uid1 (Just hostEmail) hostLocale
        meeting <- createMeeting zUser1 (Just testConnId) Nothing (mkNewMeeting API.Scheduled [addrA])
        CodeStore.deleteMeetingCode (qUnqualified meeting.meeting.id)
        deleted <- deleteMeeting zUser1 (Just testConnId) Nothing meeting.meeting.id
        rpcs <- sentRpcs
        pure (deleted, rpcs)
      case result of
        Right (deleted, rpcs) -> do
          -- Deletion succeeds despite the missing code: email preparation is
          -- best-effort and must not fail the meeting operation.
          deleted `shouldBe` True
          -- Only the invitation from create; no cancellation RPC.
          length rpcs `shouldBe` 1
        Left err -> fail $ "Error: " <> show err

    it "sends RPCs and gates join links independently of each other\'s flags" $ do
      let emailsOnlyLicense = fullLicense {meetings = (meetings fullLicense) {invitationLinks = False}}
      result <- runTestStackWithURI now0 gen0 (Left testCodeURIBase) Map.empty def emailsOnlyLicense $ do
        plantHost uid1 (Just hostEmail) hostLocale
        meeting <- createMeeting zUser1 (Just testConnId) Nothing (mkNewMeeting API.Scheduled [addrA])
        code <- meetingCode (qUnqualified meeting.meeting.id)
        joinOutcome <-
          catch
            (Right <$> joinMeeting zUser2 testConnId (Domain "wire.com") code.codeKey code.codeValue Nothing)
            (pure . Left)
        rpcs <- sentRpcs
        pure (joinOutcome, rpcs)
      case result of
        Right (Right (joinOutcome, rpcs)) -> do
          joinOutcome `shouldBe` Left MeetingNotLicensed
          case rpcs of
            [rpc] -> rpc.kind `shouldBe` MeetingEmailInvitation
            _ -> expectationFailure "expected exactly one invitation RPC"
        Right (Left err) -> fail $ "Error: " <> show err
        Left _denied -> expectationFailure "unexpected CheckDeleteLocalConversation denial"

    it "leaves the meeting operation successful when the SendMeetingEmails RPC fails" $ do
      result <- runTestStackWithURICfg now0 gen0 (Left testCodeURIBase) Map.empty def fullLicense (MeetingSystemConfig {legacyTimeZone = API.defaultLegacyTimeZone, validityPeriod = 3600, pastEditPeriod = configuredPastEditPeriod, emailsEnabled = True}) $ do
        plantHost uid1 (Just hostEmail) hostLocale
        put (FailSendMeetingEmails True)
        created <- createMeeting zUser1 (Just testConnId) Nothing (mkNewMeeting API.Scheduled [addrA])
        deleted <- deleteMeeting zUser1 (Just testConnId) Nothing created.meeting.id
        pure (created, deleted)
      case result of
        Right (Right (_created, deleted)) ->
          -- The failing dispatch is swallowed (warn-logged in a detached
          -- child): both operations return normally.
          deleted `shouldBe` True
        Right (Left err) -> fail $ "Error: " <> show err
        Left _denied -> expectationFailure "unexpected CheckDeleteLocalConversation denial"

    it "leaves the meeting operation successful when the dispatch RPC throws an IO exception" $ do
      -- Pins the meeting-operation outcome: the detached child's raw IO
      -- exception must not affect createMeeting (the child itself discards
      -- its result, so pre- and post-fix this succeeds either way; the
      -- warn-logging of the transport failure is the behavioral change).
      result <- runTestStackWithURICfg now0 gen0 (Left testCodeURIBase) Map.empty def fullLicense (MeetingSystemConfig {legacyTimeZone = API.defaultLegacyTimeZone, validityPeriod = 3600, pastEditPeriod = configuredPastEditPeriod, emailsEnabled = True}) $ do
        plantHost uid1 (Just hostEmail) hostLocale
        -- Set before the fork: the child snapshots the pure state at fork time.
        put (FailSendMeetingEmailsIO True)
        createMeeting zUser1 (Just testConnId) Nothing (mkNewMeeting API.Scheduled [addrA])
      case result of
        Right (Right _created) -> pure ()
        Right (Left err) -> fail $ "Error: " <> show err
        Left _denied -> expectationFailure "unexpected CheckDeleteLocalConversation denial"

-- | Plant a brig user record for the meeting creator in the mock brig state.
plantHost :: UserId -> Maybe EmailAddress -> Locale -> Sem TestStack ()
plantHost uid email loc = do
  u <- liftIO (mkHostUser uid "Alice Host" email loc)
  modify (Map.insert uid u)

-- | All recorded @SendMeetingEmails@ RPCs, in dispatch order. Polls until the
-- recorder is stable (three consecutive identical reads, 50 ms apart): the
-- dispatch runs in a detached async child, so a bare read races the child.
sentRpcs :: Sem TestStack [SendMeetingEmails]
sentRpcs = go Nothing (0 :: Int) (0 :: Int)
  where
    go prev stable iters
      | iters > 100 = reverseCurrent
      | otherwise = do
          rpcs <- readRpcs
          if Just rpcs == prev && stable >= 3
            then pure rpcs
            else do
              embed (threadDelay 50000)
              go (Just rpcs) (if Just rpcs == prev then stable + 1 else 0) (iters + 1)
      where
        reverseCurrent = readRpcs
    readRpcs = do
      ref <- get @(IORef [SendMeetingEmails])
      embed (reverse <$> readIORef ref)

-- | Total number of (address, RPC) deliveries issued to brig.
totalRecipients :: Sem TestStack Int
totalRecipients = sum . map (length . (.recipients)) <$> sentRpcs

specCalendarManagedMeetings :: Spec
specCalendarManagedMeetings =
  describe "calendar-managed meetings" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid1 = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser1 = toLocalUnsafe (Domain "wire.com") uid1
        teamId = Id $ read "00000000-0000-0000-0000-000000000100"
        teamMember1 = mkTeamMember uid1 fullPermissions Nothing UserLegalHoldDisabled
        calNewMeeting mgr =
          API.NewMeeting
            { title = fromJust $ checked "Calendar Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              tzid = API.defaultLegacyTimeZone,
              mtype = API.Scheduled,
              externallyManagedBy = mgr,
              externallyCalendarEventId = Just "evt-1",
              externallyCalendarId = Just "cal-1",
              password = Nothing,
              recurrence = Nothing,
              invitedEmails = []
            }
        wireNewMeeting = (calNewMeeting Nothing) {API.externallyCalendarEventId = Nothing, API.externallyCalendarId = Nothing}
        noCalendarsLicense =
          fullLicense
            { meetings =
                (meetings fullLicense) {calendarsIntegration = False}
            }
        meetingsEnabledConfig =
          npUpdate @MeetingsConfig
            (LockableFeature FeatureStatusEnabled LockStatusUnlocked def)
            $ def

    it "creates a google_calendar-managed meeting for an OAuth caller with the Z-Client header" $ do
      result <-
        runTestStackWithURI now gen (Left testCodeURIBase) (Map.singleton teamId [teamMember1]) meetingsEnabledConfig fullLicense $
          createMeeting zUser1 Nothing (Just "Calendar") (calNewMeeting (Just (API.ExternalManager "google_calendar")))
      case result of
        Right (Left err) -> fail $ "expected success, got: " <> show err
        Right (Right _mwc) -> pure ()
        Left _denied -> fail "unexpected CheckDeleteLocalConversation denial"

    it "treats any non-null externally_managed_by as externally-managed (free text, not value-validated)" $ do
      result <-
        runTestStackWithURI now gen (Left testCodeURIBase) (Map.singleton teamId [teamMember1]) meetingsEnabledConfig fullLicense $
          createMeeting zUser1 Nothing (Just "Calendar") (calNewMeeting (Just (API.ExternalManager "any-cal")))
      case result of
        Right (Left err) -> fail $ "expected success, got: " <> show err
        Right (Right mwc) -> mwc.meeting.externallyManagedBy `shouldBe` Just (API.ExternalManager "any-cal")
        Left _denied -> fail "unexpected CheckDeleteLocalConversation denial"

    it "echoes externally_managed_by on re-fetch" $ do
      result <-
        runTestStackWithURI now gen (Left testCodeURIBase) (Map.singleton teamId [teamMember1]) meetingsEnabledConfig fullLicense $ do
          meeting <- createMeeting zUser1 Nothing (Just "Calendar") (calNewMeeting (Just (API.ExternalManager "google_calendar")))
          fetched <- getMeeting zUser1 meeting.meeting.id
          pure (meeting.meeting.externallyManagedBy, fmap (.externallyManagedBy) fetched)
      case result of
        Right (Right (Just (API.ExternalManager "google_calendar"), Just (Just (API.ExternalManager "google_calendar")))) -> pure ()
        other -> fail $ "expected the echo on both create and fetch, got: " <> show other

    it "rejects wire-client creation of an externally-managed meeting" $ do
      result <-
        runTestStackWithURI now gen (Left testCodeURIBase) (Map.singleton teamId [teamMember1]) meetingsEnabledConfig fullLicense $
          createMeeting zUser1 (Just (ConnId "test-conn")) Nothing (calNewMeeting (Just (API.ExternalManager "google_calendar")))
      case result of
        Right (Left ManagedByCalendar) -> pure ()
        other -> fail $ "expected ManagedByCalendar, got: " <> show other

    it "allows the calendar integration (no connection) to delete an externally-managed meeting" $ do
      result <-
        runTestStackWithURI now gen (Left testCodeURIBase) (Map.singleton teamId [teamMember1]) meetingsEnabledConfig fullLicense $ do
          mwc <- createMeeting zUser1 Nothing (Just "Calendar") (calNewMeeting (Just (API.ExternalManager "google_calendar")))
          deleteMeeting zUser1 Nothing (Just "Calendar") ((mwc.meeting).id)
      case result of
        Right (Right True) -> pure ()
        other -> fail $ "expected successful delete, got: " <> show other

    it "rejects wire-client refresh, invitation-add and delete of an externally-managed meeting" $ do
      -- Throws from the interpreter escape handlers inside the interpreted
      -- computation and are surfaced as @Left@ by the test runner; each
      -- rejected mutation runs in its own stack seeded by a fresh meeting.
      let expectRejected action = do
            r <-
              runTestStackWithURI
                now
                gen
                (Left testCodeURIBase)
                (Map.singleton teamId [teamMember1])
                meetingsEnabledConfig
                fullLicense
                $ do
                  mwc <- createMeeting zUser1 Nothing (Just "Calendar") (calNewMeeting (Just (API.ExternalManager "google_calendar")))
                  void $ action ((mwc.meeting).id)
            case r of
              Right (Left ManagedByCalendar) -> pure ()
              other -> fail $ "expected ManagedByCalendar, got: " <> show other
      expectRejected $ \m ->
        void $ refreshMeetingLink zUser1 (Just (ConnId "test-conn")) Nothing m (API.RefreshMeetingLinkRequest Nothing)
      expectRejected $ \m ->
        void $ addInvitedEmails zUser1 (Just (ConnId "test-conn")) Nothing m [unsafeEmailAddress "someone" "example.com"] Nothing
      expectRejected $ \m ->
        void $ deleteMeeting zUser1 (Just (ConnId "test-conn")) Nothing m

    it "externally-managed meeting create dispatches no meeting emails" $ do
      let addr = unsafeEmailAddress "invitee" "example.com"
          extMgr = Just (API.ExternalManager "google_calendar")
          calNew = calNewMeeting extMgr
          nm =
            API.NewMeeting
              { title = calNew.title,
                startTime = calNew.startTime,
                endTime = calNew.endTime,
                tzid = calNew.tzid,
                mtype = calNew.mtype,
                externallyManagedBy = calNew.externallyManagedBy,
                externallyCalendarEventId = calNew.externallyCalendarEventId,
                externallyCalendarId = calNew.externallyCalendarId,
                password = calNew.password,
                recurrence = calNew.recurrence,
                invitedEmails = [addr]
              }
      result <-
        runTestStackWithURI now gen (Left testCodeURIBase) (Map.singleton teamId [teamMember1]) meetingsEnabledConfig fullLicense $ do
          plantHost uid1 (Just (unsafeEmailAddress "host" "example.com")) (def :: Locale)
          _ <- createMeeting zUser1 Nothing (Just "Calendar") nm
          sentRpcs
      case result of
        Right (Right rpcs) -> rpcs `shouldBe` []
        Right (Left err) -> fail $ "expected success, got: " <> show err
        Left _denied -> fail "unexpected CheckDeleteLocalConversation denial"

    it "externally-managed invitation add dispatches no meeting emails" $ do
      let addr2 = unsafeEmailAddress "invitee2" "example.com"
      result <-
        runTestStackWithURI now gen (Left testCodeURIBase) (Map.singleton teamId [teamMember1]) meetingsEnabledConfig fullLicense $ do
          plantHost uid1 (Just (unsafeEmailAddress "host" "example.com")) (def :: Locale)
          mwc <- createMeeting zUser1 Nothing (Just "Calendar") (calNewMeeting (Just (API.ExternalManager "google_calendar")))
          success <- addInvitedEmails zUser1 Nothing (Just "Calendar") ((mwc.meeting).id) [addr2] Nothing
          rpcs <- sentRpcs
          pure (success, rpcs)
      case result of
        Right (Right (True, rpcs)) -> rpcs `shouldBe` []
        Right (Right (False, _)) -> fail "expected addInvitedEmails to succeed"
        Right (Left err) -> fail $ "expected success, got: " <> show err
        Left _denied -> fail "unexpected CheckDeleteLocalConversation denial"

    it "creates an outlook-managed meeting for an OAuth caller with the Z-Client header" $ do
      result <-
        runTestStackWithURI now gen (Left testCodeURIBase) (Map.singleton teamId [teamMember1]) meetingsEnabledConfig fullLicense $
          createMeeting zUser1 Nothing (Just "Calendar") (calNewMeeting (Just (API.ExternalManager "outlook")))
      case result of
        Right (Left err) -> fail $ "expected success, got: " <> show err
        Right (Right _mwc) -> pure ()
        Left _denied -> fail "unexpected CheckDeleteLocalConversation denial"

    it "rejects a calendar-managed create when the license lacks calendarsIntegration" $ do
      result <-
        runTestStackWithURI now gen (Left testCodeURIBase) (Map.singleton teamId [teamMember1]) meetingsEnabledConfig noCalendarsLicense $
          createMeeting zUser1 Nothing (Just "Calendar") (calNewMeeting (Just (API.ExternalManager "google_calendar")))
      case result of
        Right (Left MeetingNotLicensed) -> pure ()
        other -> fail $ "expected MeetingNotLicensed, got: " <> show other

    it "rejects a calendar-managed create without the Z-Client header" $ do
      result <-
        runTestStackWithURI now gen (Left testCodeURIBase) (Map.singleton teamId [teamMember1]) meetingsEnabledConfig fullLicense $
          createMeeting zUser1 Nothing Nothing (calNewMeeting (Just (API.ExternalManager "google_calendar")))
      case result of
        Right (Left InvalidCalendarFields) -> pure ()
        other -> fail $ "expected InvalidCalendarFields, got: " <> show other

    it "rejects wire-managed creates carrying the Z-Client header or calendar fields" $ do
      r1 <-
        runTestStackWithURI now gen (Left testCodeURIBase) (Map.singleton teamId [teamMember1]) meetingsEnabledConfig fullLicense $
          createMeeting zUser1 Nothing (Just "Calendar") wireNewMeeting
      case r1 of
        Right (Left InvalidCalendarFields) -> pure ()
        other -> fail $ "expected InvalidCalendarFields, got: " <> show other
      r2 <-
        runTestStackWithURI now gen (Left testCodeURIBase) (Map.singleton teamId [teamMember1]) meetingsEnabledConfig fullLicense $
          createMeeting zUser1 Nothing Nothing (calNewMeeting Nothing)
      case r2 of
        Right (Left InvalidCalendarFields) -> pure ()
        other -> fail $ "expected InvalidCalendarFields, got: " <> show other

    it "guards mutations of calendar-managed meetings by caller kind and header" $ do
      -- Each mutation runs in its own stack (errors abort the interpreted
      -- computation and the in-memory store is per-run), each seeded by a
      -- fresh calendar-managed meeting.
      let withCalMeeting action =
            runTestStackWithURI
              now
              gen
              (Left testCodeURIBase)
              (Map.singleton teamId [teamMember1])
              meetingsEnabledConfig
              fullLicense
              $ do
                mwc <- createMeeting zUser1 Nothing (Just "Calendar") (calNewMeeting (Just (API.ExternalManager "google_calendar")))
                action ((mwc.meeting).id)
      -- OAuth caller with the header may update.
      ok <- withCalMeeting $ \mid ->
        updateMeeting zUser1 Nothing (Just "Calendar") mid (API.UpdateMeeting Nothing Nothing (Just (fromJust $ checked "By Calendar")) Nothing Nothing)
      case ok of
        Right (Right (Just _)) -> pure ()
        other -> fail $ "expected successful update, got: " <> show other
      -- OAuth caller without the header is rejected.
      noHeader <- withCalMeeting $ \mid ->
        updateMeeting zUser1 Nothing Nothing mid (API.UpdateMeeting Nothing Nothing (Just (fromJust $ checked "Hijack")) Nothing Nothing)
      case noHeader of
        Right (Left InvalidCalendarFields) -> pure ()
        other -> fail $ "expected InvalidCalendarFields, got: " <> show other
      -- A wire caller (connection present) is rejected as managed-by-calendar.
      viaWire <- withCalMeeting $ \mid ->
        updateMeeting zUser1 (Just (ConnId "test-conn")) Nothing mid (API.UpdateMeeting Nothing Nothing (Just (fromJust $ checked "Hijack")) Nothing Nothing)
      case viaWire of
        Right (Left ManagedByCalendar) -> pure ()
        other -> fail $ "expected ManagedByCalendar, got: " <> show other

-- | Synchronize with 'Wire.MeetingsSubsystem.Interpreter.startTimeTolerance'
expectedStartTimeTolerance :: NominalDiffTime
expectedStartTimeTolerance = 60

-- | Past-edit window configured in 'runTestStack'.
configuredPastEditPeriod :: NominalDiffTime
configuredPastEditPeriod = 3600

-- | Validity window, beyond this one-time meeting belong to the past
validityWindow :: NominalDiffTime
validityWindow = 11000

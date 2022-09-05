module Test.Spar.Scim.UserSpec where

import Arbitrary ()
import Brig.Types.Intra
import Brig.Types.User
import Control.Monad.Except (runExceptT)
import Data.Id
import qualified Data.Json.Util
import Imports
import Polysemy
import Polysemy.TinyLog
import Spar.Scim.User (deleteScimUser)
import Spar.Sem.BrigAccess
import Spar.Sem.IdPConfigStore
import Spar.Sem.IdPConfigStore.Mem (TypedState, idPToMem)
import Spar.Sem.SAMLUserStore
import Spar.Sem.SAMLUserStore.Mem (UserRefOrd, samlUserStoreToMem)
import qualified Spar.Sem.ScimExternalIdStore as ScimExternalIdStore
import Spar.Sem.ScimExternalIdStore.Mem (scimExternalIdStoreToMem)
import Spar.Sem.ScimUserTimesStore
import Spar.Sem.ScimUserTimesStore.Mem (scimUserTimesStoreToMem)
import System.Logger (Msg)
import Test.Hspec
import Test.QuickCheck
import Web.Scim.Schema.Error
import Wire.API.User
import qualified Wire.API.User.Identity
import Wire.API.User.Scim
import Wire.Sem.Logger.TinyLog (discardTinyLogs)

spec :: Spec
spec = describe "deleteScimUser" $ do
  it "returns no error when the account was deleted for the first time (or partially)" $ do
    uid <- generate arbitrary
    tokenInfo <- generate arbitrary
    r <-
      interpretWithBrigAccessMock
        (mockBrigForActiveUser tokenInfo AccountDeleted)
        (deleteUserAndAssertDeletionInSpar uid tokenInfo)
    handlerResult r `shouldBe` Right ()
  it "returns an error when the account was deleted before" $ do
    uid <- generate arbitrary
    tokenInfo <- generate arbitrary
    r <-
      interpretWithBrigAccessMock
        (mockBrigForActiveUser tokenInfo AccountAlreadyDeleted)
        (deleteUserAndAssertDeletionInSpar uid tokenInfo)
    handlerResult r `shouldBe` Left (notFound "user" (idToText uid))
  it "returns an error when there never was an account" $ do
    uid <- generate arbitrary
    tokenInfo <- generate arbitrary
    r <-
      interpretWithBrigAccessMock
        mockBrigForNonExistendUser
        (runExceptT $ deleteScimUser tokenInfo uid)
    handlerResult r `shouldBe` Left (notFound "user" (idToText uid))

deleteUserAndAssertDeletionInSpar ::
  forall (r :: EffectRow).
  Members
    '[ Logger (Msg -> Msg),
       BrigAccess,
       ScimExternalIdStore.ScimExternalIdStore,
       ScimUserTimesStore,
       SAMLUserStore,
       IdPConfigStore,
       Embed IO
     ]
    r =>
  UserId ->
  ScimTokenInfo ->
  Sem r (Either ScimError ())
deleteUserAndAssertDeletionInSpar uid tokenInfo = do
  let tid = stiTeam tokenInfo
      email = (fromJust . parseEmail) "someone@wire.com"
  ScimExternalIdStore.insert tid email uid
  r <- runExceptT $ deleteScimUser tokenInfo uid
  lr <- ScimExternalIdStore.lookup tid email
  liftIO $ lr `shouldBe` Nothing
  pure r

type EffsWithoutBrigAccess =
  '[ IdPConfigStore,
     SAMLUserStore,
     ScimUserTimesStore,
     ScimExternalIdStore.ScimExternalIdStore,
     Logger (Msg -> Msg),
     Embed IO,
     Final IO
   ]

type Effs = BrigAccess ': EffsWithoutBrigAccess

type InterpreterState =
  ( Map (Data.Id.TeamId, Wire.API.User.Identity.Email) Data.Id.UserId,
    ( Map Data.Id.UserId (Data.Json.Util.UTCTimeMillis, Data.Json.Util.UTCTimeMillis),
      ( Map UserRefOrd UserId,
        (Spar.Sem.IdPConfigStore.Mem.TypedState, Either ScimError ())
      )
    )
  )

handlerResult :: InterpreterState -> Either ScimError ()
handlerResult = snd . snd . snd . snd

interpretWithBrigAccessMock ::
  ( Sem Effs (Either ScimError ()) ->
    Sem EffsWithoutBrigAccess (Either ScimError ())
  ) ->
  Sem Effs (Either ScimError ()) ->
  IO InterpreterState
interpretWithBrigAccessMock mock =
  runFinal
    . embedToFinal @IO
    . discardTinyLogs
    . scimExternalIdStoreToMem
    . scimUserTimesStoreToMem
    . samlUserStoreToMem
    . idPToMem
    . mock

mockBrigForNonExistendUser ::
  forall (r1 :: EffectRow).
  Members
    '[ Logger (Msg -> Msg),
       ScimExternalIdStore.ScimExternalIdStore,
       ScimUserTimesStore,
       SAMLUserStore,
       IdPConfigStore,
       Embed IO
     ]
    r1 =>
  Sem (BrigAccess ': r1) (Either ScimError ()) ->
  Sem r1 (Either ScimError ())
mockBrigForNonExistendUser = interpret $ \case
  (GetAccount WithPendingInvitations _) -> pure Nothing
  (Spar.Sem.BrigAccess.DeleteUser _) -> pure NoUser
  _ -> do
    liftIO $ expectationFailure $ "Unexpected effect (call to brig)"
    error "Throw error here to avoid implementation of all cases."

mockBrigForActiveUser ::
  forall (r1 :: EffectRow).
  Members
    '[ Logger (Msg -> Msg),
       ScimExternalIdStore.ScimExternalIdStore,
       ScimUserTimesStore,
       SAMLUserStore,
       IdPConfigStore,
       Embed IO
     ]
    r1 =>
  ScimTokenInfo ->
  DeleteUserResult ->
  Sem (BrigAccess ': r1) (Either ScimError ()) ->
  Sem r1 (Either ScimError ())
mockBrigForActiveUser tokenInfo deletionResult = interpret $ \case
  (GetAccount WithPendingInvitations uid) -> do
    acc <- liftIO $ someActiveUser uid tokenInfo
    pure $ Just acc
  (Spar.Sem.BrigAccess.DeleteUser _) -> pure deletionResult
  _ -> do
    liftIO $ expectationFailure $ "Unexpected effect (call to brig)"
    error "Throw error here to avoid implementation of all cases."

someActiveUser :: UserId -> ScimTokenInfo -> IO UserAccount
someActiveUser uid tokenInfo = do
  user <- generate arbitrary
  pure $
    UserAccount
      { accountStatus = Active,
        accountUser =
          user
            { userDisplayName = Name "default",
              userAccentId = defaultAccentId,
              userPict = noPict,
              userAssets = [],
              userHandle = Nothing,
              userLocale = defLoc,
              userIdentity = (Just . EmailIdentity . fromJust . parseEmail) "someone@wire.com",
              userId = uid,
              userTeam = Just $ stiTeam tokenInfo
            }
      }
  where
    defLoc = fromJust $ parseLocale "De-de"

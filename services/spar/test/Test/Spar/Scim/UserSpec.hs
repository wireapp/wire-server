module Test.Spar.Scim.UserSpec where

import Arbitrary ()
import Brig.Types.Intra
import Brig.Types.User
import Control.Monad.Except (runExceptT)
import Data.Handle (parseHandle)
import Data.Id
import Imports
import Polysemy
import Polysemy.TinyLog
import Spar.Scim.User (deleteScimUser)
import Spar.Sem.BrigAccess
import Spar.Sem.IdPConfigStore
import Spar.Sem.IdPConfigStore.Mem (idPToMem)
import Spar.Sem.SAMLUserStore
import Spar.Sem.SAMLUserStore.Mem (samlUserStoreToMem)
import qualified Spar.Sem.ScimExternalIdStore as ScimExternalIdStore
import Spar.Sem.ScimExternalIdStore.Mem (scimExternalIdStoreToMem)
import Spar.Sem.ScimUserTimesStore
import Spar.Sem.ScimUserTimesStore.Mem (scimUserTimesStoreToMem)
import System.Logger (Msg)
import Test.Hspec
import Test.QuickCheck
import Web.Scim.Schema.Error
import Wire.API.User
import Wire.API.User.Scim
import Wire.Sem.Logger.TinyLog (discardTinyLogs)

spec :: Spec
spec = describe "deleteScimUser" $ do
  it "returns no error when the account was deleted for the first time (or partially)" $ do
    tokenInfo <- generate arbitrary
    acc <- someActiveUser tokenInfo
    r <-
      interpretWithBrigAccessMock
        (mockBrig (withActiveUser acc) AccountDeleted)
        (deleteUserAndAssertDeletionInSpar acc.account tokenInfo)
    r `shouldBe` Right ()
  it "is idempotent" $ do
    tokenInfo <- generate arbitrary
    acc <- someActiveUser tokenInfo
    r <-
      interpretWithBrigAccessMock
        (mockBrig (withActiveUser acc) AccountAlreadyDeleted)
        (deleteUserAndAssertDeletionInSpar acc.account tokenInfo)
    r `shouldBe` Right ()
  it "works if there never was an account" $ do
    uid <- generate arbitrary
    tokenInfo <- generate arbitrary
    r <-
      interpretWithBrigAccessMock
        (mockBrig (const Nothing) NoUser)
        (runExceptT $ deleteScimUser tokenInfo uid)
    r `shouldBe` Right ()
  it "returns no error when there was a partially deleted account" $ do
    uid <- generate arbitrary
    tokenInfo <- generate arbitrary
    r <-
      interpretWithBrigAccessMock
        (mockBrig (const Nothing) AccountDeleted)
        (runExceptT $ deleteScimUser tokenInfo uid)
    r `shouldBe` Right ()

deleteUserAndAssertDeletionInSpar ::
  forall (r :: EffectRow).
  ( Members
      '[ Logger (Msg -> Msg),
         BrigAccess,
         ScimExternalIdStore.ScimExternalIdStore,
         ScimUserTimesStore,
         SAMLUserStore,
         IdPConfigStore,
         Embed IO
       ]
      r
  ) =>
  UserAccount ->
  ScimTokenInfo ->
  Sem r (Either ScimError ())
deleteUserAndAssertDeletionInSpar acc tokenInfo = do
  let tid = stiTeam tokenInfo
      email = (fromJust . emailIdentity . fromJust . userIdentity . accountUser) acc
      uid = (userId . accountUser) acc
  ScimExternalIdStore.insert tid (fromEmail email) uid
  r <- runExceptT $ deleteScimUser tokenInfo uid
  lr <- ScimExternalIdStore.lookup tid (fromEmail email)
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

interpretWithBrigAccessMock ::
  ( Sem (BrigAccess ': EffsWithoutBrigAccess) a ->
    Sem EffsWithoutBrigAccess a
  ) ->
  Sem (BrigAccess ': EffsWithoutBrigAccess) a ->
  IO a
interpretWithBrigAccessMock mock =
  runFinal
    . embedToFinal @IO
    . discardTinyLogs
    . ignoringState scimExternalIdStoreToMem
    . ignoringState scimUserTimesStoreToMem
    . ignoringState samlUserStoreToMem
    . ignoringState idPToMem
    . mock

ignoringState :: (Functor f) => (a -> f (c, b)) -> a -> f b
ignoringState f = fmap snd . f

mockBrig ::
  forall (r :: EffectRow) a.
  (Member (Embed IO) r) =>
  (UserId -> Maybe ExtendedUserAccount) ->
  DeleteUserResult ->
  Sem (BrigAccess ': r) a ->
  Sem r a
mockBrig lookup_user delete_response = interpret $ \case
  (GetAccount WithPendingInvitations uid) -> pure $ lookup_user uid
  (Spar.Sem.BrigAccess.DeleteUser _) -> pure delete_response
  _ -> do
    liftIO $ expectationFailure $ "Unexpected effect (call to brig)"
    error "Throw error here to avoid implementation of all cases."

withActiveUser :: ExtendedUserAccount -> UserId -> Maybe ExtendedUserAccount
withActiveUser acc uid =
  if uid == (userId . accountUser) acc.account
    then Just acc
    else Nothing

someActiveUser :: ScimTokenInfo -> IO ExtendedUserAccount
someActiveUser tokenInfo = do
  user <- generate arbitrary
  pure $
    ExtendedUserAccount
      { account =
          UserAccount
            { accountStatus = Active,
              accountUser =
                user
                  { userDisplayName = Name "Some User",
                    userAccentId = defaultAccentId,
                    userPict = noPict,
                    userAssets = [],
                    userHandle = parseHandle "some-handle",
                    userIdentity = (Just . EmailIdentity . fromJust . emailAddressText) "someone@wire.com",
                    userTeam = Just $ stiTeam tokenInfo
                  }
            },
        emailUnvalidated = Nothing
      }

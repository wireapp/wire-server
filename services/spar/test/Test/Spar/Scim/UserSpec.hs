module Test.Spar.Scim.UserSpec where

import Arbitrary ()
import Brig.Types.Intra (AccountStatus (Deleted), UserAccount (..))
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
import Spar.Sem.ScimExternalIdStore
import Spar.Sem.ScimExternalIdStore.Mem (scimExternalIdStoreToMem)
import Spar.Sem.ScimUserTimesStore
import Spar.Sem.ScimUserTimesStore.Mem (scimUserTimesStoreToMem)
import System.Logger (Msg)
import Test.Hspec
import Test.QuickCheck
import qualified Web.Scim.Handler as Scim
import Web.Scim.Schema.Error (ScimError (..))
import Wire.API.User
import qualified Wire.API.User.Identity
import Wire.Sem.Logger.TinyLog (discardTinyLogs)

spec :: Spec
spec = describe "deleteScimUser" $ do
  it "runs deletion for deleted brig users again" $ do
    uid <- generate arbitrary
    tokenInfo <- generate arbitrary
    void $ (simulateDeletedBrigUser . toSem) $ deleteScimUser tokenInfo uid

toSem ::
  forall (r :: EffectRow).
  Members
    '[ Logger (Msg -> Msg),
       BrigAccess,
       ScimExternalIdStore,
       ScimUserTimesStore,
       SAMLUserStore,
       IdPConfigStore
     ]
    r =>
  Scim.ScimHandler (Sem r) () ->
  Sem r (Either ScimError ())
toSem = runExceptT

type Effs =
  '[ BrigAccess,
     IdPConfigStore,
     SAMLUserStore,
     ScimUserTimesStore,
     ScimExternalIdStore,
     Logger (Msg -> Msg),
     Embed IO,
     Final IO
   ]

type InterpreterState =
  ( Map (Data.Id.TeamId, Wire.API.User.Identity.Email) Data.Id.UserId,
    ( Map Data.Id.UserId (Data.Json.Util.UTCTimeMillis, Data.Json.Util.UTCTimeMillis),
      ( Map UserRefOrd UserId,
        (Spar.Sem.IdPConfigStore.Mem.TypedState, Either ScimError ())
      )
    )
  )

simulateDeletedBrigUser ::
  Sem Effs (Either ScimError ()) ->
  IO InterpreterState
simulateDeletedBrigUser =
  runFinal
    . embedToFinal @IO
    . discardTinyLogs
    . scimExternalIdStoreToMem
    . scimUserTimesStoreToMem
    . samlUserStoreToMem
    . idPToMem
    . mockBrigForDeletedUser

mockBrigForDeletedUser ::
  forall (r1 :: EffectRow).
  Members
    '[ Logger (Msg -> Msg),
       ScimExternalIdStore,
       ScimUserTimesStore,
       SAMLUserStore,
       IdPConfigStore,
       Embed IO
     ]
    r1 =>
  Sem (BrigAccess ': r1) (Either ScimError ()) ->
  Sem r1 (Either ScimError ())
mockBrigForDeletedUser = interpret $ \case
  (GetAccount WithPendingInvitations uid) -> do
    userAcc <- liftIO $ someUserAccountTombstone uid
    pure $ Just userAcc
  (Spar.Sem.BrigAccess.DeleteUser _) -> pure AccountDeleted
  _ -> do
    liftIO $ expectationFailure $ "Unexpected effect (call to brig)"
    error "Make typechecker happy. This won't be reached."
  where
    someUserAccountTombstone :: UserId -> IO UserAccount
    someUserAccountTombstone uid = do
      user <- generate arbitrary
      pure $
        UserAccount
          { accountStatus = Deleted,
            accountUser =
              user
                { userDisplayName = Name "default",
                  userAccentId = defaultAccentId,
                  userPict = noPict,
                  userAssets = [],
                  userHandle = Nothing,
                  userLocale = defLoc,
                  userIdentity = Nothing,
                  userId = uid
                }
          }
    defLoc = fromJust $ parseLocale "De-de"

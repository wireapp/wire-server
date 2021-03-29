{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Work where

import Brig.Data.Activation (mkActivationKey)
import Brig.Data.UserKey (UserKey, keyText, userEmailKey)
import Brig.Types.Activation (ActivationKey)
import Cassandra
import qualified Cassandra as C
import qualified Cassandra.Settings as C
import Control.Lens ((^.))
import qualified Data.ByteString.Lazy as BL
import Data.Csv (FromField (..), HasHeader (..), decode)
import Data.Id
import Data.Vector
import Imports
import Options
import Options.Applicative
import qualified Spar.Data as SparData
import qualified System.Logger as Log
import Wire.API.User

data Env = Env
  { brigClient :: ClientState,
    sparClient :: ClientState,
    logger :: Log.Logger
  }

main :: IO ()
main = do
  cmd <- execParser (info (helper <*> commandParser) desc)
  lgr <- initLogger
  case cmd of
    (Change tid csvpath connSettings) -> do
      brig <- initCas (connSettings ^. setCasBrig) lgr
      spar <- initCas (connSettings ^. setCasSpar) lgr
      rows <- readCSV csvpath
      let env = Env brig spar lgr
      runChange env tid rows
  where
    desc =
      header "change-external-ids"
        <> progDesc "Change external ids from email to a non-email identifier"
        <> fullDesc
    initLogger =
      Log.new
        . Log.setOutput Log.StdOut
        . Log.setFormat Nothing
        . Log.setBufSize 0
        . Log.setLogLevel Log.Debug
        $ Log.defSettings
    initCas cas l =
      C.init
        . C.setLogger (C.mkLogger l)
        . C.setContacts (cas ^. cHosts) []
        . C.setPortNumber (fromIntegral $ cas ^. cPort)
        . C.setKeyspace (cas ^. cKeyspace)
        . C.setProtocolVersion C.V4
        $ C.defSettings

instance FromField Email where
  parseField bs = do
    txt <- parseField @Text bs
    case parseEmail txt of
      Nothing -> fail "not an email"
      Just email -> pure email

readCSV :: FilePath -> IO (Vector (Email, Text))
readCSV csvpath = do
  content <- BL.readFile csvpath
  case decode NoHeader content of
    Left err -> fail err
    Right rows -> pure rows

runChange :: Env -> TeamId -> Vector (Email, Text) -> IO ()
runChange env tid rows = do
  for_ rows $ \(email, extIdTxt) ->
    changeExtId env tid email (ExternalId tid extIdTxt)

changeExtId :: Env -> TeamId -> Email -> ExternalId -> IO ()
changeExtId env@Env {..} tid email extid = do
  mbUByExt <- lookupExternalId env tid (fromEmail email)
  mbUByEmail <- lookupByEmail env email
  case listToMaybe (catMaybes [mbUByExt, mbUByEmail]) of
    Nothing -> do
      pure ()
    Just uid -> do
      managedBy <- fromMaybe ManagedByWire <$> getUserManagedBy env uid
      case managedBy of
        ManagedByWire -> do
          pure ()
        ManagedByScim -> do
          runClient sparClient $ do
            SparData.insertScimExternalId extid uid
            SparData.deleteScimExternalId (ExternalId tid (fromEmail email))
          let authId = AuthSCIM (ScimDetails extid (EmailWithSource email EmailFromEmailField))
          updateAuthId env uid (Just authId)

lookupExternalId :: Env -> TeamId -> Text -> IO (Maybe UserId)
lookupExternalId Env {..} tid extid =
  runClient sparClient $
    SparData.lookupScimExternalId (ExternalId tid extid)

-- Should behave similar to Brig.API.User.lookupAccountsByIdentity
lookupByEmail :: Env -> Email -> IO (Maybe UserId)
lookupByEmail env email = do
  let uk = userEmailKey email
  activeUid <- lookupKey env uk
  uidFromKey <- lookupActivationCode env uk
  pure (listToMaybe $ catMaybes [activeUid, uidFromKey])

-- -- Should behave similar to Brig.Data.UserKey.lookupKey
lookupKey :: Env -> UserKey -> IO (Maybe UserId)
lookupKey Env {..} k =
  runClient brigClient $
    fmap runIdentity
      <$> retry x1 (query1 keySelect (params Quorum (Identity $ keyText k)))
  where
    keySelect :: PrepQuery R (Identity Text) (Identity UserId)
    keySelect = "SELECT user FROM user_keys WHERE key = ?"

lookupActivationCode :: Env -> UserKey -> IO (Maybe UserId)
lookupActivationCode Env {..} k =
  fmap join $
    fmap runIdentity
      <$> runClient
        brigClient
        ( liftIO (mkActivationKey k)
            >>= retry x1 . query1 codeSelect . params Quorum . Identity
        )
  where
    codeSelect :: PrepQuery R (Identity ActivationKey) (Identity (Maybe UserId))
    codeSelect = "SELECT user FROM activation_keys WHERE key = ?"

updateAuthId :: Env -> UserId -> Maybe AuthId -> IO ()
updateAuthId Env {..} u authid =
  runClient brigClient $
    retry x5 $ write userAuthIdUpdate (params Quorum (authid, u))
  where
    userAuthIdUpdate :: PrepQuery W (Maybe AuthId, UserId) ()
    userAuthIdUpdate = "UPDATE user SET sso_id = ? WHERE id = ?"

getUserManagedBy :: Env -> UserId -> IO (Maybe ManagedBy)
getUserManagedBy Env {..} u = do
  fmap runIdentity
    <$> runClient
      brigClient
      ( retry x5 $ query1 selectManagedBy (params Quorum (Identity u))
      )
  where
    selectManagedBy :: PrepQuery R (Identity UserId) (Identity ManagedBy)
    selectManagedBy = "SELECT managed_by WHERE id = ?"

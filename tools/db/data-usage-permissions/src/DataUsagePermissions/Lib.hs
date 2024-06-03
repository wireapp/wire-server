{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module DataUsagePermissions.Lib where

import Cassandra as C
import Cassandra.Settings as C
import qualified Data.Aeson as A
import Data.Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.List as CL
import Data.Id
import qualified Data.Map as Map
import DataUsagePermissions.Types
import qualified Database.CQL.Protocol as CQL
import Imports
import Options.Applicative
import qualified System.Logger as Log
import System.Logger.Message ((.=), (~~))
import Wire.API.Properties
import Wire.API.User (AccountStatus (Active))

readUsers :: ClientState -> ConduitM () [UserRow] IO ()
readUsers client =
  transPipe (runClient client) (paginateC selectUsersAll (paramsP One () 1000) x5)
    .| Conduit.map (fmap CQL.asRecord)
  where
    selectUsersAll :: C.PrepQuery C.R () (CQL.TupleType UserRow)
    selectUsersAll =
      "SELECT id, activated, status, team FROM user"

lookupProperty :: ClientState -> UserId -> IO [(PropertyKey, RawPropertyValue)]
lookupProperty client uid = do
  runClient client $ retry x1 (query propertySelect (params One (Identity uid)))
  where
    propertySelect :: PrepQuery R (Identity UserId) (PropertyKey, RawPropertyValue)
    propertySelect = "SELECT key, value FROM properties where user = ?"

lookupDataUsageSettings :: Log.Logger -> ClientState -> UserId -> IO (Maybe WebApp, Bool)
lookupDataUsageSettings logger client uid = do
  props <- lookupProperty client uid
  let propsMap = Map.fromList props
  mWebapp <- case Map.lookup (PropertyKey "webapp") propsMap of
    Just v -> case A.eitherDecode $ rawPropertyBytes v of
      Right wa -> pure $ Just wa
      Left e -> do
        Log.warn logger $
          Log.msg (Log.val "Failed to decode webapp property for user")
            ~~ "uid" .= show uid
            ~~ "error" .= e
            ~~ "property" .= rawPropertyBytes v
        pure Nothing
    Nothing -> pure Nothing
  let mMarketingConsent = Map.lookup (PropertyKey "WIRE_MARKETING_CONSENT") propsMap >>= A.decode . rawPropertyBytes
  let marketingConsent = case mMarketingConsent of
        Just (A.Number 1) -> True
        _ -> False
  pure (mWebapp, marketingConsent)

getUserInfo :: Log.Logger -> ClientState -> UserRow -> IO UserDataPermissionInfo
getUserInfo logger brigClient ur = do
  if not $ isActive
    then pure InactiveUser
    else do
      webAppUser <- toWebAppUser <$> lookupDataUsageSettings logger brigClient ur.id
      if isJust ur.team
        then pure $ Team webAppUser
        else pure $ Personal webAppUser
  where
    isActive :: Bool
    isActive =
      ur.activated && ur.status == Just Active

    toWebAppUser :: (Maybe WebApp, Bool) -> WebAppUser
    toWebAppUser (Just wa, nlConsent) =
      WebAppUser
      (ConsentToNewsLetter nlConsent)
      (UserConsentInfo
        (fromMaybe False $ wa.settings.privacy >>= (.improveWire))
        (fromMaybe False $ wa.settings.privacy >>= (.telemetrySharing))
      )
    toWebAppUser (Nothing, nlConsent) = NoWebUser (ConsentToNewsLetter nlConsent)

process :: Log.Logger -> Maybe Int -> ClientState -> IO ConsentResult
process logger limit brigClient =
  runConduit $
    readUsers brigClient
      .| Conduit.concat
      .| (maybe (Conduit.filter (const True)) Conduit.take limit)
      .| Conduit.mapM (getUserInfo logger brigClient)
      .| forever (CL.isolate 10000 .| (Conduit.foldMap userToConsentResult >>= yield))
      .| Conduit.takeWhile ((> 0) . entriesSearched)
      .| CL.scan (<>) mempty
        `fuseUpstream` Conduit.mapM_ (\r -> Log.info logger $ "intermediate_result" .= show r)

main :: IO ()
main = do
  opts <- execParser (info (helper <*> optsParser) desc)
  logger <- initLogger
  brigClient <- initCas opts.brigDb logger
  putStrLn "scanning users table..."
  res <- process logger opts.limit brigClient
  Log.info logger $ "result" .= show res
  where
    initLogger =
      Log.new
        . Log.setLogLevel Log.Info
        . Log.setOutput Log.StdOut
        . Log.setFormat Nothing
        . Log.setBufSize 0
        $ Log.defSettings
    initCas settings l =
      C.init
        . C.setLogger (C.mkLogger l)
        . C.setContacts settings.host []
        . C.setPortNumber (fromIntegral settings.port)
        . C.setKeyspace settings.keyspace
        . C.setProtocolVersion C.V4
        $ C.defSettings
    desc = header "data-usage-permissions" <> progDesc "check user consent for data usage" <> fullDesc

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.Calling.API
  ( getCallsConfig,
    getCallsConfigV2,
    getAuthenticatedCallsConfig,

    -- * Exposed for testing purposes
    newConfig,
    CallsConfigVersion (..),
    NoTurnServers,
  )
where

import Brig.API.Error
import Brig.API.Handler
import Brig.App
import Brig.Calling
import Brig.Calling qualified as Calling
import Brig.Calling.Internal
import Brig.Effects.SFT
import Brig.Effects.SFTStore
import Brig.Options (ListAllSFTServers (..))
import Brig.Options qualified as Opt
import Control.Error (hush, throwE)
import Control.Lens
import Data.ByteString.Conversion
import Data.ByteString.Lens
import Data.Id
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Misc (HttpsUrl)
import Data.Range
import Data.Text.Ascii (AsciiBase64, encodeBase64)
import Data.Text.Strict.Lens
import Data.Time.Clock.POSIX
import Imports hiding (head)
import OpenSSL.EVP.Digest (Digest, hmacBS)
import Polysemy
import Polysemy.Error qualified as Polysemy
import System.Logger.Class qualified as Log
import System.Random.MWC qualified as MWC
import Wire.API.Call.Config qualified as Public
import Wire.Network.DNS.SRV (srvTarget)

-- | ('UserId', 'ConnId' are required as args here to make sure this is an authenticated end-point.)
getAuthenticatedCallsConfig ::
  ( Member (Embed IO) r,
    Member SFT r,
    Member SFTStore r
  ) =>
  UserId ->
  ClientId ->
  ConnId ->
  Maybe (Range 1 10 Int) ->
  (Handler r) Public.RTCConfiguration
getAuthenticatedCallsConfig u c _ limit = do
  env <- view turnEnv
  staticUrl <- view $ settings . Opt.sftStaticUrl
  sftListAllServers <- fromMaybe Opt.HideAllSFTServers <$> view (settings . Opt.sftListAllServers)
  sftEnv' <- view sftEnv
  enableFederation' <- view enableFederation
  discoveredServers <- turnServersV2 (env ^. turnServers)
  eitherConfig <-
    lift
      . liftSem
      . Polysemy.runError
      $ newConfig
        env
        discoveredServers
        staticUrl
        sftEnv'
        limit
        sftListAllServers
        (AuthenticatedCallsConfig u c enableFederation')
  handleNoTurnServers eitherConfig

-- | ('UserId', 'ConnId' are required as args here to make sure this is an authenticated end-point.)
getCallsConfigV2 ::
  ( Member (Embed IO) r,
    Member SFT r,
    Member SFTStore r
  ) =>
  UserId ->
  ConnId ->
  Maybe (Range 1 10 Int) ->
  (Handler r) Public.RTCConfiguration
getCallsConfigV2 _ _ limit = do
  env <- view turnEnv
  staticUrl <- view $ settings . Opt.sftStaticUrl
  sftListAllServers <- fromMaybe Opt.HideAllSFTServers <$> view (settings . Opt.sftListAllServers)
  sftEnv' <- view sftEnv
  discoveredServers <- turnServersV2 (env ^. turnServers)
  eitherConfig <-
    lift
      . liftSem
      . Polysemy.runError
      $ newConfig env discoveredServers staticUrl sftEnv' limit sftListAllServers CallsConfigV2
  handleNoTurnServers eitherConfig

-- | Throws '500 Internal Server Error' when no turn servers are found. This is
-- done to keep backwards compatibility, the previous code initialized an 'IORef'
-- with an 'error' so reading the 'IORef' threw a 500.
--
-- FUTUREWORK: Making this a '404 Not Found' would be more idiomatic, but this
-- should be done after consulting with client teams.
handleNoTurnServers :: Either NoTurnServers a -> (Handler r) a
handleNoTurnServers (Right x) = pure x
handleNoTurnServers (Left NoTurnServers) = do
  Log.err $ Log.msg (Log.val "Call config requested before TURN URIs could be discovered.")
  throwE $ StdError internalServerError

getCallsConfig ::
  ( Member (Embed IO) r,
    Member SFT r,
    Member SFTStore r
  ) =>
  UserId ->
  ConnId ->
  (Handler r) Public.RTCConfiguration
getCallsConfig _ _ = do
  env <- view turnEnv
  discoveredServers <- turnServersV1 (env ^. turnServers)
  eitherConfig <-
    (dropTransport <$$>)
      . lift
      . liftSem
      . Polysemy.runError
      $ newConfig env discoveredServers Nothing Nothing Nothing HideAllSFTServers CallsConfigDeprecated
  handleNoTurnServers eitherConfig
  where
    -- In order to avoid being backwards incompatible, remove the `transport` query param from the URIs
    dropTransport :: Public.RTCConfiguration -> Public.RTCConfiguration
    dropTransport =
      set
        (Public.rtcConfIceServers . traverse . Public.iceURLs . traverse . Public.turiTransport)
        Nothing

data CallsConfigVersion
  = CallsConfigDeprecated
  | CallsConfigV2
  | AuthenticatedCallsConfig UserId ClientId Bool

data NoTurnServers = NoTurnServers
  deriving (Show)

instance Exception NoTurnServers

-- | FUTUREWORK: It is not reflected in the function type the part of the
-- business logic that says that the SFT static URL parameter cannot be set at
-- the same time as the SFT environment parameter. See how to allow either none
-- to be set or only one of them (perhaps Data.These combined with error
-- handling).
newConfig ::
  ( Member (Embed IO) r,
    Member SFT r,
    Member SFTStore r,
    Member (Polysemy.Error NoTurnServers) r
  ) =>
  Calling.TurnEnv ->
  Discovery (NonEmpty Public.TurnURI) ->
  Maybe HttpsUrl ->
  Maybe SFTEnv ->
  Maybe (Range 1 10 Int) ->
  ListAllSFTServers ->
  CallsConfigVersion ->
  Sem r Public.RTCConfiguration
newConfig env discoveredServers sftStaticUrl mSftEnv limit listAllServers version = do
  -- randomize list of servers (before limiting the list, to ensure not always the same servers are chosen if limit is set)
  randomizedUris <-
    liftIO . randomize
      =<< Polysemy.note NoTurnServers (discoveryToMaybe discoveredServers)
  let limitedUris = case limit of
        Nothing -> randomizedUris
        Just lim -> limitedList randomizedUris lim
  -- randomize again (as limitedList partially re-orders uris)
  finalUris <- liftIO $ randomize limitedUris
  srvs <- for finalUris $ \uri -> do
    u <- liftIO $ genTurnUsername (env ^. turnTokenTTL) (env ^. turnPrng)
    pure . Public.rtcIceServer (pure uri) u $ computeCred (env ^. turnSHA512) (env ^. turnSecret) u

  let staticSft = pure . Public.sftServer <$> sftStaticUrl
  allSrvEntries <-
    fmap join $
      for mSftEnv $
        (unSFTServers <$$>) . fmap discoveryToMaybe . readIORef . sftServers
  srvEntries <- fmap join $
    for mSftEnv $ \actualSftEnv -> liftIO $ do
      let subsetLength = Calling.sftListLength actualSftEnv
      mapM (getRandomElements subsetLength) allSrvEntries

  let enableFederation' = case version of
        CallsConfigDeprecated -> Nothing
        CallsConfigV2 -> Nothing
        AuthenticatedCallsConfig _ _ fed -> Just fed

  mSftServersAll <-
    case version of
      CallsConfigDeprecated -> pure Nothing
      CallsConfigV2 ->
        case (listAllServers, sftStaticUrl) of
          (HideAllSFTServers, _) -> pure Nothing
          (ListAllSFTServers, Nothing) -> pure . pure $ Public.nauthSFTServer . sftServerFromSrvTarget . srvTarget <$> maybe [] toList allSrvEntries
          (ListAllSFTServers, Just url) -> Public.nauthSFTServer <$$$> (hush . unSFTGetResponse <$> sftGetAllServers url)
      AuthenticatedCallsConfig u c _ ->
        case (listAllServers, sftStaticUrl) of
          (HideAllSFTServers, _) -> pure Nothing
          (ListAllSFTServers, Nothing) -> mapM (mapM $ authenticate u c) . pure $ sftServerFromSrvTarget . srvTarget <$> maybe [] toList allSrvEntries
          (ListAllSFTServers, Just url) -> mapM (mapM $ authenticate u c) . hush . unSFTGetResponse =<< sftGetAllServers url

  let mSftServers = staticSft <|> sftServerFromSrvTarget . srvTarget <$$> srvEntries
  pure $ Public.rtcConfiguration srvs mSftServers (env ^. turnConfigTTL) mSftServersAll enableFederation'
  where
    limitedList :: NonEmpty Public.TurnURI -> Range 1 10 Int -> NonEmpty Public.TurnURI
    limitedList uris lim =
      -- assuming limitServers is safe with respect to the length of its return value
      -- (see property tests in brig-types)
      -- since the input is List1 and limit is in Range 1 10
      -- it should also be safe to assume the returning list has length >= 1
      NonEmpty.nonEmpty (Public.limitServers (NonEmpty.toList uris) (fromRange lim))
        & fromMaybe (error "newConfig:limitedList: empty list of servers")
    genUsername :: Word32 -> MWC.GenIO -> IO (POSIXTime, Text)
    genUsername ttl prng = do
      rnd <- view (packedBytes . utf8) <$> replicateM 16 (MWC.uniformR (97, 122) prng)
      t <- fromIntegral . (+ ttl) . round <$> getPOSIXTime
      pure $ (t, rnd)
    genTurnUsername :: Word32 -> MWC.GenIO -> IO Public.TurnUsername
    genTurnUsername = (fmap (uncurry Public.turnUsername) .) . genUsername
    genSFTUsername :: Word32 -> MWC.GenIO -> IO Public.SFTUsername
    genSFTUsername = (fmap (uncurry Public.mkSFTUsername) .) . genUsername
    computeCred :: ToByteString a => Digest -> ByteString -> a -> AsciiBase64
    computeCred dig secret = encodeBase64 . hmacBS dig secret . toByteString'
    authenticate ::
      ( Member (Embed IO) r,
        Member SFTStore r
      ) =>
      UserId ->
      ClientId ->
      Public.SFTServer ->
      Sem r Public.AuthSFTServer
    authenticate u c =
      maybe
        (pure . Public.nauthSFTServer)
        ( \SFTTokenEnv {..} sftsvr -> do
            username <- liftIO $ genSFTUsername sftTokenTTL sftTokenPRNG
            let credential = computeCred sftTokenSHA sftTokenSecret username
            void $ sftStoreCredential u c username credential sftTokenSecondsBeforeNew
            maybe
              (Public.nauthSFTServer sftsvr)
              (uncurry (Public.authSFTServer sftsvr))
              <$> sftGetCredential u c
        )
        (sftToken =<< mSftEnv)

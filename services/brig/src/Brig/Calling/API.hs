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
    getCallsConfigV3,
    base26,
    genTurnUid,

    -- * Exposed for testing purposes
    newConfig,
    newConfigV3,
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
import Brig.Options (ListAllSFTServers (..))
import Brig.Options qualified as Opt
import Control.Error (hush, throwE)
import Control.Lens
import Crypto.Hash qualified as Crypto
import Data.ByteArray (convert)
import Data.ByteString qualified as B
import Data.ByteString.Conversion
import Data.ByteString.Lazy qualified as BL
import Data.Id
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Misc (HttpsUrl)
import Data.Range
import Data.Text.Ascii (AsciiBase64, encodeBase64)
import Data.Time.Clock.POSIX
import Data.UUID qualified as UUID
import Imports hiding (head)
import OpenSSL.EVP.Digest (Digest, hmacBS)
import Polysemy
import Polysemy.Error qualified as Polysemy
import System.Logger.Class qualified as Log
import Wire.API.Call.Config qualified as Public
import Wire.API.Team.Feature
import Wire.Error
import Wire.GalleyAPIAccess (GalleyAPIAccess, getAllTeamFeaturesForUser)
import Wire.Network.DNS.SRV (SrvEntry, srvTarget)
import Wire.SFT

conferenceCallingEnabled :: (Member GalleyAPIAccess r) => UserId -> (Handler r) Bool
conferenceCallingEnabled uid = do
  ccStatus <- lift $ liftSem $ ((.status) . npProject @ConferenceCallingConfig <$> getAllTeamFeaturesForUser (Just uid))
  pure $ case ccStatus of
    FeatureStatusEnabled -> True
    FeatureStatusDisabled -> False

-- | ('UserId', 'ConnId' are required as args here to make sure this is an authenticated end-point.)
getCallsConfigV2 ::
  ( Member (Embed IO) r,
    Member SFT r,
    Member GalleyAPIAccess r
  ) =>
  UserId ->
  ConnId ->
  Maybe (Range 1 10 Int) ->
  (Handler r) Public.RTCConfiguration
getCallsConfigV2 uid _ limit = do
  env <- asks (.turnEnv)
  staticUrl <- asks (.settings.sftStaticUrl)
  sftListAllServers <- fromMaybe Opt.HideAllSFTServers <$> asks (.settings.sftListAllServers)
  sftEnv' <- asks (.sftEnv)
  sftFederation <- asks (.enableSFTFederation)
  discoveredServers <- turnServersV2 (env ^. turnServers)
  shared <- conferenceCallingEnabled uid
  eitherConfig <-
    lift
      . liftSem
      . Polysemy.runError
      $ newConfig uid env discoveredServers staticUrl sftEnv' limit sftListAllServers (CallsConfigV2 sftFederation) shared
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
    Member GalleyAPIAccess r
  ) =>
  UserId ->
  ConnId ->
  (Handler r) Public.RTCConfiguration
getCallsConfig uid _ = do
  env <- asks (.turnEnv)
  discoveredServers <- turnServersV1 (env ^. turnServers)
  shared <- conferenceCallingEnabled uid
  eitherConfig <-
    (dropTransport <$$>)
      . lift
      . liftSem
      . Polysemy.runError
      $ newConfig uid env discoveredServers Nothing Nothing Nothing HideAllSFTServers CallsConfigDeprecated shared
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
  | CallsConfigV2 (Maybe Bool)

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
    Member (Polysemy.Error NoTurnServers) r
  ) =>
  UserId ->
  Calling.TurnEnv ->
  Discovery (NonEmpty Public.TurnURI) ->
  Maybe HttpsUrl ->
  Maybe SFTEnv ->
  Maybe (Range 1 10 Int) ->
  ListAllSFTServers ->
  CallsConfigVersion ->
  Bool ->
  Sem r Public.RTCConfiguration
newConfig uid env discoveredServers sftStaticUrl mSftEnv limit listAllServers version shared = do
  finalUris <- selectTurnURIs discoveredServers limit
  srvs <- for finalUris $ \uri -> do
    u <- liftIO $ Public.turnUsername <$> turnExpiry (env ^. turnTokenTTL) <*> pure (genTurnUid uid)
    pure . Public.rtcIceServer (pure uri) u $ computeCred (env ^. turnSHA512) (env ^. turnSecret) u

  let staticSft = pure . Public.sftServer <$> sftStaticUrl
  allSrvEntries <- discoverSFTServers mSftEnv
  mSftServers' <- selectSFTServers allSrvEntries mSftEnv

  let sftFederation' = case version of
        CallsConfigDeprecated -> Nothing
        CallsConfigV2 fed -> fed

  mSftServersAll <-
    case version of
      CallsConfigDeprecated -> pure Nothing
      CallsConfigV2 _ -> sftServersAllFor uid shared listAllServers sftStaticUrl mSftEnv allSrvEntries

  pure $ Public.rtcConfiguration srvs (staticSft <|> mSftServers') (env ^. turnConfigTTL) mSftServersAll sftFederation'

-- | Assemble a v3 call config with coturn native long-term TURN credentials.
-- The SFT part of the response is identical to v2 (SFT keeps zauth credentials).
newConfigV3 ::
  ( Member (Embed IO) r,
    Member SFT r,
    Member (Polysemy.Error NoTurnServers) r
  ) =>
  UserId ->
  Calling.TurnEnv ->
  -- | coturn static-auth-secret
  ByteString ->
  Discovery (NonEmpty Public.TurnURI) ->
  Maybe HttpsUrl ->
  Maybe SFTEnv ->
  Maybe (Range 1 10 Int) ->
  ListAllSFTServers ->
  -- | sft federation (is_federating)
  Maybe Bool ->
  -- | conference calling feature enabled
  Bool ->
  Sem r Public.RTCConfigurationV3
newConfigV3 uid env coturnSecret discoveredServers sftStaticUrl mSftEnv limit listAllServers sftFederation shared = do
  finalUris <- selectTurnURIs discoveredServers limit
  srvs <- for finalUris $ \uri -> do
    u <- liftIO $ Public.coturnUsername <$> turnExpiry (env ^. turnTokenTTL) <*> pure (genTurnUid uid)
    pure . Public.rtcIceServerV3 (pure uri) u $ computeCred (env ^. turnSHA1) coturnSecret u
  let staticSft = pure . Public.sftServer <$> sftStaticUrl
  allSrvEntries <- discoverSFTServers mSftEnv
  mSftServers' <- selectSFTServers allSrvEntries mSftEnv
  mSftServersAll <- sftServersAllFor uid shared listAllServers sftStaticUrl mSftEnv allSrvEntries
  pure $ Public.rtcConfigurationV3 srvs (staticSft <|> mSftServers') (env ^. turnConfigTTL) mSftServersAll sftFederation

-- | ('UserId', 'ConnId' are required as args here to make sure this is an authenticated end-point.)
getCallsConfigV3 ::
  ( Member (Embed IO) r,
    Member SFT r,
    Member GalleyAPIAccess r
  ) =>
  UserId ->
  ConnId ->
  Maybe (Range 1 10 Int) ->
  (Handler r) Public.RTCConfigurationV3
getCallsConfigV3 uid _ limit = do
  env <- asks (.turnEnv)
  case env ^. turnV3Secret of
    Nothing -> do
      Log.err $ Log.msg (Log.val "Call config v3 requested but no coturn secret is configured (turn.coturnSecret).")
      throwE $ StdError internalServerError
    Just coturnSecret -> do
      staticUrl <- asks (.settings.sftStaticUrl)
      sftListAllServers <- fromMaybe Opt.HideAllSFTServers <$> asks (.settings.sftListAllServers)
      sftEnv' <- asks (.sftEnv)
      sftFederation <- asks (.enableSFTFederation)
      discoveredServers <- turnServersV2 (env ^. turnServers)
      shared <- conferenceCallingEnabled uid
      eitherConfig <-
        lift
          . liftSem
          . Polysemy.runError
          $ newConfigV3 uid env coturnSecret discoveredServers staticUrl sftEnv' limit sftListAllServers sftFederation shared
      handleNoTurnServers eitherConfig

-- | Select and randomize the TURN URIs to advertise, honoring the optional
-- limit. Throws 'NoTurnServers' if no servers have been discovered yet or if
-- the limit leaves the list empty.
selectTurnURIs ::
  ( Member (Embed IO) r,
    Member (Polysemy.Error NoTurnServers) r
  ) =>
  Discovery (NonEmpty Public.TurnURI) ->
  Maybe (Range 1 10 Int) ->
  Sem r (NonEmpty Public.TurnURI)
selectTurnURIs discoveredServers limit = do
  -- randomize list of servers (before limiting the list, to ensure not always the same servers are chosen if limit is set)
  randomizedUris <-
    liftIO . randomize
      =<< Polysemy.note NoTurnServers (discoveryToMaybe discoveredServers)
  let limitedUris = case limit of
        Nothing -> randomizedUris
        Just lim -> limitedList randomizedUris lim
  -- randomize again (as limitedList partially re-orders uris)
  liftIO $ randomize limitedUris

limitedList :: NonEmpty Public.TurnURI -> Range 1 10 Int -> NonEmpty Public.TurnURI
limitedList uris lim =
  -- assuming limitServers is safe with respect to the length of its return value
  -- since the input is NonEmpty and limit is in Range 1 10
  -- it should also be safe to assume the returning list has length >= 1
  NonEmpty.nonEmpty (Public.limitServers (NonEmpty.toList uris) (fromRange lim))
    & fromMaybe (error "limitedList: empty list of servers")

hashSHA256 :: ByteString -> ByteString
hashSHA256 = convert . Crypto.hash @ByteString @Crypto.SHA256

-- | Stable per-user UID component of TURN usernames (base26 of the first 16
-- bytes of SHA256 of the user UUID). Same value v2 uses; deterministic per user.
genTurnUid :: UserId -> Text
genTurnUid =
  base26
    . foldr (\x r -> fromIntegral x + r * 256) 0
    . take 16
    . B.unpack
    . hashSHA256
    . BL.toStrict
    . UUID.toByteString
    . toUUID

turnExpiry :: Word32 -> IO POSIXTime
turnExpiry ttl = fromIntegral . (+ ttl) . round <$> getPOSIXTime

computeCred :: (ToByteString a) => Digest -> ByteString -> a -> AsciiBase64
computeCred dig secret = encodeBase64 . hmacBS dig secret . toByteString'

discoverSFTServers :: (Member (Embed IO) r) => Maybe SFTEnv -> Sem r (Maybe (NonEmpty SrvEntry))
discoverSFTServers mSftEnv =
  fmap join $
    for mSftEnv $
      (unSFTServers <$$>) . fmap discoveryToMaybe . readIORef . sftServers

selectSFTServers :: (Member (Embed IO) r) => Maybe (NonEmpty SrvEntry) -> Maybe SFTEnv -> Sem r (Maybe (NonEmpty Public.SFTServer))
selectSFTServers allSrvEntries mSftEnv = do
  srvEntries <- fmap join $
    for mSftEnv $ \actualSftEnv -> liftIO $ do
      let subsetLength = Calling.sftListLength actualSftEnv
      mapM (getRandomElements subsetLength) allSrvEntries
  pure $ sftServerFromSrvTarget . srvTarget <$$> srvEntries

sftServersAllFor ::
  ( Member (Embed IO) r,
    Member SFT r
  ) =>
  UserId ->
  Bool ->
  ListAllSFTServers ->
  Maybe HttpsUrl ->
  Maybe SFTEnv ->
  Maybe (NonEmpty SrvEntry) ->
  Sem r (Maybe [Public.AuthSFTServer])
sftServersAllFor uid shared listAllServers sftStaticUrl mSftEnv allSrvEntries =
  case (listAllServers, sftStaticUrl) of
    (HideAllSFTServers, _) -> pure Nothing
    (ListAllSFTServers, Nothing) -> mapM (mapM authenticateSFT) . pure $ sftServerFromSrvTarget . srvTarget <$> maybe [] toList allSrvEntries
    (ListAllSFTServers, Just url) -> mapM (mapM authenticateSFT) . hush . unSFTGetResponse =<< sftGetAllServers url
  where
    authenticateSFT =
      maybe
        (pure . Public.nauthSFTServer)
        ( \SFTTokenEnv {..} sftsvr -> do
            username <- liftIO $ Public.mkSFTUsername shared <$> turnExpiry sftTokenTTL <*> pure (genTurnUid uid)
            let credential = computeCred sftTokenSHA sftTokenSecret username
            pure $ Public.authSFTServer sftsvr username credential
        )
        (sftToken =<< mSftEnv)

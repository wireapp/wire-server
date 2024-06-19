{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

module MlsStats.Run
  ( run,
  )
where

import Amazonka hiding (await)
import Amazonka.S3
import Amazonka.S3.CreateMultipartUpload
import Amazonka.S3.Lens
import Cassandra as C
import Cassandra.Settings as C
import Conduit
import Control.Exception
import Control.Lens ((.~), (?~), (^.))
import Data.Aeson qualified as A
import Data.ByteString.Base64 qualified as BS64
import Data.ByteString.Lazy qualified as LBS
import Data.Conduit.Combinators hiding (foldMap, stderr, stdout)
import Data.Domain
import Data.Id
import Data.List.NonEmpty (nonEmpty)
import Data.Schema
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
import Data.Time.Format.ISO8601
import Imports hiding (concat, filter, print)
import MlsStats.Options
import Network.HTTP.Types
import System.FilePath.Posix
import System.Logger qualified as Log
import Util.Options
import Wire.API.Conversation.Protocol
import Wire.API.MLS.Group

run :: Opts -> IO ()
run o = do
  logger' <- initLogger
  let settings = o.cassandraSettings
  galleyTables <- initCas settings.galleyHost settings.galleyPort settings.galleyKeyspace logger'
  brigTables <- initCas settings.brigHost settings.brigPort settings.brigKeyspace logger'
  runCommand o.s3Settings galleyTables brigTables o.cassandraSettings.pageSize
  where
    initLogger =
      Log.new
        . Log.setOutput Log.StdOut
        . Log.setFormat Nothing
        . Log.setBufSize 0
        $ Log.defSettings
    initCas casHost casPort casKeyspace l =
      C.init
        . C.setLogger (C.mkLogger l)
        . C.setContacts casHost []
        . C.setPortNumber (fromIntegral casPort)
        . C.setProtocolVersion C.V4
        . C.setKeyspace casKeyspace
        $ C.defSettings

runCommand :: S3Settings -> ClientState -> ClientState -> Int32 -> IO ()
runCommand s3 galleyTables brigTables queryPageSize = do
  logger <- newLogger Debug stderr
  let service =
        setEndpoint (s3.endpoint ^. awsSecure) (s3.endpoint ^. awsHost) (s3.endpoint ^. awsPort) defaultService
          & service_s3AddressingStyle .~ s3.addressingStyle
  env <-
    maybe id (\reg e -> (e :: Env) {region = Region' reg}) s3.region
      . (\e -> e {logger = logger})
      . configureService service
      <$> newEnv discover
  now <- formatShowM iso8601Format <$> getCurrentTime
  let upload =
        uploadStream env (BucketName s3.bucketName)
          . ObjectKey
          . T.pack
          . maybe id (</>) s3.bucketDir
          . maybe id (</>) now
  runResourceT $ do
    upload "user-client.csv" (userClient brigTables queryPageSize)
    upload "conv-group-team-protocol.csv" (convGroupTeamProtocol galleyTables queryPageSize)
    upload "domain-user-client-group.csv" (domainUserClientGroup galleyTables queryPageSize)
    upload "user-conv.csv" (userConv galleyTables queryPageSize)

userClient :: (MonadIO m) => ClientState -> Int32 -> ConduitT () ByteString m ()
userClient cassandra queryPageSize = do
  yield "user,client\r\n"
  ( transPipe
      (runClient cassandra)
      (paginateC userClientCql (paramsP LocalQuorum () queryPageSize) x1)
      .| concat
      .| mapC (\(u, c) -> T.encodeUtf8 $ T.pack (show u) <> "," <> clientToText c <> "\r\n")
    )
  where
    userClientCql :: PrepQuery R () (UserId, ClientId)
    userClientCql = "SELECT user, client FROM clients"

convGroupTeamProtocol :: (MonadIO m) => ClientState -> Int32 -> ConduitT () ByteString m ()
convGroupTeamProtocol cassandra queryPageSize = do
  yield "conversation,group,team,protocol\r\n"
  ( transPipe
      (runClient cassandra)
      (paginateC convGroupTeamProtocolCql (paramsP LocalQuorum () queryPageSize) x1)
      .| concat
      .| mapC (\(c, g, mt, p) -> fmap (c,g,,p) mt) -- filter out non-team conversations
      .| concat
      .| mapC
        ( \(c, g, t, p) ->
            T.encodeUtf8 (T.pack (show c))
              <> ","
              <> foldMap (BS64.encode . unGroupId) g
              <> ","
              <> T.encodeUtf8 (T.pack (show t))
              <> ","
              <> T.encodeUtf8 (convertProtocol p)
              <> "\r\n"
        )
    )
  where
    convGroupTeamProtocolCql :: PrepQuery R () (ConvId, Maybe GroupId, Maybe TeamId, Maybe ProtocolTag)
    convGroupTeamProtocolCql = "SELECT conv, group_id, team, protocol FROM conversation"
    convertProtocol :: Maybe ProtocolTag -> Text
    convertProtocol p = case schemaToJSON (fromMaybe ProtocolProteusTag p) of
      A.String s -> s
      _ -> "?"

domainUserClientGroup :: (MonadIO m) => ClientState -> Int32 -> ConduitT () ByteString m ()
domainUserClientGroup cassandra queryPageSize = do
  yield "user_domain,user,client,group\r\n"
  ( transPipe
      (runClient cassandra)
      (paginateC domainUserClientGroupCql (paramsP LocalQuorum () queryPageSize) x1)
      .| concat
      .| mapC
        ( \(d, u, c, g) ->
            (T.encodeUtf8 (domainText d))
              <> ","
              <> T.encodeUtf8 (T.pack (show u))
              <> ","
              <> T.encodeUtf8 (clientToText c)
              <> ","
              <> BS64.encode (unGroupId g)
              <> "\r\n"
        )
    )
  where
    domainUserClientGroupCql :: PrepQuery R () (Domain, UserId, ClientId, GroupId)
    domainUserClientGroupCql = "SELECT user_domain, user, client, group_id FROM mls_group_member_client"

userConv :: (MonadIO m) => ClientState -> Int32 -> ConduitT () ByteString m ()
userConv cassandra queryPageSize = do
  yield "user,conversation\r\n"
  ( transPipe
      (runClient cassandra)
      (paginateC userConvCql (paramsP LocalQuorum () queryPageSize) x1)
      .| concat
      .| mapC (\(u, c) -> T.encodeUtf8 $ T.pack (show u) <> "," <> T.pack (show c) <> "\r\n")
    )
  where
    userConvCql :: PrepQuery R () (UserId, ConvId)
    userConvCql = "SELECT user, conv FROM user"

uploadStream ::
  Env ->
  BucketName ->
  ObjectKey ->
  ConduitT () ByteString (ResourceT IO) () ->
  (ResourceT IO) ()
uploadStream env bucket key stream = do
  createMultipartResp <-
    sendEither env (newCreateMultipartUpload bucket key) >>= \case
      Left (ServiceError e) | e.status.statusCode == 404 && e.code == ErrorCode "NoSuchBucket" -> do
        void $ send env (newCreateBucket bucket)
        send env (newCreateMultipartUpload bucket key)
      Left e -> liftIO $ throwIO e
      Right resp -> pure resp
  let uploadId' = createMultipartResp ^. createMultipartUploadResponse_uploadId
  parts <-
    runConduit $
      stream
        .| chunksOfE chunkSize
        .| uploadParts env bucket key uploadId' 0
        .| mapC (uncurry newCompletedPart)
        .| sinkList
  void $
    send env $
      newCompleteMultipartUpload bucket key uploadId'
        & completeMultipartUpload_multipartUpload
          ?~ ( newCompletedMultipartUpload
                 & completedMultipartUpload_parts .~ nonEmpty parts
             )
  where
    chunkSize = 5 * 1024 * 1024

uploadParts ::
  Env ->
  BucketName ->
  ObjectKey ->
  Text ->
  Int ->
  ConduitT ByteString (Int, ETag) (ResourceT IO) ()
uploadParts env bucket key uploadId partNum = do
  chunkM <- await
  case chunkM of
    Just chunk -> do
      let req = newUploadPart bucket key partNum uploadId $ toBody chunk
      resp <- send env req
      for_ (resp ^. uploadPartResponse_eTag) $ \etag ->
        yield (partNum, etag)
      uploadParts env bucket key uploadId (partNum + 1)
    Nothing -> pure ()

instance Cql ProtocolTag where
  ctype = Tagged IntColumn

  toCql = CqlInt . fromIntegral . fromEnum

  fromCql (CqlInt i) = do
    let i' = fromIntegral i
    if i' < fromEnum @ProtocolTag minBound
      || i' > fromEnum @ProtocolTag maxBound
      then Left $ "unexpected protocol: " ++ show i
      else Right $ toEnum i'
  fromCql _ = Left "protocol: int expected"

instance Cql GroupId where
  ctype = Tagged BlobColumn

  toCql = CqlBlob . LBS.fromStrict . unGroupId

  fromCql (CqlBlob b) = Right . GroupId . LBS.toStrict $ b
  fromCql _ = Left "group_id: blob expected"

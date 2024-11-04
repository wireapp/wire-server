{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

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

module Brig.User.Search.Index
  ( boolQuery,

    -- * Monad
    IndexEnv (..),
    IndexIO,
    runIndexIO,
    MonadIndexIO (..),

    -- * Administrative
    createIndex,
    createIndexIfNotPresent,
    resetIndex,
    refreshIndex,
    updateMapping,

    -- * Re-exports
    ES.IndexSettings (..),
    ES.IndexName,
  )
where

import Bilge.IO (MonadHttp)
import Bilge.IO qualified as RPC
import Brig.Index.Types (CreateIndexSettings (..))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, catch, throwM)
import Control.Monad.Except
import Data.Aeson as Aeson
import Data.Credentials
import Data.Id
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding
import Database.Bloodhound (BHResponse (getResponse))
import Database.Bloodhound qualified as ES
import Database.Bloodhound.Common.Requests qualified as ESR
import Imports hiding (log, searchable)
import Network.HTTP.Client hiding (host, path, port)
import Network.HTTP.Types (statusCode)
import Prometheus (MonadMonitor)
import System.Logger qualified as Log
import System.Logger.Class (Logger, MonadLogger (..), field, info, msg, val, (+++), (~~))
import Util.Options (Endpoint)
import Wire.IndexedUserStore (IndexedUserStoreError (..))
import Wire.UserSearch.Types (searchVisibilityInboundFieldName)

--------------------------------------------------------------------------------
-- IndexIO Monad

data IndexEnv = IndexEnv
  { idxLogger :: Logger,
    idxElastic :: ES.BHEnv,
    idxRequest :: Maybe RequestId,
    idxName :: ES.IndexName,
    idxAdditionalName :: Maybe ES.IndexName,
    idxAdditionalElastic :: Maybe ES.BHEnv,
    idxGalley :: Endpoint,
    -- | Used to make RPC calls to other wire-server services
    idxRpcHttpManager :: Manager,
    -- credentials for reindexing have to be passed via the env because bulk API requests are not supported by bloodhound
    idxCredentials :: Maybe Credentials
  }

newtype IndexIO a = IndexIO (ReaderT IndexEnv IO a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader IndexEnv,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadMonitor
    )

runIndexIO :: (MonadIO m) => IndexEnv -> IndexIO a -> m a
runIndexIO e (IndexIO m) = liftIO $ runReaderT m e

class (MonadIO m) => MonadIndexIO m where
  liftIndexIO :: IndexIO a -> m a

instance MonadIndexIO IndexIO where
  liftIndexIO = id

instance MonadLogger IndexIO where
  log l m = do
    g <- asks idxLogger
    r <- asks idxRequest
    Log.log g l $ maybe id (field "request" . unRequestId) r ~~ m

instance MonadLogger (ExceptT e IndexIO) where
  log l m = lift (log l m)

instance ES.MonadBH IndexIO where
  dispatch req = do
    bhEnv <- asks idxElastic
    either throwM pure =<< ES.runBH bhEnv (ES.dispatch req)
  tryEsError action = (Right <$> action) `catch` \e -> pure (Left e)
  throwEsError = throwM

instance MonadHttp IndexIO where
  handleRequestWithCont req handler = do
    manager <- asks idxRpcHttpManager
    liftIO $ withResponse req manager handler

--------------------------------------------------------------------------------
-- Administrative

refreshIndex :: (MonadIndexIO m) => m ()
refreshIndex = liftIndexIO $ do
  idx <- asks idxName
  void $ ES.refreshIndex idx

createIndexIfNotPresent ::
  (MonadIndexIO m) =>
  CreateIndexSettings ->
  m ()
createIndexIfNotPresent = createIndex' False

createIndex ::
  (MonadIndexIO m) =>
  CreateIndexSettings ->
  m ()
createIndex = createIndex' True

createIndex' ::
  (MonadIndexIO m) =>
  -- | Fail if index alredy exists
  Bool ->
  CreateIndexSettings ->
  m ()
createIndex' failIfExists (CreateIndexSettings settings shardCount mbDeleteTemplate) = liftIndexIO $ do
  idx <- asks idxName
  ex <- ES.indexExists idx
  when (failIfExists && ex) $
    throwM (IndexError "Index already exists.")
  unless ex $ do
    let fullSettings = settings ++ [ES.AnalysisSetting analysisSettings]

    -- A previous release added an ES Index Template that matched all indices
    -- named 'directory*'. This template is deprecated now, but it might still
    -- be present in production instances. If present then it causes the update mapping
    -- step to fail.
    -- FUTUREWORK: remove this block and the --delete-template option,
    -- after this has been released.
    for_ mbDeleteTemplate $ \templateName@(ES.TemplateName tname) -> do
      tExists <- ES.templateExists templateName
      when tExists $ do
        dr <-
          traceES
            ( encodeUtf8
                ("Delete index template " <> "\"" <> tname <> "\"")
            )
            $ fmap fst (ES.performBHRequest $ ES.keepBHResponse $ ESR.deleteTemplate templateName)
        unless (ES.isSuccess dr) $
          throwM (IndexError "Deleting index template failed.")

    cr <- traceES "Create index" $ fmap fst (ES.performBHRequest $ ES.keepBHResponse $ ESR.createIndexWith fullSettings shardCount idx)
    unless (ES.isSuccess cr) $
      throwM (IndexError "Index creation failed.")
    mr <-
      traceES "Put mapping" $
        fmap fst (ES.performBHRequest $ ES.keepBHResponse $ ESR.putMapping @Value idx indexMapping)
    unless (ES.isSuccess mr) $
      throwM (IndexError "Put Mapping failed.")

analysisSettings :: ES.Analysis
analysisSettings =
  let analyzerDef =
        Map.fromList
          [ ("prefix_index", ES.AnalyzerDefinition (Just $ ES.Tokenizer "whitespace") [ES.TokenFilter "edge_ngram_1_30"] []),
            ("prefix_search", ES.AnalyzerDefinition (Just $ ES.Tokenizer "whitespace") [ES.TokenFilter "truncate_30"] [])
          ]
      filterDef =
        Map.fromList
          [ ("edge_ngram_1_30", ES.TokenFilterDefinitionEdgeNgram (ES.NgramFilter 1 30) Nothing),
            ("truncate_30", ES.TokenFilterTruncate 30)
          ]
   in ES.Analysis analyzerDef mempty filterDef mempty

updateMapping :: (MonadIndexIO m) => m ()
updateMapping = liftIndexIO $ do
  idx <- asks idxName
  ex <- ES.indexExists idx
  unless ex $
    throwM (IndexError "Index does not exist.")
  -- FUTUREWORK: check return code (ES.isSuccess) and fail if appropriate.
  -- But to do that we have to consider the consequences of this failing in our helm chart:
  -- https://github.com/wireapp/wire-server-deploy/blob/92311d189818ffc5e26ff589f81b95c95de8722c/charts/elasticsearch-index/templates/create-index.yaml
  void $
    traceES "Put mapping" $
      fmap fst (ES.performBHRequest $ ES.keepBHResponse $ ESR.putMapping @Value idx indexMapping)

resetIndex ::
  (MonadIndexIO m) =>
  CreateIndexSettings ->
  m ()
resetIndex ciSettings = liftIndexIO $ do
  idx <- asks idxName
  ES.indexExists idx >>= \case
    True -> do
      info $ msg ("Delete Index" :: String)
      void $ ES.deleteIndex idx
    False -> pure ()
  createIndex ciSettings

--------------------------------------------------------------------------------
-- Internal

traceES :: (MonadIndexIO m) => ByteString -> IndexIO (ES.BHResponse contextualized body) -> m (ES.BHResponse contextualized body)
traceES descr act = liftIndexIO $ do
  info (msg descr)
  r <- act
  info . msg $ (statusCode . responseStatus $ getResponse r) +++ val " - " +++ responseBody (getResponse r)
  pure r

-- | This mapping defines how elasticsearch will treat each field in a document. Here
-- is how it treats each field:
-- name: Not indexed, as it is only meant to be shown to user, for querying we use
--       normalized
-- team: Used to ensure only teammates can find each other
-- accent_id: Not indexed, we cannot search by this.
-- normalized: This is transliterated version of the name to ASCII Latin characters,
--             this is used for searching by name
-- handle: Used for searching by handle
-- normalized.prefix: Used for searching by name prefix
-- handle.prefix: Used for searching by handle prefix
-- saml_idp: URL of SAML issuer, not indexed, used for sorting
-- managed_by: possible values "scim" or "wire", indexed as keyword
-- created_at: date when "activated" state last chagned in epoch-millis, not indexed, used for sorting
--
-- The prefix fields use "prefix_index" analyzer for indexing and "prefix_search"
-- analyzer for searching. The "prefix_search" analyzer uses "edge_ngram" filter, this
-- indexes the handles and normalized names by prefixes. For example: "alice" will be
-- indexed as "a", "al", "ali", "alic" and "alice". While searching for say "ali", we
-- do not want to again use the "prefix" analyzer, otherwise we would get a match for
-- "a", "al" and "ali" each, this skews the scoring in elasticsearch a lot and exact
-- matches get pushed behind prefix matches.
--
-- The "prefix_index" analyzer is defined as a combination of the "whitespace"
-- tokenizer and "edge_ngram_1_30" filter. The edge_ngram_1_30 filter generates tokens
-- of from length 1 to 30 and the whitespace tokenizer ensures words separated by
-- whitespaces are tokenized separately. So, tokens for "Alice Charlie" would be:
-- ["a", "al", "ali", "alic", "alice", "c", "ch", "cha", "char", "charl", "charlie"]
-- This makes searching for somebody by just their last or middle name possible.
-- Additionally one could look for "ali char" and still expect to find "Alice Charlie"
--
-- The "prefix_search" analyzer is defined as a combination of the "whitespace"
-- tokenizer and "truncate_30" filter. The truncate_30 filter ensures that the
-- searched tokens are not bigger than 30 characters by truncating them, this is
-- necessary as our "prefix_index" analyzer only creates edge_ngrams until 30
-- characters.
--
-- About the dynamic field: When this is not set and we add another field to our
-- user document, elasticsearch will try to guess how it is supposed to be indexed.
-- Changes to this require creating a new index and a cumbersome migration. So it is
-- important that we set this field to `false`. This will make new fields will just
-- not be indexed. After we decide what they should look like, we can just run a
-- reindex to make them usable. More info:
-- https://www.elastic.co/guide/en/elasticsearch/reference/7.7/dynamic.html
indexMapping :: Value
indexMapping =
  object
    [ "dynamic" .= False,
      "properties"
        .= object
          [ "normalized" -- normalized user name
              .= MappingProperty
                { mpType = MPText,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields =
                    Map.fromList [("prefix", MappingField MPText (Just "prefix_index") (Just "prefix_search"))]
                },
            "name"
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = False,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "handle"
              .= MappingProperty
                { mpType = MPText,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields =
                    Map.fromList
                      [ ("prefix", MappingField MPText (Just "prefix_index") (Just "prefix_search")),
                        ("keyword", MappingField MPKeyword Nothing Nothing)
                      ]
                },
            "email"
              .= MappingProperty
                { mpType = MPText,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields =
                    Map.fromList
                      [ ("prefix", MappingField MPText (Just "prefix_index") (Just "prefix_search")),
                        ("keyword", MappingField MPKeyword Nothing Nothing)
                      ]
                },
            "team"
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "accent_id"
              .= MappingProperty
                { mpType = MPByte,
                  mpStore = False,
                  mpIndex = False,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "account_status"
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "saml_idp"
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = False,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "managed_by"
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "created_at"
              .= MappingProperty
                { mpType = MPDate,
                  mpStore = False,
                  mpIndex = False,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "role"
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            searchVisibilityInboundFieldName
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "scim_external_id"
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = False,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "sso"
              .= object
                [ "type" .= Aeson.String "nested",
                  "properties"
                    .= object
                      [ "issuer"
                          .= MappingProperty
                            { mpType = MPKeyword,
                              mpStore = False,
                              mpIndex = False,
                              mpAnalyzer = Nothing,
                              mpFields = mempty
                            },
                        "nameid"
                          .= MappingProperty
                            { mpType = MPKeyword,
                              mpStore = False,
                              mpIndex = False,
                              mpAnalyzer = Nothing,
                              mpFields = mempty
                            }
                      ]
                ],
            "email_unvalidated"
              .= MappingProperty
                { mpType = MPText,
                  mpStore = False,
                  mpIndex = False,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                }
          ]
    ]

data MappingProperty = MappingProperty
  { mpType :: MappingPropertyType,
    mpStore :: Bool,
    mpIndex :: Bool,
    mpAnalyzer :: Maybe Text,
    mpFields :: Map Text MappingField
  }

data MappingField = MappingField
  { mfType :: MappingPropertyType,
    mfAnalyzer :: Maybe Text,
    mfSearchAnalyzer :: Maybe Text
  }

data MappingPropertyType = MPText | MPKeyword | MPByte | MPDate
  deriving (Eq)

instance ToJSON MappingProperty where
  toJSON mp =
    object
      ( [ "type" .= mpType mp,
          "store" .= mpStore mp,
          "index" .= mpIndex mp
        ]
          <> ["analyzer" .= mpAnalyzer mp | isJust $ mpAnalyzer mp]
          <> ["fields" .= mpFields mp | not . Map.null $ mpFields mp]
      )

instance ToJSON MappingPropertyType where
  toJSON MPText = Aeson.String "text"
  toJSON MPKeyword = Aeson.String "keyword"
  toJSON MPByte = Aeson.String "byte"
  toJSON MPDate = Aeson.String "date"

instance ToJSON MappingField where
  toJSON mf =
    object $
      ["type" .= mfType mf]
        <> ["analyzer" .= mfAnalyzer mf | isJust (mfAnalyzer mf)]
        <> ["search_analyzer" .= mfSearchAnalyzer mf | isJust (mfSearchAnalyzer mf)]

boolQuery :: ES.BoolQuery
boolQuery = ES.mkBoolQuery [] [] [] []

data ParseException = ParseException
  { _parseExceptionRemote :: !Text,
    _parseExceptionMsg :: String
  }

instance Show ParseException where
  show (ParseException r m) =
    "Failed to parse response from remote "
      ++ Text.unpack r
      ++ " with message: "
      ++ m

instance Exception ParseException

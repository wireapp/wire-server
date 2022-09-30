{-# LANGUAGE TemplateHaskell #-}

module Brig.API.Public.Swagger
  ( SwaggerDocsAPI,
    pregenSwagger,
    swaggerPregenUIServer,
  )
where

import qualified Data.Aeson as Aeson
import Data.FileEmbed
import qualified Data.Text as T
import FileEmbedLzma
import Imports hiding (head)
import Language.Haskell.TH
import Servant
import Servant.Swagger.Internal.Orphans ()
import Servant.Swagger.UI
import Wire.API.Routes.Version

type SwaggerDocsAPIBase = SwaggerSchemaUI "swagger-ui" "swagger.json"

type SwaggerDocsAPI = "api" :> Header VersionHeader Version :> SwaggerDocsAPIBase

pregenSwagger :: Version -> Q Exp
pregenSwagger v =
  embedLazyByteString
    =<< makeRelativeToProject
      ("docs/swagger-v" <> T.unpack (toUrlPiece v) <> ".json")

swaggerPregenUIServer :: LByteString -> Server SwaggerDocsAPIBase
swaggerPregenUIServer =
  swaggerSchemaUIServer
    . fromMaybe Aeson.Null
    . Aeson.decode

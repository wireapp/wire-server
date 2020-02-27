{-# LANGUAGE OverloadedStrings #-}

module Wire.Swagger where

import Data.Swagger.Build.Api
import Imports hiding (max, min)

errorModel :: Model
errorModel = defineModel "Error" $ do
  description "Basic error information"
  errorProperties

errorProperties :: ModelBuilder
errorProperties = do
  property "code" int32' $
    description "HTTP status code"
  property "label" string' $
    description "Textual classifier for programmatic consumption."
  property "message" string' $
    description "More detailed error description."

int32Between :: Int32 -> Int32 -> DataType
int32Between m n = int32 (min m . max n)

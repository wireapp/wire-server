{-# LANGUAGE OverloadedStrings #-}

module Data.Swagger where

import Imports
import Data.Swagger.Build.Api

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

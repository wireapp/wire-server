{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE LambdaCase        #-}

{-# OPTIONS_GHC -Wno-unused-binds #-}

module Stern.Servant.Types where

import Imports

import Data.Aeson
import Data.Id
import Servant.API
import Servant.API.Generic
import Servant.Swagger.UI


data API route = API
  { _apiSwaggerDoc
    :: route :- RootPrefix :> NoSwagger :>
       SwaggerSchemaUI "api-docs" "swagger.json"


  , _apiInternalGetStatus
    :: route :- RootPrefix :> NoSwagger :>
       "i" :> "status" :> Verb 'GET 200 '[JSON] NoContent
    -- FUTUREWORK: status204 would be more correct

  , _apiInternalHeadStatus
    :: route :- RootPrefix :> NoSwagger :>
       "i" :> "status" :> Verb 'HEAD 200 '[JSON] NoContent
    -- FUTUREWORK: would status204 be more correct here, too?  not sure how 'HEAD works...

  , _apiInternalMonitoring
    :: route :- RootPrefix :> NoSwagger :>
       "i" :> "monitoring" :> Get '[JSON] Value
    -- This is deprecated in favour of /i/metrics via prometheus middleware.


  , _apiSuspendUser
    :: route :- RootPrefix :>
       "users" :> Capture "uid" UserId :> "suspend" :> Post '[JSON] NoContent

  , _apiUnsuspendUser
    :: route :- RootPrefix :>
       "users" :> Capture "uid" UserId :> "unsuspend" :> Post '[JSON] NoContent

  }
  deriving (Generic)


type RootPrefix = "servant"

data NoSwagger

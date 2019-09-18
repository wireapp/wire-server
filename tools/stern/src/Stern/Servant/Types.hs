{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE LambdaCase        #-}

{-# OPTIONS_GHC -Wno-unused-binds #-}

module Stern.Servant.Types where

import Imports

import Data.Id
import Servant.API
import Servant.API.Generic
import Servant.Swagger.UI


data API route = API
  { _apiSwaggerDoc
    :: route :- SwaggerSchemaUI "api-docs" "swagger.json"

  , _apiSuspendUser
    :: route :- "users" :> Capture "uid" UserId :> "suspend" :> Post '[JSON] NoContent

  , _apiUnsuspendUser
    :: route :- "users" :> Capture "uid" UserId :> "unsuspend" :> Post '[JSON] NoContent

  }
  deriving (Generic)

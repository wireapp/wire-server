{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Scimmy.Experiment where

import Brig.Types.User
import Data.Text (Text)
import Servant
import Servant.Client
import Network.URI()

-- TODO: once servant-derived clients can be combined with some helper concepts (e.g. RequestId, retry logic, etc - see Bilge), the actual types should be moved to brig / brig-types

type GetUser = "self" :> Header "Z-User" Text :> Get '[JSON] SelfProfile

getUser :: Maybe Text -> ClientM SelfProfile
getUser = client (Proxy :: Proxy GetUser)

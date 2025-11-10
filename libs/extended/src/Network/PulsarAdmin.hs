module Network.PulsarAdmin where

import Imports
import Servant

type Tenant = Text

type Namespace = Text

type Topic = Text

data AdminAPI route = AdminAPI
  { -- | https://pulsar.apache.org/docs/4.1.x/admin-api-topics/#delete-topic
    deleteTopic ::
      route
        :- Capture "tenant" Tenant
          :> Capture "namespace" Namespace
          :> Capture "topic" Topic
          :> DeleteNoContent
  }

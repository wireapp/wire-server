module Data.EmailVisibility where

import Data.Aeson
import Data.Schema
import qualified Data.Schema as Schema
import Data.Text
import GHC.Generics
import Wire.Arbitrary
import Prelude

-- | Configurations for whether to show a user's email to others.
data EmailVisibility
  = -- | Anyone can see the email of someone who is on ANY team.
    --         This may sound strange; but certain on-premise hosters have many different teams
    --         and still want them to see each-other's emails.
    EmailVisibleIfOnTeam
  | -- | Anyone on your team with at least 'Member' privileges can see your email address.
    EmailVisibleIfOnSameTeam
  | -- | Show your email only to yourself
    EmailVisibleToSelf
  deriving (Eq, Show, Bounded, Enum, Generic)
  deriving (FromJSON, ToJSON) via (Schema.Schema EmailVisibility)
  deriving (Arbitrary) via (GenericUniform EmailVisibility)

instance Schema.ToSchema EmailVisibility where
  schema =
    enum @Text "EmailVisibility" $
      mconcat
        [ element "visible_if_on_team" EmailVisibleIfOnTeam,
          element "visible_if_on_same_team" EmailVisibleIfOnSameTeam,
          element "visible_to_self" EmailVisibleToSelf
        ]

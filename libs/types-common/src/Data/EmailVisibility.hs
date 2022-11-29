module Data.EmailVisibility where

import Prelude
import qualified Data.Schema as Schema
import Data.Schema
import Data.Text
import Data.Aeson

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
  deriving (Eq, Show, Bounded, Enum)
  deriving (FromJSON, ToJSON) via (Schema.Schema EmailVisibility)

instance Schema.ToSchema EmailVisibility where
   schema = enum @Text "EmailVisibility" $
      mconcat
        [ element "visible_if_on_team" EmailVisibleIfOnTeam,
          element "visible_if_on_same_team" EmailVisibleIfOnSameTeam,
          element "visible_to_self" EmailVisibleToSelf
        ]

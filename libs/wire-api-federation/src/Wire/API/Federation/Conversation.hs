{-# LANGUAGE DerivingVia #-}

module Wire.API.Federation.Conversation where

import Data.Aeson (FromJSON, ToJSON)
import Data.Id (ConvId, UserId)
import Data.Qualified (Qualified)
import Imports
import Servant.API ((:>), Capture, JSON, Post, ReqBody)
import Servant.API.Generic ((:-))
import Test.QuickCheck (Arbitrary (arbitrary))
import qualified Test.QuickCheck as QC
import Wire.API.Federation.Types.Event (Event, MemberJoin)
import Wire.API.Federation.Util.Aeson (CustomEncoded (CustomEncoded))

data Api routes
  = Api
      { joinConversationById ::
          routes
            :- "f"
            :> "conversation"
            :> Capture "cnv" (Qualified ConvId)
            :> "join"
            :> ReqBody '[JSON] JoinConversationByIdRequest
            :> Post '[JSON] (ConversationUpdateResult MemberJoin)
      }
  deriving (Generic)

data JoinConversationByIdRequest
  = JoinConversationByIdRequest
      { joinUserId :: Qualified UserId
      }
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded JoinConversationByIdRequest)

data ConversationUpdateResult a
  = ConversationUpdated (Event a)
  | ConversationUnchanged
  deriving stock (Show, Generic, Functor)
  deriving (ToJSON, FromJSON) via (CustomEncoded (ConversationUpdateResult a))

instance Arbitrary a => Arbitrary (ConversationUpdateResult a) where
  arbitrary = QC.oneof [pure ConversationUnchanged, ConversationUpdated <$> arbitrary]

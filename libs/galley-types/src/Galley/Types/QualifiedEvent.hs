{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Galley.Types.QualifiedEvent
  ( AnyQualifiedEvent,
    QualifiedEvent (..),
    AnyQualifiedEventData (..),
    toEventType,
    MemberJoin (..),
    QualifiedSimpleMember (..),
    QualifiedSimpleMembers (..),
  )
where

import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import Data.Id
import Data.Json.Util
import Data.Qualified (Qualified)
import Data.Time
import Galley.Types
import Galley.Types.Conversations.Roles
import Imports

-- | Similar to 'Galley.Types.Event', but all IDs are qualified, to allow this
-- representation to be sent across backends.
type AnyQualifiedEvent = QualifiedEvent AnyQualifiedEventData

-- | Instead of just being a generic event, it also allows to specify which type
-- of event it is, e.g. @QualifiedEvent MemberJoin@.
data QualifiedEvent a
  = QualifiedEvent
      { qevtConv :: Qualified ConvId,
        qevtFrom :: Qualified UserId,
        qevtTime :: UTCTime,
        qevtData :: a
      }
  deriving stock (Foldable, Functor, Show)

-- FUTUREWORK(federation): Extend with the other Event types that need to be
-- sent across backends.
data AnyQualifiedEventData
  = DataMemberJoin MemberJoin
  deriving stock (Show)

toEventType :: AnyQualifiedEventData -> EventType
toEventType = \case
  DataMemberJoin _ -> MemberJoin

newtype MemberJoin
  = QuMemberJoin QualifiedSimpleMembers
  deriving stock (Show)
  deriving newtype (ToJSON, FromJSON)

newtype QualifiedSimpleMembers
  = QualifiedSimpleMembers
      { qsmMembers :: [QualifiedSimpleMember]
      }
  deriving stock (Eq, Show)

data QualifiedSimpleMember
  = QualifiedSimpleMember
      { qsmId :: !UserId,
        qsmConvRoleName :: !RoleName
      }
  deriving stock (Eq, Show)

-- Instances ----------------------------------------------------------------

-- JSON

instance ToJSON (QualifiedEvent AnyQualifiedEventData) where
  toJSON e =
    Aeson.object
      [ "type" .= toEventType (qevtData e),
        "conversation" .= qevtConv e,
        "from" .= qevtFrom e,
        "time" .= toUTCTimeMillis (qevtTime e),
        "data" .= case qevtData e of
          DataMemberJoin x -> toJSON x
      ]

instance ToJSON (QualifiedEvent MemberJoin) where
  toJSON = toJSON . fmap DataMemberJoin

instance FromJSON (QualifiedEvent AnyQualifiedEventData) where
  parseJSON =
    withObject "qualified event" $ do
      withType $ \case
        MemberJoin -> fmap (fmap DataMemberJoin) . parseQualifiedEvent
        other -> fail $ "parser for type " <> show (Aeson.encode other) <> "not implemented"

instance FromJSON (QualifiedEvent MemberJoin) where
  parseJSON =
    withObject "qualified event" $ \e -> do
      assertType MemberJoin e
      parseQualifiedEvent e

withType :: (EventType -> Aeson.Object -> Parser a) -> Aeson.Object -> Parser a
withType parse o = do
  typ <- o .: "type"
  parse typ o

assertType :: EventType -> Aeson.Object -> Parser ()
assertType expectedType = withType $ \actualType _ ->
  if actualType == expectedType
    then pure ()
    else fail $ "expected event type " <> showJSON expectedType <> ", got " <> showJSON actualType
  where
    showJSON = show . Aeson.encode

-- only safe to be used after already checking the event type
parseQualifiedEvent :: FromJSON a => Aeson.Object -> Parser (QualifiedEvent a)
parseQualifiedEvent o =
  QualifiedEvent
    <$> o .: "conversation"
    <*> o .: "from"
    <*> o .: "time"
    <*> o .: "data"

instance ToJSON QualifiedSimpleMembers where
  toJSON e =
    object
      [ "users" .= qsmMembers e
      ]

instance FromJSON QualifiedSimpleMembers where
  parseJSON = withObject "simple-members-payload" $ \o -> do
    QualifiedSimpleMembers
      <$> o .: "users"

instance ToJSON QualifiedSimpleMember where
  toJSON m =
    object
      [ "id" .= qsmId m,
        "conversation_role" .= qsmConvRoleName m
      ]

instance FromJSON QualifiedSimpleMember where
  parseJSON = withObject "simple member object" $ \o ->
    QualifiedSimpleMember
      <$> o .: "id"
      <*> o .:? "conversation_role" .!= roleNameWireAdmin

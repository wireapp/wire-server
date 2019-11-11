{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Galley.Types.Conversations.Roles where

import Imports
import Control.Applicative (optional)
import Data.Aeson
import Data.Attoparsec.Text
import Data.ByteString.Conversion
import Data.Bits (testBit, (.|.))
import Data.Hashable
import qualified Data.Set as Set

data ConvRoleType = WireConvRoleAdmin
                  | WireConvRoleMember
                  | ConvRoleCustom Label Actions
                  deriving (Eq, Show)

-- TODO: This needs to be modeled in a better way
data ConversationRole = ConversationRole
    { crRoleName    :: !ConvRoleType
    , crRoleActions :: !Actions
    } deriving (Eq, Show)

-- Labels with `wire_` prefix are reserved
-- and cannot be created by externals

newtype Label = Label { fromLabel :: Text }
    deriving (Eq, Show, ToJSON, ToByteString, Hashable, Generic)

instance FromByteString Label where
    parser = parser >>= maybe (fail "Invalid label") return . parseLabel

instance FromJSON Label where
    parseJSON = withText "Label" $
        maybe (fail "Invalid label") pure . parseLabel

parseLabel :: Text -> Maybe Label
parseLabel t
    | isValidLabel t = Just (Label t)
    | otherwise            = Nothing

-- All labels should have 2-128 chars
--  * Wire labels _must_ start with `wire_`
--  * Custom labels _must not_ start with `wire_`
-- TODO: Ensure that all Wire defined labels start with `wire_`
-- TODO: Should we accept non ascii chars as labels?
--       this will be used for search and thus be awkward...
isValidLabel :: Text -> Bool
isValidLabel = either (const False) (const True)
             . parseOnly customLabel
  where
    customLabel = count 2 (satisfy chars)
                *> count 126 (optional (satisfy chars))
                *> endOfInput
    chars = inClass "a-zA-Z0-9_-"

data Actions = Actions
    { allowedActions :: Set Action
    } deriving (Eq, Ord, Show, Generic)

data Action =
      AddConvMember
    | RemoveConvMember
    | ModifyConvMetadata
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

intToAction :: Word64 -> Maybe Action
intToAction 0x0001 = Just AddConvMember
intToAction 0x0002 = Just RemoveConvMember
intToAction 0x0004 = Just ModifyConvMetadata
intToAction _      = Nothing

intToActions :: Word64 -> Actions
intToActions n =
    let actions = [ 2^i | i <- [0 .. 62], n `testBit` i ] in
    Actions $ Set.fromList (mapMaybe intToAction actions)

allActions :: Actions
allActions = intToActions maxBound

noActions :: Actions
noActions = Actions mempty

convRoleWireAdmin :: ConversationRole
convRoleWireAdmin = ConversationRole WireConvRoleAdmin $ Actions (roleActions WireConvRoleAdmin)

convRoleWireMember :: ConversationRole
convRoleWireMember = ConversationRole WireConvRoleMember $ Actions (roleActions WireConvRoleMember)

convRoleCustom :: Text -> Actions -> Maybe ConversationRole
convRoleCustom name actions = case parseLabel name of
   Just label -> Just $ ConversationRole (ConvRoleCustom label actions) actions
   Nothing    -> Nothing

roleActions :: ConvRoleType -> Set Action
roleActions WireConvRoleAdmin = roleActions WireConvRoleMember <> Set.fromList
    [ ModifyConvMetadata
    ]
roleActions WireConvRoleMember = Set.fromList
    [ AddConvMember
    , RemoveConvMember
    ]
roleActions (ConvRoleCustom _ (Actions actions)) = actions

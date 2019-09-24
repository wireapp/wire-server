{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | This is where currently all the json roundtrip tests happen for brig-types and
-- galley-types.
module Test.Brig.Types.Common where

import Imports

import "swagger2" Data.Swagger hiding (Header(..))
  -- NB: this package depends on both types-common, swagger2, so there is no away around this name
  -- clash other than -XPackageImports.

import Brig.Types.Activation
import Brig.Types.Client
import Brig.Types.Connection
import Brig.Types.Intra
import Brig.Types.Properties
import Brig.Types.Search as Search
import Brig.Types.Servant
import Brig.Types.Team.LegalHold
import Brig.Types.Test.Arbitrary ()
import Brig.Types.User
import Brig.Types.User.Auth
import Brig.Types.User.Auth (CookieLabel)
import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson                 (Value)
import Data.Currency (Alpha)
import Data.Id
import Data.LegalHold
import Data.Misc
import Data.Range
import Data.Text.Ascii
import Data.Typeable (typeOf)
import Galley.Types
import Galley.Types.Bot.Service
import Galley.Types.Teams
import Galley.Types.Teams.Intra
import Galley.Types.Teams.SSO
import Gundeck.Types.Notification
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Galley.Types.Teams.Internal ()

import qualified Data.Json.Util
import qualified URI.ByteString


-- FUTUREWORK: validateEveryToJSON from servant-swagger doesn't render these tests
-- unnecessary, as it only tests ToJSON against ToSchema, and only of the types that the
-- server side requires a ToJSON for.  it's very feasible to extend this, though.

tests :: TestTree
tests = testGroup "Common (types vs. aeson)"
    [ run @AccountStatus
    , run @AccountStatusObject
    , run @AccountStatusUpdate
    , run @ActivationCode
    , run @ActivationCodeObject
    , run @ActivationKey
    , run @Alpha
    , run @ApproveLegalHoldForUserRequest
    , run @(AsciiText Base64Url)
    , run @(AsciiText Printable)
    , run @Asset
    , run @AssetSize
    , run @BindingNewTeam
    , run @BindingNewTeamUser
    , run @Brig.Types.User.EmailUpdate
    , run @Client
    , run @ClientId
    , run @ColourId
    , run @ConnectionsStatusRequest
    , run @ConnectionStatus
    , run @Conversation
    , run @CookieLabel
    , run @CookieList
    , run @Data.Json.Util.UTCTimeMillis
    , run @DisableLegalHoldForUserRequest
    , run @Email
    , run @ExcludedPrefix
    , run @FeatureFlags
    , run @Handle
    , run @(Id U)
    , run @InvitationCode
    , run @LegalHoldClientRequest
    , run @LegalHoldService
    , run @LegalHoldServiceConfirm
    , run @LegalHoldStatus
    , run @LegalHoldTeamConfig
    , run @Locale
    , run @ManagedBy
    , run @ManagedByUpdate
    , run @Message
    , run @Name
    , run @NewLegalHoldClient
    , run @NewLegalHoldService
    , run @(NewTeam ())
    , run @NewUser
    , run @Phone
    , run @PhonePrefix
    , run @PhoneUpdate
    , run @Pict
    , run @PlainTextPassword
    , run @PropertyKey
    , run @PropertyValue
    , run @QueuedNotification
    , run @(Range 3 10 Int32)
    , run @Relation
    , run @RemoveLegalHoldSettingsRequest
    , run @RequestNewLegalHoldClient
    , run @RichField
    , run @RichInfo
    , run @RichInfoUpdate
    , run @Search.Contact
    , run @(SearchResult Search.Contact)
    , run @SelfProfile
    , run @ServiceRef
    , run @SSOStatus
    , run @SSOTeamConfig
    , run @Swagger
    , run @Team
    , run @TeamBinding
    , run @TeamData
    , run @TeamMember
    , run @TeamMemberDeleteData
    , run @TeamStatus
    , run @URI.ByteString.URI
    , run @User
    , run @UserAccount
    , run @UserClients
    , run @UserConnection
    , run @UserIdentity
    , run @UserIds
    , run @UserLegalHoldStatus
    , run @UserLegalHoldStatusResponse
    , run @UserSet
    , run @UserSSOId
    , run @Value
    , run @ViewLegalHoldService

    -- FUTUREWORK: collect all types with something like this:
    -- find ~/src/wire-server/libs/ -type f -name "*.hs" | sort -u | xargs perl -ne "/^(data|newtype|type)\s+(\S.*)=/ && print \"\$2\n\""

    , testCase "{} is a valid TeamMemberDeleteData" $ do
        assertEqual "{}" (Right $ newTeamMemberDeleteData Nothing) (eitherDecode "{}")
    ]
  where
    run :: forall a. (Arbitrary a, Typeable a, ToJSON a, FromJSON a, Eq a, Show a)
         => TestTree
    run = testProperty msg trip
      where
        msg = show $ typeOf (undefined :: a)
        trip (v :: a) = counterexample (show $ toJSON v)
                      $ Right v === (parseEither parseJSON . toJSON) v


-- TODO: move all these instances to /libs/brig-types/src/Brig/Types/Test/Arbitrary.hs

instance Arbitrary TeamMemberDeleteData where
  arbitrary = newTeamMemberDeleteData <$> arbitrary

instance Eq TeamMemberDeleteData where
  a == b = a ^. tmdAuthPassword == b ^. tmdAuthPassword

instance Show TeamMemberDeleteData where
  show a = "(TeamMemberDeleteData " <> show (a ^. tmdAuthPassword) <> ")"

instance Arbitrary SSOStatus where
  arbitrary = Test.Tasty.QuickCheck.elements [minBound..]

instance Arbitrary SSOTeamConfig where
  arbitrary = SSOTeamConfig <$> arbitrary

instance Arbitrary FeatureFlags where
  arbitrary = FeatureFlags
      <$> Test.Tasty.QuickCheck.elements [minBound..]
      <*> Test.Tasty.QuickCheck.elements [minBound..]

instance Arbitrary AccountStatus where
  arbitrary = undefined

instance Arbitrary AccountStatusObject where
  arbitrary = undefined

instance Arbitrary AccountStatusUpdate where
  arbitrary = undefined

instance Arbitrary ActivationCodeObject where
  arbitrary = undefined

instance Arbitrary ActivationKey where
  arbitrary = undefined

instance Arbitrary (AsciiText Printable) where
  arbitrary = undefined

instance Arbitrary BindingNewTeam where
  arbitrary = undefined

instance Arbitrary Client where
  arbitrary = undefined

instance Arbitrary ConnectionsStatusRequest where
  arbitrary = undefined

instance Arbitrary ConnectionStatus where
  arbitrary = undefined

instance Arbitrary Conversation where
  arbitrary = undefined

instance Arbitrary Message where
  arbitrary = undefined

instance Arbitrary PropertyKey where
  arbitrary = undefined

instance Arbitrary PropertyValue where
  arbitrary = undefined

instance Arbitrary QueuedNotification where
  arbitrary = undefined

instance Arbitrary (Range 3 10 Int32) where
  arbitrary = undefined

instance Arbitrary Relation where
  arbitrary = undefined

instance Arbitrary Swagger where
  arbitrary = undefined

instance Arbitrary Team where
  arbitrary = undefined

instance Arbitrary TeamBinding where
  arbitrary = undefined

instance Arbitrary TeamData where
  arbitrary = undefined

instance Arbitrary TeamStatus where
  arbitrary = undefined

instance Arbitrary UserAccount where
  arbitrary = undefined

instance Arbitrary UserClients where
  arbitrary = undefined

instance Arbitrary UserConnection where
  arbitrary = undefined

instance Arbitrary UserIds where
  arbitrary = undefined

instance Arbitrary UserSet where
  arbitrary = undefined

instance Arbitrary Value where
  arbitrary = undefined

instance Arbitrary CookieList where
  arbitrary = undefined

instance Arbitrary Search.Contact where
  arbitrary = undefined

instance Arbitrary (SearchResult Search.Contact) where
  arbitrary = undefined

instance Arbitrary URI.ByteString.URI where
  arbitrary = undefined

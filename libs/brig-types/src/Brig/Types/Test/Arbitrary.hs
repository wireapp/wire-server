{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Brig.Types.Test.Arbitrary where

#ifdef WITH_ARBITRARY

import Imports

import Brig.Types.Activation
import Brig.Types.Client
import Brig.Types.Code
import Brig.Types.Connection
import Brig.Types.Intra
import Brig.Types.Properties
import Brig.Types.Provider (UpdateServiceWhitelist(..), ServiceKeyType(..),ServiceKey(..), ServiceKeyPEM(..))
import Brig.Types.Search as Search
import Brig.Types.Servant
import Brig.Types.Team.Invitation
import Brig.Types.Team.LegalHold
import Brig.Types.TURN
import Brig.Types.TURN.Internal
import Brig.Types.User
import Brig.Types.User.Auth
import Brig.Types.User.Auth (CookieLabel)
import Control.Lens hiding (elements)
import Data.Currency (Alpha)
import Data.IP
import Data.Json.Util (UTCTimeMillis (..), toUTCTimeMillis)
import Data.LanguageCodes
import Data.List.Extra (nubOn)
import Data.Misc
import Data.PEM (pemParseBS)
import Data.Proxy
import Data.Range
import Data.String.Conversions (cs)
import Data.Text.Ascii
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (nil)
import Galley.Types
import Galley.Types.Bot.Service.Internal
import Galley.Types.Teams
import Galley.Types.Teams.Internal
import Galley.Types.Teams.Intra
import Galley.Types.Teams.SSO
import GHC.TypeLits
import Gundeck.Types.Notification
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Text.Hostname
import URI.ByteString.QQ (uri)

import qualified Data.List1 as List1
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set
import qualified Data.Text as ST
import qualified Data.Text            as Text
import qualified GHC.Unicode as Unicode
import qualified System.Random
import qualified Test.Tasty.QuickCheck
import qualified URI.ByteString


newtype Octet = Octet { octet :: Word16 }
    deriving (Eq, Show)

instance Arbitrary Octet where
    arbitrary = Octet <$> arbitrary `suchThat` (<256)

instance Arbitrary Scheme where
    arbitrary = genEnumBounded

-- TODO: Add an arbitrary instance for IPv6
instance Arbitrary IpAddr where
    arbitrary = ipV4Arbitrary
      where
        ipV4Arbitrary :: Gen IpAddr
        ipV4Arbitrary = do
            a <- ipV4Part
            b <- ipV4Part
            c <- ipV4Part
            d <- ipV4Part
            let adr = show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d
            IpAddr . IPv4 <$> return (read adr)

        ipV4Part = octet <$> arbitrary

instance Arbitrary TurnHost where
    arbitrary = oneof
              [ TurnHostIp   <$> arbitrary
              , TurnHostName <$> arbitrary `suchThat` (validHostname . encodeUtf8)
              ]

instance Arbitrary Port where
    arbitrary = Port <$> arbitrary

instance Arbitrary Transport where
    arbitrary = genEnumBounded

instance Arbitrary TurnURI where
    arbitrary = turnURI <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary


instance Arbitrary Handle where
  arbitrary = Handle . ST.pack <$> do
      let manyC n = replicateM n (elements $ ['a'..'z'] <> ['0'..'9'] <> ['_'])
      ((<>) <$> manyC 2 <*> (manyC =<< choose (0, 19)))

instance Arbitrary Name where
  arbitrary = Name . ST.pack <$>
      ((`replicateM` arbitrary) =<< choose (1, 128))

instance Arbitrary ColourId where
  arbitrary = ColourId <$> arbitrary

instance Arbitrary Email where
  arbitrary = do
      localPart <- ST.filter (/= '@') <$> arbitrary
      domain <- ST.filter (/= '@') <$> arbitrary
      pure $ Email localPart domain

instance Arbitrary Phone where
  arbitrary = Phone . ST.pack <$> do
      let mkdigits n = replicateM n (elements ['0'..'9'])
      mini <- mkdigits 8
      maxi <- mkdigits =<< choose (0, 7)
      pure $ '+' : mini <> maxi

instance Arbitrary PhonePrefix where
  arbitrary = PhonePrefix . ST.pack <$> do
      let mkdigits n = replicateM n (elements ['0'..'9'])
      mini <- mkdigits 1
      maxi <- mkdigits =<< choose (0, 14)
      pure $ '+' : mini <> maxi

instance Arbitrary ExcludedPrefix where
    arbitrary = ExcludedPrefix <$> arbitrary <*> arbitrary

instance Arbitrary UserIdentity where
  arbitrary = oneof
    [ FullIdentity  <$> arbitrary <*> arbitrary
    , EmailIdentity <$> arbitrary
    , PhoneIdentity <$> arbitrary
    , SSOIdentity   <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary UserSSOId where
  arbitrary = UserSSOId <$> arbitrary <*> arbitrary

instance Arbitrary AssetSize where
  arbitrary = genEnumBounded

instance Arbitrary Asset where
  arbitrary = ImageAsset <$> arbitrary <*> arbitrary


-- TODO: since new team members do not get serialized, we zero them here.  it may be worth looking
-- into how this can be solved on in the types.
instance Arbitrary BindingNewTeamUser where
    arbitrary = BindingNewTeamUser
        <$> (BindingNewTeam . (newTeamMembers .~ Nothing) <$> arbitrary @(NewTeam ()))
        <*> arbitrary
    shrink (BindingNewTeamUser (BindingNewTeam nt) cur) =
        BindingNewTeamUser <$> (BindingNewTeam <$> shrink nt) <*> [cur]

instance Arbitrary Alpha where
    arbitrary = genEnumBounded

instance Arbitrary (NewTeam ()) where
    -- TODO: the aeson instances of 'BindingNewTeam', 'NonBindingNewTeam' do not render
    -- '_newTeamMembers'.  we need to either remove that field, or replace '()' with 'Void' in
    -- the definition of 'BindingNewTeam', 'NonBindingNewTeam' to make sure bad values cannot
    -- be constructed.
    arbitrary = NewTeam <$> arbitrary <*> arbitrary <*> arbitrary <*> pure Nothing
    shrink (NewTeam x0 x1 x2 x3) = NewTeam <$> shrink x0 <*> shrink x1 <*> shrink x2 <*> shrink x3

instance Arbitrary CheckHandles where
    arbitrary = CheckHandles <$> arbitrary <*> arbitrary

instance Arbitrary CompletePasswordReset where
    arbitrary = CompletePasswordReset <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary PasswordResetCode where
    arbitrary = PasswordResetCode . fromRange <$> genRangeAsciiBase64Url @6 @1024

instance Arbitrary PasswordResetIdentity where
    arbitrary = oneof
        [ PasswordResetIdentityKey . PasswordResetKey <$> arbitrary
        , PasswordResetEmailIdentity <$> arbitrary
        , PasswordResetPhoneIdentity <$> arbitrary
        ]

instance Arbitrary AsciiBase64Url where
    arbitrary = encodeBase64Url <$> arbitrary

instance Arbitrary (AsciiText Printable) where
    arbitrary = do
        txt <- Text.filter (\c -> Unicode.isAscii c && Unicode.isPrint c) <$> arbitrary
        either (error . show) pure $ validate txt

instance Arbitrary ReAuthUser where
    arbitrary = ReAuthUser <$> arbitrary

instance Arbitrary DeleteUser where
    arbitrary = DeleteUser <$> arbitrary

instance Arbitrary DeletionCodeTimeout where
    arbitrary = DeletionCodeTimeout <$> arbitrary

instance Arbitrary Timeout where
    arbitrary = Timeout . fromIntegral <$> arbitrary @Int

instance Arbitrary EmailRemove where
    arbitrary = EmailRemove <$> arbitrary

instance Arbitrary EmailUpdate where
    arbitrary = EmailUpdate <$> arbitrary

instance Arbitrary HandleUpdate where
    arbitrary = HandleUpdate <$> arbitrary

instance Arbitrary LocaleUpdate where
    arbitrary = LocaleUpdate <$> arbitrary

instance Arbitrary ManagedByUpdate where
    arbitrary = ManagedByUpdate <$> arbitrary

instance Arbitrary NewPasswordReset where
    arbitrary = NewPasswordReset <$> arbitrary

instance Arbitrary NewUser where
    arbitrary = do
        newUserIdentity <- arbitrary
        teamid <- arbitrary
        let hasSSOId = case newUserIdentity of
                Just SSOIdentity {} -> True
                _ -> False
            ssoOrigin = Just (NewUserOriginTeamUser (NewTeamMemberSSO teamid))
            isSsoOrigin (Just (NewUserOriginTeamUser (NewTeamMemberSSO _))) = True
            isSsoOrigin _ = False
        newUserOrigin <- if hasSSOId then pure ssoOrigin else arbitrary `suchThat` (not . isSsoOrigin)
        let isTeamUser = case newUserOrigin of
                Just (NewUserOriginTeamUser _) -> True
                _ -> False
        newUserName       <- arbitrary
        newUserUUID       <- elements [Just nil, Nothing]
        newUserPict       <- arbitrary
        newUserAssets     <- arbitrary
        newUserAccentId   <- arbitrary
        newUserEmailCode  <- arbitrary
        newUserPhoneCode  <- arbitrary
        newUserLabel      <- arbitrary
        newUserLocale     <- arbitrary
        newUserPassword   <- if isTeamUser && not hasSSOId then Just <$> arbitrary else arbitrary
        newUserExpiresIn  <- if isJust newUserIdentity then pure Nothing else arbitrary
        newUserManagedBy  <- arbitrary
        pure NewUser{..}

instance Arbitrary UTCTimeMillis where
    arbitrary = toUTCTimeMillis <$> arbitrary

instance Arbitrary NewUserOrigin where
    arbitrary = oneof
        [ NewUserOriginInvitationCode <$> arbitrary
        , NewUserOriginTeamUser <$> arbitrary
        ]

instance Arbitrary Pict where -- ('Pict' is DEPRECATED)
    arbitrary = pure $ Pict []

instance Arbitrary ActivationCode where
    arbitrary = ActivationCode <$> arbitrary
    shrink (ActivationCode x) = ActivationCode <$> shrink x

instance Arbitrary InvitationCode where
    arbitrary = InvitationCode <$> arbitrary
    shrink (InvitationCode x) = InvitationCode <$> shrink x

instance Arbitrary CookieLabel where
    arbitrary = CookieLabel <$> arbitrary
    shrink (CookieLabel x) = CookieLabel <$> shrink x

instance Arbitrary NewTeamUser where
    arbitrary = oneof
        [ NewTeamMember <$> arbitrary
        , NewTeamCreator <$> arbitrary
        , NewTeamMemberSSO <$> arbitrary
        ]

instance Arbitrary TeamMember where
    arbitrary = newTeamMember <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary PasswordChange where
    arbitrary = PasswordChange <$> arbitrary <*> arbitrary

instance Arbitrary PhoneRemove where
    arbitrary = PhoneRemove <$> arbitrary

instance Arbitrary PhoneUpdate where
    arbitrary = PhoneUpdate <$> arbitrary

instance Arbitrary SelfProfile where
    arbitrary = SelfProfile <$> arbitrary

instance Arbitrary UserHandleInfo where
    arbitrary = UserHandleInfo <$> arbitrary

instance Arbitrary UserProfile where
    arbitrary = UserProfile
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary RichField where
    arbitrary = RichField <$> arbitrary <*> arbitrary

instance Arbitrary RichInfo where
    arbitrary = do
        richInfoFields <- nubOn richFieldType <$> arbitrary
        richInfoVersion <- pure RichInfoVersion0
        pure RichInfo{..}

instance Arbitrary RichInfoUpdate where
    arbitrary = RichInfoUpdate <$> arbitrary

instance Arbitrary ServiceRef where
    arbitrary = ServiceRef <$> arbitrary <*> arbitrary

instance Arbitrary UserUpdate where
    arbitrary = UserUpdate
        <$> arbitrary
        <*> pure Nothing
        <*> arbitrary
        <*> arbitrary

instance Arbitrary User where
    arbitrary = User
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary VerifyDeleteUser where
    arbitrary = VerifyDeleteUser <$> arbitrary <*> arbitrary

instance Arbitrary Key where
    arbitrary = Key <$> genRangeAsciiBase64Url @20 @20

instance Arbitrary Brig.Types.Code.Value where
    arbitrary = Value <$> genRangeAsciiBase64Url @6 @20

instance Arbitrary Locale where
    arbitrary = Locale <$> arbitrary <*> arbitrary

instance Arbitrary Language where
    arbitrary = Language <$> genEnumBounded

-- | <https://github.com/HugoDaniel/iso639/pull/4>
deriving instance Bounded ISO639_1

instance Arbitrary Country where
    arbitrary = Country <$> genEnumBounded

instance Arbitrary UpdateServiceWhitelist where
    arbitrary = UpdateServiceWhitelist <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary InvitationList where
    arbitrary = InvitationList <$> listOf arbitrary <*> arbitrary

instance Arbitrary Invitation where
    arbitrary = Invitation <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Permissions where
    arbitrary = maybe (error "instance Arbitrary Permissions") pure =<< do
        selfperms <- arbitrary
        copyperms <- Set.intersection selfperms <$> arbitrary
        pure $ newPermissions selfperms copyperms

instance Arbitrary Perm where
    arbitrary = elements [minBound..]

instance Arbitrary InvitationRequest where
    arbitrary = InvitationRequest <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Role where
    arbitrary = elements [minBound..]

instance Arbitrary ManagedBy where
    arbitrary = elements [minBound..]

----------------------------------------------------------------------
-- utilities

instance (KnownNat n, KnownNat m, LTE n m) => Arbitrary (Range n m ST.Text) where
    arbitrary = genRangeText arbitrary
    shrink (fromRange -> txt) = [unsafeRange @ST.Text @n @m $ ST.take (fromKnownNat (Proxy @n)) txt]

instance (KnownNat n, KnownNat m, LTE n m) => Arbitrary (Range n m Integer) where
    arbitrary = arbitraryIntegral

instance (KnownNat n, KnownNat m, LTE n m) => Arbitrary (Range n m Word) where
    arbitrary = arbitraryIntegral

instance (KnownNat n, KnownNat m, LTE n m, Arbitrary a, Show a) => Arbitrary (Range n m [a]) where
    arbitrary = genRangeList @n @m @a arbitrary

arbitraryIntegral :: forall n m i.
                     (KnownNat n, KnownNat m, LTE n m, Integral i, Show i, Bounds i, System.Random.Random i)
                  => Gen (Range n m i)
arbitraryIntegral = unsafeRange @i @n @m <$> choose (fromKnownNat (Proxy @n), fromKnownNat (Proxy @m))

fromKnownNat :: forall (k :: Nat) (i :: *). (Num i, KnownNat k) => Proxy k -> i
fromKnownNat p = fromIntegral $ natVal p

-- (can we implement this also in terms of 'genRange'?)
genRangeAsciiBase64Url :: forall (n :: Nat) (m :: Nat).
                          (HasCallStack, KnownNat n, KnownNat m, LTE n m)
                       => Gen (Range n m AsciiBase64Url)
genRangeAsciiBase64Url = do
    txt <- fromRange <$> genRangeText @n @m genBase64UrlChar
    case validateBase64Url txt of
        Right ascii -> pure $ unsafeRange @AsciiBase64Url @n @m ascii
        Left msg    -> error msg

genBase64UrlChar :: Gen Char
genBase64UrlChar = elements $ alphaNumChars <> "_-="

genAlphaNum :: Gen Char
genAlphaNum = elements $ alphaNumChars <> "_"

alphaNumChars :: [Char]
alphaNumChars = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']

genEnumBounded :: (Enum a, Bounded a) => Gen a
genEnumBounded = elements [minBound..]

instance Arbitrary UserLegalHoldStatusResponse where
    arbitrary = UserLegalHoldStatusResponse <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary LegalHoldStatus where
    arbitrary = genEnumBounded

instance Arbitrary LegalHoldTeamConfig where
    arbitrary = LegalHoldTeamConfig <$> arbitrary

instance Arbitrary NewLegalHoldService where
    arbitrary = NewLegalHoldService <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary LegalHoldService where
    arbitrary = LegalHoldService <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ViewLegalHoldService where
    arbitrary = oneof
        [ ViewLegalHoldService <$> arbitrary
        , pure ViewLegalHoldServiceNotConfigured
        , pure ViewLegalHoldServiceDisabled
        ]

instance Arbitrary ViewLegalHoldServiceInfo where
    arbitrary = ViewLegalHoldServiceInfo <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary HttpsUrl where
    arbitrary = pure $ HttpsUrl [uri|https://example.com|]

instance Arbitrary ServiceKeyType where
    arbitrary = genEnumBounded

instance Arbitrary ServiceKey where
    arbitrary = ServiceKey <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ServiceKeyPEM where
    arbitrary = pure $ ServiceKeyPEM k
      where Right [k] = pemParseBS . BS.unlines $
              [ "-----BEGIN PUBLIC KEY-----"
              , "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAu+Kg/PHHU3atXrUbKnw0"
              , "G06FliXcNt3lMwl2os5twEDcPPFw/feGiAKymxp+7JqZDrseS5D9THGrW+OQRIPH"
              , "WvUBdiLfGrZqJO223DB6D8K2Su/odmnjZJ2z23rhXoEArTplu+Dg9K+c2LVeXTKV"
              , "VPOaOzgtAB21XKRiQ4ermqgi3/njr03rXyq/qNkuNd6tNcg+HAfGxfGvvCSYBfiS"
              , "bUKr/BeArYRcjzr/h5m1In6fG/if9GEI6m8dxHT9JbY53wiksowy6ajCuqskIFg8"
              , "7X883H+LA/d6X5CTiPv1VMxXdBUiGPuC9IT/6CNQ1/LFt0P37ax58+LGYlaFo7la"
              , "nQIDAQAB"
              , "-----END PUBLIC KEY-----"
              ]

instance Arbitrary (Fingerprint Rsa) where
    arbitrary = pure $ Fingerprint
        "\138\140\183\EM\226#\129\EOTl\161\183\246\DLE\161\142\220\239&\171\241h|\\GF\172\180O\129\DC1!\159"

instance Arbitrary ServiceToken where
    arbitrary = ServiceToken <$> arbitrary

instance Arbitrary RequestNewLegalHoldClient where
    arbitrary = RequestNewLegalHoldClient <$> arbitrary <*> arbitrary

instance Arbitrary NewLegalHoldClient where
    arbitrary = NewLegalHoldClient <$> arbitrary <*> arbitrary

instance Arbitrary LegalHoldClientRequest where
    arbitrary =
        LegalHoldClientRequest
            <$> arbitrary
            <*> arbitrary

instance Arbitrary LegalHoldServiceConfirm where
    arbitrary =
        LegalHoldServiceConfirm
          <$> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary

instance Arbitrary RemoveLegalHoldSettingsRequest where
    arbitrary = RemoveLegalHoldSettingsRequest <$> arbitrary

instance Arbitrary DisableLegalHoldForUserRequest where
    arbitrary = DisableLegalHoldForUserRequest <$> arbitrary

instance Arbitrary ApproveLegalHoldForUserRequest where
    arbitrary = ApproveLegalHoldForUserRequest <$> arbitrary

instance Arbitrary LastPrekey where
    arbitrary = lastPrekey <$> arbitrary

instance Arbitrary Prekey where
    arbitrary = Prekey <$> arbitrary <*> arbitrary

instance Arbitrary PrekeyId where
    arbitrary = PrekeyId <$> arbitrary


instance {-# OVERLAPPABLE #-} (Enum a, Bounded a) => Arbitrary a where
  arbitrary = Test.Tasty.QuickCheck.elements [minBound..]

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
  arbitrary = FeatureFlags <$> arbitrary <*> arbitrary

instance Arbitrary AccountStatusObject where
  arbitrary = AccountStatusObject <$> arbitrary

instance Arbitrary AccountStatusUpdate where
  arbitrary = AccountStatusUpdate <$> arbitrary

instance Arbitrary ActivationCodeObject where
  arbitrary = ActivationCodeObject <$> arbitrary <*> arbitrary

instance Arbitrary ActivationKey where
  arbitrary = ActivationKey <$> arbitrary

instance Arbitrary BindingNewTeam where
  arbitrary = BindingNewTeam <$> arbitrary

instance Arbitrary Client where
  arbitrary = Client
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Location where
  arbitrary = location
    <$> (Latitude <$> arbitrary)
    <*> (Longitude <$> arbitrary)

instance Arbitrary ConnectionsStatusRequest where
  arbitrary = ConnectionsStatusRequest <$> arbitrary <*> arbitrary

instance Arbitrary ConnectionStatus where
  arbitrary = ConnectionStatus <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Conversation where
  arbitrary = Conversation
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink conv = filter (/= conv)
                [ conv { cnvAccess = [] }
                , conv { cnvName = Nothing }
                , conv { cnvMembers = (cnvMembers conv) { cmOthers = [] } }
                ]

instance Arbitrary ConvMembers where
  arbitrary = ConvMembers <$> arbitrary <*> arbitrary

instance Arbitrary Member where
  arbitrary = Member
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary MutedStatus where
  arbitrary = MutedStatus <$> arbitrary

instance Arbitrary OtherMember where
  arbitrary = OtherMember
    <$> arbitrary
    <*> arbitrary

instance Arbitrary ReceiptMode where
  arbitrary = ReceiptMode <$> arbitrary

instance Arbitrary Message where
  arbitrary = Message . cs <$> (vector @Char =<< choose (1, 256))  -- TODO: hide 'Message' and
                                                                   -- expose a smart
                                                                   -- constructor that ensures
                                                                   -- this.

instance Arbitrary PropertyKey where
  arbitrary = PropertyKey <$> arbitrary

instance Arbitrary PropertyValue where
  arbitrary = PropertyValue <$> arbitrary

instance Arbitrary QueuedNotification where
  arbitrary = queuedNotification
    <$> arbitrary
    <*> (List1.list1 <$> scale (`div` 3) arbitrary <*> scale (`div` 3) arbitrary)

instance Arbitrary Team where
  arbitrary = Team
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary TeamData where
  arbitrary = TeamData
    <$> arbitrary
    <*> arbitrary
    <*> (fromUTCTimeMillis . toUTCTimeMillis <$$> arbitrary)
    -- TODO: aeson serialization chops off everything after milliseconds.  this should happen
    -- in a smart constructor and TeamData should be opaque.

instance Arbitrary UserAccount where
  arbitrary = UserAccount
    <$> arbitrary
    <*> arbitrary

instance Arbitrary UserClients where
  arbitrary = UserClients <$> arbitrary

instance Arbitrary UserConnection where
  arbitrary = UserConnection
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary UserIds where
  arbitrary = UserIds <$> arbitrary

instance Arbitrary UserSet where
  arbitrary = UserSet <$> arbitrary

instance Arbitrary CookieList where
  arbitrary = CookieList <$> arbitrary

instance Arbitrary (Cookie ()) where
  arbitrary = Cookie
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary CookieId where
  arbitrary = CookieId <$> arbitrary

instance Arbitrary Contact where
  arbitrary = Contact
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary (SearchResult Contact) where
  arbitrary = SearchResult
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary URI.ByteString.URI where
  arbitrary = do
    schema <- elements ["http", "https"]
    domain <- elements ["example.com", "some.where", "no.when"]
    path   <- elements ["/", "/some/path", "other/path"]
    either (error . show) pure
      . URI.ByteString.parseURI URI.ByteString.laxURIParserOptions
      $ schema <> "://" <> domain <> path

instance Arbitrary Aeson.Value where
  arbitrary = oneof [ Aeson.Object <$> scale (`div` 3) arbitrary
                    , Aeson.Array  <$> scale (`div` 3) arbitrary
                    , Aeson.String <$> arbitrary
                    , Aeson.Number <$> arbitrary
                    , Aeson.Bool   <$> arbitrary
                    , pure Aeson.Null
                    ]
  shrink (Aeson.Object obj) = Aeson.Object <$> shrink obj
  shrink (Aeson.Array arr)  = Aeson.Array  <$> shrink arr
  shrink _                  = []

instance (KnownNat n, KnownNat m, LTE n m) => Arbitrary (Range n m Int32) where
  arbitrary = unsafeRange <$> choose @Int32 ( fromIntegral $ natVal (Proxy @n)
                                            , fromIntegral $ natVal (Proxy @m)
                                            )

#endif

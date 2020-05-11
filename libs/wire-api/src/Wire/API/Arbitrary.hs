{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.API.Arbitrary where

import Control.Lens hiding (elements)
import qualified Data.ByteString.Char8 as BS
import Data.Code (Timeout (Timeout))
import Data.Currency
import Data.IP
import Data.Json.Util (UTCTimeMillis (..), toUTCTimeMillis)
import Data.LanguageCodes
import Data.List.Extra (nubOn)
import Data.Misc
import Data.PEM (pemParseBS)
import Data.Proxy
import Data.Range
import qualified Data.Set as Set
import qualified Data.Text as ST
import Data.Text.Ascii
import Data.Text.Encoding (decodeUtf8)
import Data.UUID (nil)
import GHC.TypeLits
import Imports
import qualified System.Random
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Text.Hostname
import URI.ByteString.QQ (uri)
import Wire.API.Call.TURN
import Wire.API.Conversation.Code as Code
import Wire.API.CustomBackend
import Wire.API.Provider.Service
import Wire.API.Team
import Wire.API.Team.Feature
import Wire.API.Team.Invitation
import Wire.API.Team.LegalHold
import Wire.API.Team.LegalHold.External
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Auth
import Wire.API.User.Client.Prekey
import Wire.API.User.Handle
import Wire.API.User.Identity
import Wire.API.User.Password
import Wire.API.User.Profile
import Wire.API.User.RichInfo

newtype Octet = Octet {octet :: Word16}
  deriving (Eq, Show)

instance Arbitrary Octet where
  arbitrary = Octet <$> arbitrary `suchThat` (< 256)

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
  arbitrary =
    oneof
      [ TurnHostIp <$> arbitrary,
        TurnHostName . decodeUtf8 <$> arbitrary `suchThat` validHostname
      ]

instance Arbitrary Port where
  arbitrary = Port <$> arbitrary

instance Arbitrary Transport where
  arbitrary = genEnumBounded

instance Arbitrary TurnURI where
  arbitrary =
    turnURI <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary Name where
  arbitrary =
    Name . ST.pack
      <$> ((`replicateM` arbitrary) =<< choose (1, 128))

instance Arbitrary ColourId where
  arbitrary = ColourId <$> arbitrary

instance Arbitrary Email where
  arbitrary = do
    localPart <- ST.filter (/= '@') <$> arbitrary
    domain <- ST.filter (/= '@') <$> arbitrary
    pure $ Email localPart domain

instance Arbitrary Phone where
  arbitrary = Phone . ST.pack <$> do
    let mkdigits n = replicateM n (elements ['0' .. '9'])
    mini <- mkdigits 8
    maxi <- mkdigits =<< choose (0, 7)
    pure $ '+' : mini <> maxi

instance Arbitrary UserIdentity where
  arbitrary =
    oneof
      [ FullIdentity <$> arbitrary <*> arbitrary,
        EmailIdentity <$> arbitrary,
        PhoneIdentity <$> arbitrary,
        SSOIdentity <$> arbitrary <*> arbitrary <*> arbitrary
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
  arbitrary =
    BindingNewTeamUser
      <$> (BindingNewTeam . (newTeamMembers .~ Nothing) <$> arbitrary @(NewTeam ()))
      <*> arbitrary
  shrink (BindingNewTeamUser (BindingNewTeam nt) cur) =
    BindingNewTeamUser <$> (BindingNewTeam <$> shrink nt) <*> [cur]

instance Arbitrary Alpha where
  arbitrary = genEnumBounded

instance Arbitrary (NewTeam ()) where
  arbitrary = NewTeam <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (NewTeam x0 x1 x2 x3) = NewTeam <$> shrink x0 <*> shrink x1 <*> shrink x2 <*> shrink x3

instance Arbitrary CheckHandles where
  arbitrary = CheckHandles <$> arbitrary <*> arbitrary

instance Arbitrary CompletePasswordReset where
  arbitrary = CompletePasswordReset <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary PasswordResetCode where
  arbitrary = PasswordResetCode . fromRange <$> genRangeAsciiBase64Url @6 @1024

instance Arbitrary PasswordResetIdentity where
  arbitrary =
    oneof
      [ PasswordResetIdentityKey . PasswordResetKey <$> arbitrary,
        PasswordResetEmailIdentity <$> arbitrary,
        PasswordResetPhoneIdentity <$> arbitrary
      ]

instance Arbitrary AsciiBase64Url where
  arbitrary = encodeBase64Url <$> arbitrary

instance Arbitrary DeleteUser where
  arbitrary = DeleteUser <$> arbitrary

instance Arbitrary DeletionCodeTimeout where
  arbitrary = DeletionCodeTimeout <$> arbitrary

instance Arbitrary Timeout where
  arbitrary = Timeout . fromIntegral <$> arbitrary @Int

instance Arbitrary EmailUpdate where
  arbitrary = EmailUpdate <$> arbitrary

instance Arbitrary HandleUpdate where
  arbitrary = HandleUpdate <$> arbitrary

instance Arbitrary LocaleUpdate where
  arbitrary = LocaleUpdate <$> arbitrary

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
    newUserDisplayName <- arbitrary
    newUserUUID <- elements [Just nil, Nothing]
    newUserPict <- arbitrary
    newUserAssets <- arbitrary
    newUserAccentId <- arbitrary
    newUserEmailCode <- arbitrary
    newUserPhoneCode <- arbitrary
    newUserLabel <- arbitrary
    newUserLocale <- arbitrary
    newUserPassword <- if isTeamUser && not hasSSOId then Just <$> arbitrary else arbitrary
    newUserExpiresIn <- if isJust newUserIdentity then pure Nothing else arbitrary
    newUserManagedBy <- arbitrary
    pure NewUser {..}

instance Arbitrary UTCTimeMillis where
  arbitrary = toUTCTimeMillis <$> arbitrary

instance Arbitrary NewUserOrigin where
  arbitrary =
    oneof
      [ NewUserOriginInvitationCode <$> arbitrary,
        NewUserOriginTeamUser <$> arbitrary
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
  arbitrary =
    oneof
      [ NewTeamMember <$> arbitrary,
        NewTeamCreator <$> arbitrary,
        NewTeamMemberSSO <$> arbitrary
      ]

instance Arbitrary TeamMember where
  arbitrary = newTeamMember <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary PhoneUpdate where
  arbitrary = PhoneUpdate <$> arbitrary

instance Arbitrary SelfProfile where
  arbitrary = SelfProfile <$> arbitrary

instance Arbitrary UserHandleInfo where
  arbitrary = UserHandleInfo <$> arbitrary

instance Arbitrary UserProfile where
  arbitrary =
    UserProfile
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
  arbitrary =
    RichField
      <$> arbitrary
      <*> (arbitrary `suchThat` (/= "")) -- This is required because FromJSON calls @normalizeRichInfo@ and roundtrip tests fail

instance Arbitrary RichInfo where
  arbitrary = do
    richInfoAssocList <- nubOn richFieldType <$> arbitrary
    richInfoMap <- arbitrary
    pure RichInfo {..}

instance Arbitrary RichInfoAssocList where
  arbitrary = RichInfoAssocList <$> nubOn richFieldType <$> arbitrary

instance Arbitrary ServiceRef where
  arbitrary = ServiceRef <$> arbitrary <*> arbitrary

instance Arbitrary UserUpdate where
  arbitrary =
    UserUpdate
      <$> arbitrary
      <*> pure Nothing
      <*> arbitrary
      <*> arbitrary

instance Arbitrary User where
  arbitrary =
    User
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

instance Arbitrary Code.Value where
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
  arbitrary = Invitation <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Permissions where
  arbitrary = maybe (error "instance Arbitrary Permissions") pure =<< do
    selfperms <- arbitrary
    copyperms <- Set.intersection selfperms <$> arbitrary
    pure $ newPermissions selfperms copyperms

instance Arbitrary Perm where
  arbitrary = elements [minBound ..]

instance Arbitrary InvitationRequest where
  arbitrary = InvitationRequest <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Role where
  arbitrary = elements [minBound ..]

instance Arbitrary ManagedBy where
  arbitrary = elements [minBound ..]

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

arbitraryIntegral ::
  forall n m i.
  (KnownNat n, KnownNat m, LTE n m, Integral i, Show i, Bounds i, System.Random.Random i) =>
  Gen (Range n m i)
arbitraryIntegral = unsafeRange @i @n @m <$> choose (fromKnownNat (Proxy @n), fromKnownNat (Proxy @m))

fromKnownNat :: forall (k :: Nat) (i :: *). (Num i, KnownNat k) => Proxy k -> i
fromKnownNat p = fromIntegral $ natVal p

-- (can we implement this also in terms of 'genRange'?)
genRangeAsciiBase64Url ::
  forall (n :: Nat) (m :: Nat).
  (HasCallStack, KnownNat n, KnownNat m, LTE n m) =>
  Gen (Range n m AsciiBase64Url)
genRangeAsciiBase64Url = do
  txt <- fromRange <$> genRangeText @n @m genBase64UrlChar
  case validateBase64Url txt of
    Right ascii -> pure $ unsafeRange @AsciiBase64Url @n @m ascii
    Left msg -> error msg

genBase64UrlChar :: Gen Char
genBase64UrlChar = elements $ alphaNumChars <> "_-="

genAlphaNum :: Gen Char
genAlphaNum = elements $ alphaNumChars <> "_"

alphaNumChars :: [Char]
alphaNumChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']

genEnumBounded :: (Enum a, Bounded a) => Gen a
genEnumBounded = elements [minBound ..]

instance Arbitrary UserLegalHoldStatusResponse where
  arbitrary = UserLegalHoldStatusResponse <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary LegalHoldStatus where
  arbitrary = genEnumBounded

instance Arbitrary LegalHoldTeamConfig where
  arbitrary = LegalHoldTeamConfig <$> arbitrary

instance Arbitrary NewLegalHoldService where
  arbitrary = NewLegalHoldService <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ViewLegalHoldService where
  arbitrary =
    oneof
      [ ViewLegalHoldService <$> arbitrary,
        pure ViewLegalHoldServiceNotConfigured,
        pure ViewLegalHoldServiceDisabled
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
    where
      Right [k] =
        pemParseBS . BS.unlines $
          [ "-----BEGIN PUBLIC KEY-----",
            "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAu+Kg/PHHU3atXrUbKnw0",
            "G06FliXcNt3lMwl2os5twEDcPPFw/feGiAKymxp+7JqZDrseS5D9THGrW+OQRIPH",
            "WvUBdiLfGrZqJO223DB6D8K2Su/odmnjZJ2z23rhXoEArTplu+Dg9K+c2LVeXTKV",
            "VPOaOzgtAB21XKRiQ4ermqgi3/njr03rXyq/qNkuNd6tNcg+HAfGxfGvvCSYBfiS",
            "bUKr/BeArYRcjzr/h5m1In6fG/if9GEI6m8dxHT9JbY53wiksowy6ajCuqskIFg8",
            "7X883H+LA/d6X5CTiPv1VMxXdBUiGPuC9IT/6CNQ1/LFt0P37ax58+LGYlaFo7la",
            "nQIDAQAB",
            "-----END PUBLIC KEY-----"
          ]

instance Arbitrary (Fingerprint Rsa) where
  arbitrary =
    pure $
      Fingerprint
        "\138\140\183\EM\226#\129\EOTl\161\183\246\DLE\161\142\220\239&\171\241h|\\GF\172\180O\129\DC1!\159"

instance Arbitrary ServiceToken where
  arbitrary = ServiceToken <$> arbitrary

instance Arbitrary RequestNewLegalHoldClient where
  arbitrary = RequestNewLegalHoldClient <$> arbitrary <*> arbitrary

instance Arbitrary NewLegalHoldClient where
  arbitrary = NewLegalHoldClient <$> arbitrary <*> arbitrary

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

instance Arbitrary CustomBackend where
  arbitrary =
    CustomBackend
      <$> arbitrary
      <*> arbitrary

instance Arbitrary PasswordChange where
  arbitrary = PasswordChange <$> arbitrary <*> arbitrary

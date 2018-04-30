{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Brig.Types.Arbitrary where

import Brig.Types.Activation
import Brig.Types.Code
import Brig.Types.TURN
import Brig.Types.User
import Brig.Types.User.Auth
import Control.Lens hiding (elements)
import Control.Monad
import Data.Currency
import Data.Aeson
import Data.Either
import Data.IP
import Data.Json.Util (UTCTimeMillis (..))
import Data.LanguageCodes
import Data.Maybe
import Data.Misc
import Data.Monoid
import Data.Range
import Data.Text.Ascii
import Data.Typeable
import Data.Word
import Galley.Types.Bot.Service.Internal
import Galley.Types.Teams
import Galley.Types.Teams.Internal
import GHC.Stack
import GHC.TypeLits
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import qualified Data.Text as ST
import qualified System.Random


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
    arbitrary = TurnHost <$> arbitrary

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
      local  <- ST.filter (/= '@') <$> arbitrary
      domain <- ST.filter (/= '@') <$> arbitrary
      pure $ Email local domain

instance Arbitrary Phone where
  arbitrary = Phone . ST.pack <$> do
      let mkdigits n = replicateM n (elements ['0'..'9'])
      mini <- mkdigits 8
      maxi <- mkdigits =<< choose (0, 7)
      pure $ '+' : mini <> maxi

instance Arbitrary UserIdentity where
  arbitrary = oneof
    [ FullIdentity  <$> arbitrary <*> arbitrary
    , EmailIdentity <$> arbitrary
    , PhoneIdentity <$> arbitrary
    , SSOIdentity   <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary UserSSOId where
  arbitrary = UserSSOId <$> arbitrary

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
    arbitrary = NewTeam <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
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

instance Arbitrary PlainTextPassword where
    arbitrary = PlainTextPassword . fromRange <$> genRangeText @6 @1024 arbitrary

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

instance Arbitrary NewPasswordReset where
    arbitrary = NewPasswordReset <$> arbitrary

instance Arbitrary NewUser where
    arbitrary = do
        newUserIdentity <- arbitrary
        let hasSSOId = case newUserIdentity of
                Just SSOIdentity {} -> True
                _ -> False
            ssoOrigin = Just (NewUserOriginTeamUser NewTeamMemberSSO)
        newUserOrigin <- if hasSSOId then pure ssoOrigin else arbitrary `suchThat` (/= ssoOrigin)
        let isTeamUser = case newUserOrigin of
                Just (NewUserOriginTeamUser _) -> True
                _ -> False
        newUserName       <- arbitrary
        newUserPict       <- arbitrary
        newUserAssets     <- arbitrary
        newUserAccentId   <- arbitrary
        newUserEmailCode  <- arbitrary
        newUserPhoneCode  <- arbitrary
        newUserLabel      <- arbitrary
        newUserLocale     <- arbitrary
        newUserPassword   <- if isTeamUser && not hasSSOId then Just <$> arbitrary else arbitrary
        newUserExpiresIn  <- if isJust newUserIdentity then pure Nothing else arbitrary
        pure NewUser{..}

instance Arbitrary UTCTimeMillis where
    arbitrary = fromRight (error "instance Arbitrary UTCTimeMillis")
              . eitherDecode . encode . UTCTimeMillis
            <$> arbitrary

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
        , pure NewTeamMemberSSO
        ]

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
        <*> (fromUTCTimeMillis <$$> arbitrary)
        <*> arbitrary

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
        <*> (fromUTCTimeMillis <$$> arbitrary)
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

genRangeList :: forall (n :: Nat) (m :: Nat) (a :: *).
                (Show a, KnownNat n, KnownNat m, LTE n m)
             => Gen a -> Gen (Range n m [a])
genRangeList = genRange id

genRangeText :: forall (n :: Nat) (m :: Nat). (KnownNat n, KnownNat m, LTE n m)
             => Gen Char -> Gen (Range n m ST.Text)
genRangeText = genRange ST.pack

genRange :: forall (n :: Nat) (m :: Nat) (a :: *) (b :: *).
            (Show b, Bounds b, KnownNat n, KnownNat m, LTE n m)
         => ([a] -> b) -> Gen a -> Gen (Range n m b)
genRange pack gc = unsafeRange @b @n @m . pack <$> grange (fromKnownNat (Proxy @n)) (fromKnownNat (Proxy @m)) gc
  where
    grange mi ma gelem = (`replicateM` gelem) =<< choose (mi, ma)

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

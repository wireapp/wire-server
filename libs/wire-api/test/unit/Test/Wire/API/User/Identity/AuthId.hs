{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Wire.API.User.Identity.AuthId (specs) where

import Data.Function
import Data.Id
import Data.Set as Set
import Data.UUID
import Imports
import qualified SAML2.WebSSO.Types as SAML
import Test.Hspec
import Test.Hspec.QuickCheck
import URI.ByteString.QQ
import Wire.API.User.Identity

specs :: Spec
specs = do
  describe "AuthId markers" $ do
    it "validStates, validStatesExpected are in sync" $ do
      validStates `shouldBe` (fst `Set.map` validStatesExpected)
    it "all validStateExpected entries have examples" $ do
      forM_ (Set.toList validStatesExpected) $ \(_, examples) -> do
        examples `shouldNotBe` mempty
    prop "arbitrary AuthId values are not missing in validStatesExpected" $ \(authId :: AuthId) -> do
      allSamples `shouldContain` [Equiv authId]

  describe "what's an email" $ do
    it "1" $ parseEmail "@handle" `shouldBe` Nothing
    it "2" $ parseEmail "onlylocal@" `shouldBe` Nothing
    it "3" $ parseEmail "@" `shouldBe` Nothing
    it "4" $ parseEmail "local@only-top-level" `shouldSatisfy` isJust

  describe "AuthId combinators" $ do
    it "runAuthId" $ do
      pending

    it "authIdUref" $ do
      pending

    it "authIdEmail" $ do
      pending

    it "authIdScimDetails" $ do
      pending

    it "authIdScimEmail" $ do
      pending

    it "authIdScimEmailWithSource" $ do
      pending

    it "authIdToLegacyAuthId" $ do
      pending

    it "externalIdTeam" $ do
      pending

    it "externalIdName" $ do
      pending

-- | We support a specific set of sub-sets of these things a user can have (or do, or
-- experience).  This is a simplified model, but it should be accurate enough.
data Marker
  = MEmail
  | MEmailValidated
  | MPassword
  | MSCIM
  | MSAML
  deriving (Ord, Enum, Bounded, Eq, Show)

type State = Set Marker

-- | An enumeration of all combinations of 'Marker's that are allowed, plus examples.
validStatesExpected :: Set (State, [AuthId])
validStatesExpected =
  fromList
    [ ( fromList [MEmail, MEmailValidated, MPassword, MSCIM],
        [ AuthSCIM (ScimDetails sampleExternalId (sampleEmailWithSource EmailFromExternalIdField)),
          AuthSCIM (ScimDetails sampleExternalId (sampleEmailWithSource EmailFromEmailsField))
        ]
      ),
      ( fromList [MEmail, MEmailValidated, MPassword, MSCIM, MSAML],
        [ AuthBoth sampleTeamId sampleUserRef (Just (sampleEmailWithSource EmailFromExternalIdField)),
          AuthBoth sampleTeamId sampleUserRef (Just (sampleEmailWithSource EmailFromEmailsField))
        ]
      ),
      ( fromList [MEmail, MEmailValidated, MPassword, MSAML], -- email validation is handled brig, not in AuthId
        [AuthSAML sampleUserRef]
      ),
      ( fromList [MEmail, MEmailValidated, MSCIM, MSAML], -- EmailSource can be any of the 2 cases
        [ AuthBoth sampleTeamId sampleUserRef (Just (sampleEmailWithSource EmailFromExternalIdField)),
          AuthBoth sampleTeamId sampleUserRef (Just (sampleEmailWithSource EmailFromEmailsField))
        ]
      ),
      ( fromList [MEmail, MEmailValidated, MSAML], -- uref contains an email and validateSAMLEmail feature is activated
        [AuthSAML sampleUserRef]
      ),
      ( fromList [MEmail, MSCIM, MSAML],
        [ AuthBoth sampleTeamId sampleUserRef (Just (sampleEmailWithSource EmailFromExternalIdField)),
          AuthBoth sampleTeamId sampleUserRef (Just (sampleEmailWithSource EmailFromEmailsField))
        ]
      ),
      ( fromList [MEmail, MSAML],
        [AuthSAML sampleUserRef]
      ),
      ( fromList [MSCIM, MSAML],
        [AuthBoth sampleTeamId sampleUserRef Nothing]
      ),
      ( fromList [MSAML],
        [AuthSAML sampleUserRef]
      )
    ]

allStates :: Set State
allStates =
  fromList $
    [ fromList [a, b, c, d, e]
      | a <- [minBound ..],
        b <- [minBound ..],
        c <- [minBound ..],
        d <- [minBound ..],
        e <- [minBound ..]
    ]
      <> [mempty]

-- | A logical description of which combinations of 'Marker's are allowed.
validStates :: Set State
validStates =
  allStates
    & Set.filter someAuth
    & Set.filter samlOrScim
    & Set.filter (MPassword `implies` MEmailValidated)
    & Set.filter (MEmailValidated `implies` MEmail)

-- | users need to authenticate
someAuth :: State -> Bool
someAuth s = MSAML `member` s || MPassword `member` s

-- | users have only these two ways of getting provisioned via spar.
samlOrScim :: Set Marker -> Bool
samlOrScim s = MSCIM `member` s || MSAML `member` s

implies :: Marker -> Marker -> State -> Bool
implies a b s = not (a `member` s) || (b `member` s)

allSamples :: [Equiv AuthId]
allSamples =
  Set.toList validStatesExpected
    & fmap snd
    & join
    & nub
    & fmap Equiv

data Equiv a = Equiv a
  deriving (Show)

instance Eq (Equiv AuthId) where
  (Equiv (AuthSAML _))
    == (Equiv (AuthSAML _)) = True
  (Equiv (AuthSCIM (ScimDetails _ (EmailWithSource _ source))))
    == (Equiv (AuthSCIM (ScimDetails _ (EmailWithSource _ source')))) = source == source'
  (Equiv (AuthBoth _ _ (Just (EmailWithSource _ source))))
    == (Equiv (AuthBoth _ _ (Just (EmailWithSource _ source')))) = source == source'
  (Equiv (AuthBoth _ _ Nothing))
    == (Equiv (AuthBoth _ _ Nothing)) = True
  (Equiv _)
    == (Equiv _) = False

sampleExternalId :: ExternalId
sampleExternalId = ExternalId sampleTeamId "\72313xG<#<\9794\"I(hK"

sampleEmailWithSource :: EmailSource -> EmailWithSource
sampleEmailWithSource = EmailWithSource (fromJust $ parseEmail "me@you.there")

sampleTeamId :: TeamId
sampleTeamId = Id . fromJust . Data.UUID.fromString $ "00000040-0000-0052-0000-001d00000074"

sampleUserRef :: SAML.UserRef
sampleUserRef =
  SAML.UserRef
    (SAML.Issuer [uri|https://issuer.io|])
    (fromRight (error "impossible") $ SAML.emailNameID "me@you.there")

deriving instance Ord SAML.UserRef

deriving instance Ord AuthId

deriving instance Ord ScimDetails

deriving instance Ord ExternalId

deriving instance Ord EmailWithSource

deriving instance Ord EmailSource

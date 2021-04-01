{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Spar.IdCases where

import Data.Function
import Data.Set
import Imports
import qualified SAML2.WebSSO.Types as SAML
import Test.Hspec
import Text.Show.Pretty
import qualified Wire.API.User.Identity.AuthId as Ident

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

deriving instance Ord SAML.UserRef

deriving instance Ord Ident.AuthId

deriving instance Ord Ident.ScimDetails

deriving instance Ord Ident.ExternalId

deriving instance Ord Ident.EmailWithSource

deriving instance Ord Ident.EmailSource

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
    & Data.Set.filter someAuth
    & Data.Set.filter samlOrPw
    & Data.Set.filter (MPassword `implies` MEmailValidated)
    & Data.Set.filter (MEmailValidated `implies` MEmail)

someAuth :: State -> Bool
someAuth s = MSAML `member` s || MPassword `member` s

samlOrPw :: Set Marker -> Bool
samlOrPw s = MSCIM `member` s || MSAML `member` s

implies :: Marker -> Marker -> State -> Bool
implies a b s = not (a `member` s) || (b `member` s)

-- | An enumeration of all combinations of 'Marker's that are allowed, plus examples.
validStatesExpected :: Set (State, [Ident.AuthId])
validStatesExpected =
  fromList
    [ ( fromList [MEmail, MEmailValidated, MPassword, MSCIM],
        []
      ),
      ( fromList [MEmail, MEmailValidated, MPassword, MSCIM, MSAML],
        []
      ),
      ( fromList [MEmail, MEmailValidated, MPassword, MSAML], -- email validation is handled brig, not in AuthId
        []
      ),
      ( fromList [MEmail, MEmailValidated, MSCIM, MSAML], -- EmailSource can be any of the 2 cases
        []
      ),
      ( fromList [MEmail, MEmailValidated, MSAML], -- uref contains an email and validateSAMLEmail feature is activated
        []
      ),
      ( fromList [MEmail, MSCIM, MSAML],
        []
      ),
      ( fromList [MEmail, MSAML],
        []
      ),
      ( fromList [MSCIM, MSAML],
        []
      ),
      ( fromList [MSAML],
        []
      )
    ]

main :: IO ()
main = do
  putStrLn $ ppShow validStates
  putStrLn $ ppShow (validStates == (fst `Data.Set.map` validStatesExpected))

specs :: Spec
specs = do
  describe "AuthId markers" $ do
    it "validStates, validStatesExpected are in sync" $ do
      validStates `shouldBe` (fst `Data.Set.map` validStatesExpected)
    it "all validStateExpected entries have examples" $ do
      -- we can't really check anything more; that the examples actually represent the marker
      -- set lies in the responsibility of the test author.  (so this is something between
      -- compiler-validated documentation and an actual test.)
      forM_ (Data.Set.toList validStatesExpected) $ \(_, examples) -> do
        examples `shouldNotBe` mempty

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

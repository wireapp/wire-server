{-# LANGUAGE ScopedTypeVariables #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.OAuth where

import Data.Aeson
import Data.Set qualified as Set
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.OAuth

tests :: TestTree
tests =
  testGroup "Oauth" $
    [ testGroup "code challenge verification should succeed" $
        [ testCase "should" testCodeChallengeVerification
        ],
      testGroup "scopes" $
        [ testCase "only known scopes parse" testScopesParseOnlyKnown
        ]
    ]

-- | A scope nobody can be granted has to be an error.  If it were dropped, or
-- turned the whole set into no scopes at all, the client would get a token that
-- does not do what it asked for.
testScopesParseOnlyKnown :: Assertion
testScopesParseOnlyKnown = do
  (eitherDecode "\"read:self write-only:conversations\"" :: Either String OAuthScopes)
    @?= Right (OAuthScopes (Set.fromList [Self Read, Conversations WriteOnly]))
  for_
    [ "\"read:pizza\"", -- no such scope
      "\"write:conversations\"", -- deprecated tier
      "\"read:self read:pizza\"" -- one bad scope spoils the request
    ]
    $ \bad -> case eitherDecode bad :: Either String OAuthScopes of
      Left _ -> pure ()
      Right scopes ->
        assertFailure $ "expected a parse error for " <> show bad <> ", got " <> show scopes

testCodeChallengeVerification :: Assertion
testCodeChallengeVerification = do
  mkChallenge codeVerifier @?= codeChallenge
  where
    codeChallenge :: OAuthCodeChallenge
    codeChallenge = either (\e -> error $ "invalid code challenge " <> show e) id $ eitherDecode "\"G7CWLBqYDT8doT_oEIN3un_QwZWYKHmOqG91nwNzITc\""

    codeVerifier :: OAuthCodeVerifier
    codeVerifier = either (\e -> error $ "invalid code verifier " <> show e) id $ eitherDecode "\"nE3k3zykOmYki~kriKzAmeFiGT7cWugcuToFwo1YPgrZ1cFvaQqLa.dXY9MnDj3umAmG-8lSNIYIl31Cs_.fV5r2psa4WWZcB.Nlc3A-t3p67NDZaOJjIiH~8PvUH_hR\""

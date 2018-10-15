{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Test.Spar.SCIMSpec where

-- import Bilge
-- import Brig.Types.User
-- import Control.Monad.Reader
-- import Control.Retry
-- import Data.ByteString.Conversion
-- import Data.Id
-- import Data.List (isInfixOf)
-- import Data.Maybe
-- import Data.String.Conversions
-- import Data.UUID as UUID hiding (null, fromByteString)
-- import Data.UUID.V4 as UUID
-- import Galley.Types.Teams as Galley
-- import GHC.Stack
-- import Lens.Micro
-- import Prelude hiding (head)
-- import SAML2.WebSSO as SAML
-- import SAML2.WebSSO.Test.MockResponse
-- import Spar.Types
-- import Spar.API.Types
import Util

-- import qualified Data.Aeson as Aeson
-- import qualified Spar.Intra.Brig as Intra


spec :: SpecWith TestEnv
spec = do
    specUsers


specUsers :: SpecWith TestEnv
specUsers = describe "operations with users" $ do

    describe "POST /Users" $ do
        it "creates a user in an existing team" $ do
            pending
            -- create a user via SCIM
            -- check that this user is present in Brig

    describe "GET /Users" $ do
        it "lists all users in a team" $ do
            pending
            -- check that both normally created and SCIM-created user are listable

    describe "GET /Users/:userName" $ do
        it "finds the SCIM-provisioned user by username" $ do
            pending
            -- check that the SCIM-provisioned user is get-table
        it "finds the pre-existing user by username" $ do
            pending
            -- check that the pre-existing user is get-table
        it "doesn't find a user that's not part of the team" $ do
            pending
            -- create another team and another user in it
            -- check that this user can not be found in the "wrong" team


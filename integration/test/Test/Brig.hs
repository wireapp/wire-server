module Test.Brig where

import qualified API.Brig as Public
import qualified API.BrigInternal as Internal
import qualified API.Common as API
import qualified API.GalleyInternal as Internal
import GHC.Stack
import SetupHelpers
import Testlib.Prelude
import Wire.API.Routes.FederationDomainConfig
import qualified Data.Domain as D
import Wire.API.User.Search
import Data.String.Conversions
import Control.Monad.IO.Class
import TestLib.Assertions

testSearchContactForExternalUsers :: HasCallStack => App ()
testSearchContactForExternalUsers = do
  owner <- randomUser ownDomain def {Internal.team = True}
  partner <- randomUser ownDomain def {Internal.team = True}

  bindResponse (Internal.putTeamMember partner (partner %. "team") (API.teamRole "partner")) $ \resp ->
    resp.status `shouldMatchInt` 200

  bindResponse (Public.searchContacts partner (owner %. "name")) $ \resp ->
    resp.status `shouldMatchInt` 403

testCrudFederationRemotes :: HasCallStack => App ()
testCrudFederationRemotes = do
  let remote1 = FederationDomainConfig (D.Domain $ cs "good.example.com") NoSearch
      remote2 = FederationDomainConfig (D.Domain $ cs "evil.example.com") ExactHandleSearch
      remote2' = remote2 {cfgSearchPolicy = NoSearch}

      parseFedConns :: HasCallStack => Response -> App [FederationDomainConfig]
      parseFedConns = undefined

      shouldMatchFedConns :: HasCallStack => [FederationDomainConfig] -> [FederationDomainConfig] -> App ()
      shouldMatchFedConns _ _ = do
        liftIO $ shouldMatch _ _ --  "should return config values and good.example.com" (sort $ rem : cfgRemotes) (sort res2)
        undefined

      addOnce :: HasCallStack => FederationDomainConfig -> App ()
      addOnce rem = do
        createFedConn rem
        res <- parseFedConns =<< readFedConns
        res `shouldMatchFedConns` (sort $ rem : cfgRemotes)

      deleteOnce :: HasCallStack => Domain -> App ()
      deleteOnce = undefined

      deleteFail :: HasCallStack => Domain -> App ()
      deleteFail = undefined

      updateOnce :: HasCallStack => D.Domain -> FederationDomainConfig -> App ()
      updateOnce = undefined

      updateFail :: HasCallStack => D.Domain -> App ()
      updateFail = undefined

  -- Delete the remotes from the database
  -- This doesn't do anything with the remotes
  -- defined in config files.
  resetFedConns
  cfgRemotes <- parseFedConns =<< readFedConns
  cfgRemotes `shouldMatchFedConns` [remote2]
  deleteFail (domain $ head $ cfgRemotes)

  addOnce remote1
  readFedConns `shouldContainFedConns` remote1

  addOnce remote1 -- idempotency
  readFedConns `shouldContainFedConns` remote1

  deleteOnce (domain remote1)
  readFedConns `shouldNotContainFedConns` remote1

  deleteOnce (domain remote1) -- idempotency
  readFedConns `shouldNotContainFedConns` remote1

  addOnce remote2
  deleteFail (domain remote2) -- removing from cfg file doesn't work whether it's in the database or not
  readFedConns `shouldContainFedConns` remote2

  updateOnce (domain remote2) remote2'
  readFedConns `shouldNotContainFedConns` remote2
  readFedConns `shouldContainFedConns` remote2'

  updateOnce (domain remote2) remote2' -- idempotency
  readFedConns `shouldNotContainFedConns` remote2
  readFedConns `shouldContainFedConns` remote2'

{-

  -- updating search strategy works
  updateFederationRemote brig (domain remote2) remote2'
  res5 <- getFederationRemotes brig
  -- (move the dynamic remotes to the beginning here to make sure we look for `remote2'`, not `remote`.)
  liftIO $ assertEqual "should be NoSearch" (nub $ sort $ [remote1, remote2'] <> cfgRemotes) (sort res5)

  -- updating from config file fails
  updateFederationRemote' id brig (domain $ head $ cfgRemotes) (head $ cfgRemotes) !!! const 533 === statusCode

  -- updating domain fails
  let remote2'' = remote2' {domain = Domain "broken.example.com"}
  updateFederationRemote' id brig (domain remote2) remote2'' !!! const 533 === statusCode

  -- TODO: how do we test that the TVar is updated in all services?  some fancy unit test?
  -- duplicate internal end-point to all services, and implement the hanlers in a library?
  pure ()
  where

-}

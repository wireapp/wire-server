{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.Spar where

import qualified API.Brig as Brig
import API.BrigInternal as BrigInternal
import API.Common (defPassword, randomEmail, randomExternalId, randomHandle)
import API.GalleyInternal (setTeamFeatureStatus)
import API.Nginz (login)
import API.Spar
import Control.Concurrent (threadDelay)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Vector (fromList)
import qualified Data.Vector as Vector
import SAML2.WebSSO.Test.Util (SampleIdP (..), makeSampleIdPMetadata)
import SetupHelpers
import Testlib.JSON
import Testlib.PTest
import Testlib.Prelude
import qualified Text.XML.DSig as SAML

testSparUserCreationInvitationTimeout :: (HasCallStack) => App ()
testSparUserCreationInvitationTimeout = do
  (owner, _tid, _) <- createTeam OwnDomain 1
  tok <- createScimTokenV6 owner def >>= \resp -> resp.json %. "token" >>= asString
  scimUser <- randomScimUser
  bindResponse (createScimUser OwnDomain tok scimUser) $ \res -> do
    res.status `shouldMatchInt` 201

  -- Trying to create the same user again right away should fail
  bindResponse (createScimUser OwnDomain tok scimUser) $ \res -> do
    res.status `shouldMatchInt` 409

  -- However, if we wait until the invitation timeout has passed
  -- It's currently configured to 1s local/CI.
  liftIO $ threadDelay (2_000_000)

  -- ...we should be able to create the user again
  retryT $ bindResponse (createScimUser OwnDomain tok scimUser) $ \res -> do
    res.status `shouldMatchInt` 201

testSparExternalIdDifferentFromEmailWithIdp :: (HasCallStack) => App ()
testSparExternalIdDifferentFromEmailWithIdp = do
  (owner, tid, _) <- createTeam OwnDomain 1
  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  void $ registerTestIdPWithMeta owner >>= getJSON 201
  tok <- createScimTokenV6 owner def >>= getJSON 200 >>= (%. "token") >>= asString
  email <- randomEmail
  extId <- randomExternalId
  scimUser <- randomScimUserWith extId email
  userId <- createScimUser OwnDomain tok scimUser >>= getJSON 201 >>= (%. "id") >>= asString
  activateEmail OwnDomain email
  checkSparGetUserAndFindByExtId OwnDomain tok extId userId $ \u -> do
    u %. "externalId" `shouldMatch` extId
    (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` email
  bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
    res.status `shouldMatchInt` 200
    u <- res.json >>= asList >>= assertOne
    u %. "email" `shouldMatch` email
    subject <- u %. "sso_id.subject" >>= asString
    subject `shouldContainString` extId
    u %. "handle" `shouldMatch` (scimUser %. "userName")

  -- Verify that updating `userName` (handle) works
  scimUserWith1Update <- do
    newHandle <- randomHandle
    updatedScimUser <- setField "userName" newHandle scimUser
    bindResponse (updateScimUser OwnDomain tok userId updatedScimUser) $ \res -> do
      res.status `shouldMatchInt` 200
      res.json %. "userName" `shouldMatch` newHandle
    checkSparGetUserAndFindByExtId OwnDomain tok extId userId $ \u -> do
      u %. "externalId" `shouldMatch` extId
      (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` email
    bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
      res.status `shouldMatchInt` 200
      u <- res.json >>= asList >>= assertOne
      u %. "handle" `shouldMatch` newHandle
    pure updatedScimUser

  -- Verify that updating the user's external ID works
  scimUserWith2Updates <- do
    newExtId <- randomExternalId
    updatedScimUser <- setField "externalId" newExtId scimUserWith1Update
    bindResponse (updateScimUser OwnDomain tok userId updatedScimUser) $ \res -> do
      res.status `shouldMatchInt` 200
      res.json %. "externalId" `shouldMatch` newExtId
    checkSparGetUserAndFindByExtId OwnDomain tok newExtId userId $ \u -> do
      u %. "externalId" `shouldMatch` newExtId
      (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` email
    bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
      res.status `shouldMatchInt` 200
      u <- res.json >>= asList >>= assertOne
      u %. "email" `shouldMatch` email
      subject <- u %. "sso_id.subject" >>= asString
      subject `shouldContainString` newExtId
    bindResponse (findUsersByExternalId OwnDomain tok extId) $ \res -> do
      res.json %. "totalResults" `shouldMatchInt` 0
      res.json %. "Resources" `shouldMatch` ([] :: [Value])
    pure updatedScimUser

  -- Verify that updating the user's email works
  do
    let oldEmail = email
    newEmail <- randomEmail
    updatedScimUser <- setField "emails" (Array (Vector.fromList [object ["value" .= newEmail]])) scimUserWith2Updates
    currentExtId <- updatedScimUser %. "externalId" >>= asString
    bindResponse (updateScimUser OwnDomain tok userId updatedScimUser) $ \res -> do
      res.status `shouldMatchInt` 200

    -- before activation the old email should still be present
    checkSparGetUserAndFindByExtId OwnDomain tok currentExtId userId $ \u -> do
      u %. "externalId" `shouldMatch` currentExtId
      (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` newEmail
    bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
      res.status `shouldMatchInt` 200
      u <- res.json >>= asList >>= assertOne
      u %. "email" `shouldMatch` oldEmail
      subject <- u %. "sso_id.subject" >>= asString
      subject `shouldContainString` currentExtId

    -- after activation the new email should be present
    activateEmail OwnDomain newEmail
    checkSparGetUserAndFindByExtId OwnDomain tok currentExtId userId $ \u -> do
      u %. "externalId" `shouldMatch` currentExtId
      (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` newEmail
    bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
      res.status `shouldMatchInt` 200
      u <- res.json >>= asList >>= assertOne
      u %. "email" `shouldMatch` newEmail
      subject <- u %. "sso_id.subject" >>= asString
      subject `shouldContainString` currentExtId

registerTestIdPWithMeta :: (HasCallStack, MakesValue owner) => owner -> App Response
registerTestIdPWithMeta owner = fst <$> registerTestIdPWithMetaWithPrivateCreds owner

registerTestIdPWithMetaWithPrivateCreds :: (HasCallStack, MakesValue owner) => owner -> App (Response, SAML.SignPrivCreds)
registerTestIdPWithMetaWithPrivateCreds owner = do
  SampleIdP idpmeta pCreds _ _ <- makeSampleIdPMetadata
  (,pCreds) <$> createIdp owner idpmeta

testSparExternalIdDifferentFromEmail :: (HasCallStack) => App ()
testSparExternalIdDifferentFromEmail = do
  (owner, tid, _) <- createTeam OwnDomain 1
  tok <- createScimTokenV6 owner def >>= \resp -> resp.json %. "token" >>= asString
  email <- randomEmail
  extId <- randomExternalId
  scimUser <- randomScimUserWith extId email
  userId <- createScimUser OwnDomain tok scimUser >>= getJSON 201 >>= (%. "id") >>= asString

  checkSparGetUserAndFindByExtId OwnDomain tok extId userId $ \u -> do
    u %. "externalId" `shouldMatch` extId
    (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` email
  bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
    res.status `shouldMatchInt` 200
    res.json >>= asList >>= shouldBeEmpty

  registerUser OwnDomain tid email

  checkSparGetUserAndFindByExtId OwnDomain tok extId userId $ \u -> do
    u %. "externalId" `shouldMatch` extId
    (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` email
  bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
    res.status `shouldMatchInt` 200
    u <- res.json >>= asList >>= assertOne
    u %. "email" `shouldMatch` email
    u %. "sso_id.scim_external_id" `shouldMatch` extId
    u %. "handle" `shouldMatch` (scimUser %. "userName")

  -- Verify that updating the scim user works
  scimUserWith1Update <- do
    -- FUTUREWORK: test updating other fields besides handle as well
    newHandle <- randomHandle
    updatedScimUser <- setField "userName" newHandle scimUser
    bindResponse (updateScimUser OwnDomain tok userId updatedScimUser) $ \res -> do
      res.status `shouldMatchInt` 200
      res.json %. "userName" `shouldMatch` newHandle
    checkSparGetUserAndFindByExtId OwnDomain tok extId userId $ \u -> do
      u %. "externalId" `shouldMatch` extId
      (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` email
    bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
      res.status `shouldMatchInt` 200
      u <- res.json >>= asList >>= assertOne
      u %. "handle" `shouldMatch` newHandle
    pure updatedScimUser

  -- Verify that updating the user's external ID works
  scimUserWith2Updates <- do
    newExtId <- randomExternalId
    updatedScimUser <- setField "externalId" newExtId scimUserWith1Update
    bindResponse (updateScimUser OwnDomain tok userId updatedScimUser) $ \res -> do
      res.status `shouldMatchInt` 200
      res.json %. "externalId" `shouldMatch` newExtId
    checkSparGetUserAndFindByExtId OwnDomain tok newExtId userId $ \u -> do
      u %. "externalId" `shouldMatch` newExtId
      (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` email
    bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
      res.status `shouldMatchInt` 200
      u <- res.json >>= asList >>= assertOne
      u %. "email" `shouldMatch` email
      u %. "sso_id.scim_external_id" `shouldMatch` newExtId
    bindResponse (findUsersByExternalId OwnDomain tok extId) $ \res -> do
      res.json %. "totalResults" `shouldMatchInt` 0
      res.json %. "Resources" `shouldMatch` ([] :: [Value])
    pure updatedScimUser

  -- Verify that updating the user's email works
  do
    let oldEmail = email
    newEmail <- randomEmail
    updatedScimUser <- setField "emails" (Array (Vector.fromList [object ["value" .= newEmail]])) scimUserWith2Updates
    currentExtId <- updatedScimUser %. "externalId" >>= asString
    bindResponse (updateScimUser OwnDomain tok userId updatedScimUser) $ \res -> do
      res.status `shouldMatchInt` 200

    -- before activation the new email should be returned by the SCIM API
    checkSparGetUserAndFindByExtId OwnDomain tok currentExtId userId $ \u -> do
      u %. "externalId" `shouldMatch` currentExtId
      (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` newEmail
    -- however brig should still return the old email
    bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
      res.status `shouldMatchInt` 200
      u <- res.json >>= asList >>= assertOne
      u %. "email" `shouldMatch` oldEmail
      u %. "sso_id.scim_external_id" `shouldMatch` currentExtId

    -- after activation the new email should be present
    activateEmail OwnDomain newEmail
    checkSparGetUserAndFindByExtId OwnDomain tok currentExtId userId $ \u -> do
      u %. "externalId" `shouldMatch` currentExtId
      (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` newEmail
    bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
      res.status `shouldMatchInt` 200
      u <- res.json >>= asList >>= assertOne
      u %. "email" `shouldMatch` newEmail
      u %. "sso_id.scim_external_id" `shouldMatch` currentExtId

testSparExternalIdUpdateToANonEmail :: (HasCallStack) => App ()
testSparExternalIdUpdateToANonEmail = do
  (owner, tid, _) <- createTeam OwnDomain 1
  tok <- createScimTokenV6 owner def >>= \resp -> resp.json %. "token" >>= asString
  scimUser <- randomScimUser >>= removeField "emails"
  email <- scimUser %. "externalId" >>= asString
  userId <- bindResponse (createScimUser OwnDomain tok scimUser) $ \resp -> do
    resp.status `shouldMatchInt` 201
    (resp.json %. "emails" >>= asList >>= assertOne >>= (%. "value") >>= asString) `shouldMatch` email
    resp.json %. "id" >>= asString
  registerUser OwnDomain tid email

  let extId = "notanemailaddress"
  updatedScimUser <- setField "externalId" extId scimUser
  updateScimUser OwnDomain tok userId updatedScimUser >>= assertStatus 400

testSparMigrateFromExternalIdOnlyToEmail :: (HasCallStack) => Tagged "mailUnchanged" Bool -> App ()
testSparMigrateFromExternalIdOnlyToEmail (MkTagged emailUnchanged) = do
  (owner, tid, _) <- createTeam OwnDomain 1
  tok <- createScimTokenV6 owner def >>= \resp -> resp.json %. "token" >>= asString
  scimUser <- randomScimUser >>= removeField "emails"
  email <- scimUser %. "externalId" >>= asString
  userId <- createScimUser OwnDomain tok scimUser >>= getJSON 201 >>= (%. "id") >>= asString
  registerUser OwnDomain tid email

  -- Verify that updating a user with an empty emails does not change the email
  bindResponse (updateScimUser OwnDomain tok userId scimUser) $ \resp -> do
    resp.json %. "emails" `shouldMatch` (Array (fromList [object ["value" .= email]]))
    resp.status `shouldMatchInt` 200

  newEmail <- if emailUnchanged then pure email else randomEmail
  let newEmails = (Array (fromList [object ["value" .= newEmail]]))
  updatedScimUser <- setField "emails" newEmails scimUser
  updateScimUser OwnDomain tok userId updatedScimUser `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "externalId" `shouldMatch` (updatedScimUser %. "externalId")
    resp.json %. "emails" `shouldMatch` (updatedScimUser %. "emails")

  -- after activation the new email should be present
  unless emailUnchanged $ activateEmail OwnDomain newEmail

  extId <- scimUser %. "externalId" >>= asString
  checkSparGetUserAndFindByExtId OwnDomain tok extId userId $ \u -> do
    u %. "externalId" `shouldMatch` extId
    (u %. "emails" >>= asList >>= assertOne >>= (%. "value")) `shouldMatch` newEmail
  bindResponse (getUsersId OwnDomain [userId]) $ \res -> do
    res.status `shouldMatchInt` 200
    u <- res.json >>= asList >>= assertOne
    u %. "email" `shouldMatch` newEmail
    u %. "sso_id.scim_external_id" `shouldMatch` extId

registerUser :: (HasCallStack, MakesValue domain) => domain -> String -> String -> App ()
registerUser domain tid email = do
  BrigInternal.getInvitationByEmail domain email
    >>= getJSON 200
    >>= BrigInternal.getInvitationCodeForTeam domain tid
    >>= getJSON 200
    >>= (%. "code")
    >>= asString
    >>= Brig.registerUser domain email
    >>= assertSuccess

activateEmail :: (HasCallStack, MakesValue domain) => domain -> String -> App ()
activateEmail domain email = do
  (actkey, code) <- bindResponse (BrigInternal.getActivationCode domain email) $ \res -> do
    (,)
      <$> (res.json %. "key" >>= asString)
      <*> (res.json %. "code" >>= asString)
  Brig.activate domain actkey code >>= assertSuccess

checkSparGetUserAndFindByExtId :: (HasCallStack, MakesValue domain) => domain -> String -> String -> String -> (Value -> App ()) -> App ()
checkSparGetUserAndFindByExtId domain tok extId uid k = do
  usersByExtIdResp <- findUsersByExternalId domain tok extId
  usersByExtIdResp.status `shouldMatchInt` 200
  userByIdExtId <- usersByExtIdResp.json %. "Resources" >>= asList >>= assertOne
  k userByIdExtId

  userByUidResp <- getScimUser domain tok uid
  userByUidResp.status `shouldMatchInt` 200
  userByUid <- userByUidResp.json
  k userByUid

  userByUid `shouldMatch` userByIdExtId

testSparCreateScimTokenNoName :: (HasCallStack) => App ()
testSparCreateScimTokenNoName = do
  (owner, _tid, mem : _) <- createTeam OwnDomain 2
  createScimTokenV6 owner def >>= assertSuccess
  createScimTokenV6 owner def >>= assertSuccess
  tokens <- bindResponse (getScimTokens owner) $ \resp -> do
    resp.status `shouldMatchInt` 200
    tokens <- resp.json %. "tokens" >>= asList
    for_ tokens $ \token -> do
      token %. "name" `shouldMatch` (token %. "id")
    pure tokens
  for_ tokens $ \token -> do
    tokenId <- token %. "id" >>= asString
    putScimTokenName mem tokenId "new name" >>= assertStatus 403
    putScimTokenName owner tokenId ("token:" <> tokenId) >>= assertSuccess
  bindResponse (getScimTokens owner) $ \resp -> do
    resp.status `shouldMatchInt` 200
    updatedTokens <- resp.json %. "tokens" >>= asList
    for_ updatedTokens $ \token -> do
      tokenId <- token %. "id" >>= asString
      token %. "name" `shouldMatch` ("token:" <> tokenId)

testSparCreateScimTokenWithName :: (HasCallStack) => App ()
testSparCreateScimTokenWithName = do
  (owner, _tid, _) <- createTeam OwnDomain 1
  let expected = "my scim token"
  createScimTokenV6 owner (def {name = Just expected}) >>= assertSuccess
  tokens <- getScimTokens owner >>= getJSON 200 >>= (%. "tokens") >>= asList
  for_ tokens $ \token -> do
    token %. "name" `shouldMatch` expected

testCreateMultipleIdps :: (HasCallStack) => App ()
testCreateMultipleIdps = do
  (owner, tid, _) <- createTeam OwnDomain 1
  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  idp1 <- registerTestIdPWithMeta owner >>= getJSON 201 >>= (%. "id") >>= asString
  idp2 <- registerTestIdPWithMeta owner >>= getJSON 201 >>= (%. "id") >>= asString
  createScimToken owner (def {name = Just "foobar", idp = Just idp1}) >>= assertSuccess
  createScimToken owner (def {name = Just "bazqux", idp = Just idp2}) >>= assertSuccess

-- | Create a few saml IdPs and a few scim peers.  Randomize the order in which they are
-- created, and which peers / IdPs they are associated with.
testCreateIdpsAndScimsV7 :: (HasCallStack) => App ()
testCreateIdpsAndScimsV7 = do
  runSteps
    [ MkSaml "saml1" ExpectSuccess
    ]
  runSteps
    [ MkScim "scim1" Nothing ExpectSuccess,
      MkSaml "saml1" ExpectSuccess,
      MkSaml "saml2" ExpectSuccess,
      MkScim "scim2" (Just "saml1") ExpectSuccess,
      -- two scims can be associated with one idp
      MkScim "scim3" (Just "saml1") ExpectSuccess,
      MkScim "scim4" (Just "saml2") ExpectSuccess,
      MkScim "scim5" Nothing ExpectSuccess
    ]
  -- two saml idps cannot associate with the same scim peer: it would be unclear which idp the
  -- next user is supposed to be provisioned for.  (not need to test, because it cannot be
  -- expressed in the API.)
  runSteps
    [ MkSaml "saml1" ExpectSuccess,
      MkScim "scim1" (Just "saml1") ExpectSuccess,
      RmScim "scim1",
      MkScim "scim2" (Just "saml1") ExpectSuccess
    ]

-- | DSL with relevant api calls (not test cases).  This should make writing down different
-- test cases very concise and not cost any generality.
data Step samlRef scimRef
  = MkScim scimRef (Maybe samlRef) ExpectedResult
  | -- | `RmScim` has expected result: delete is idempotent.
    RmScim scimRef
  | -- | you can't associate a saml idp with a existing scim peer when creating the idp.
    -- do that by replacing the scim token and associating the new one during creation.
    MkSaml samlRef ExpectedResult
  deriving (Show)

type StringStep = Step String String

data ExpectedResult = ExpectSuccess | ExpectFailure Int String
  deriving (Eq, Show, Generic)

data State samlRef scimRef = State
  { allIdps :: Map samlRef (String {- id -}, SAML.SignPrivCreds),
    allScims :: Map scimRef (String {- id -}, String {- bearer token -}),
    allScimAssocs :: Map String String
  }

type StringState = State String String

emptyState :: StringState
emptyState = State mempty mempty mempty

runSteps :: (HasCallStack) => [StringStep] -> App ()
runSteps steps = do
  (owner, tid, []) <- createTeam OwnDomain 1
  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  go owner tid emptyState steps
  where
    go :: Value -> String -> StringState -> [StringStep] -> App ()
    go _ _ _ [] = pure ()
    -- add scim
    go owner tid state (next@(MkScim scimRef mbSamlRef expected) : steps') = addFailureContext (show next) do
      let mIdPId = ((fst <$> state.allIdps) !) <$> mbSamlRef
      let p = def {name = Just scimRef, idp = mIdPId}
      state' <- bindResponse (createScimToken owner p) $ \resp -> do
        case expected of
          ExpectSuccess -> validateScimRegistration state scimRef mIdPId resp
          ExpectFailure errStatus errLabel -> validateError resp errStatus errLabel $> state
      validateState owner tid state'
      go owner tid state' steps'
    -- add saml
    go owner tid state (next@(MkSaml samlRef expected) : steps') = addFailureContext (show next) do
      state' <- do
        (resp, creds) <- registerTestIdPWithMetaWithPrivateCreds owner
        case expected of
          ExpectSuccess -> validateSamlRegistration state samlRef resp creds
          ExpectFailure errStatus errLabel -> validateError resp errStatus errLabel $> state
      validateState owner tid state'
      go owner tid state' steps'
    -- remove scim
    go owner tid state (next@(RmScim scimRef) : steps') = addFailureContext (show next) do
      let (tokenId, _) = state.allScims Map.! scimRef
      state' <- bindResponse (deleteScimToken owner tokenId) $ \resp -> do
        resp.status `shouldMatchInt` 204
        pure
          $ state
            { allScims = Map.delete scimRef (allScims state),
              allScimAssocs = Map.delete tokenId (allScimAssocs state)
            }
      validateState owner tid state'
      go owner tid state' steps'

    validateScimRegistration :: StringState -> String -> Maybe String -> Response -> App StringState
    validateScimRegistration state scimRef mIdPId resp = do
      resp.status `shouldMatchInt` 200
      scimId <- resp.json %. "info.id" >>= asString
      tok <- resp.json %. "token" >>= asString
      pure
        $ state
          { allScims = Map.insert scimRef (scimId, tok) (allScims state),
            allScimAssocs = maybe id (Map.insert scimId) mIdPId $ allScimAssocs state
          }

    validateSamlRegistration :: StringState -> String -> Response -> SAML.SignPrivCreds -> App StringState
    validateSamlRegistration state samlRef resp creds = do
      resp.status `shouldMatchInt` 201
      samlId <- resp.json %. "id" >>= asString
      pure $ state {allIdps = Map.insert samlRef (samlId, creds) (allIdps state)}

    validateState :: Value -> String -> StringState -> App ()
    validateState owner tid state = do
      allIdps <- getIdps owner >>= getJSON 200 >>= (%. "providers") >>= asList
      allScims <- getScimTokens owner >>= getJSON 200 >>= (%. "tokens") >>= asList

      do
        -- are all idps from spar in the local test state and vice versa?
        let allLocal = Map.elems state.allIdps
        allSpar <- ((%. "id") >=> asString) `traverse` allIdps
        (fst <$> allLocal) `shouldMatchSet` allSpar

      do
        -- are all scim peers from spar in the local test state and vice versa?
        let allLocal = fst <$> Map.elems state.allScims
        allSpar <- (%. "id") `traverse` allScims
        allLocal `shouldMatchSet` allSpar

      do
        -- are all local associations the same as on spar?
        let toScimIdpPair tokInfo = do
              mIdp <- lookupField tokInfo "idp"
              case mIdp of
                Just idp -> Just <$> ((,) <$> (tokInfo %. "id" >>= asString) <*> asString idp)
                Nothing -> pure Nothing

        sparState <- Map.fromList . catMaybes <$> (toScimIdpPair `mapM` allScims)
        sparState `shouldMatch` state.allScimAssocs

      do
        -- login.
        -- (auto-provisioning with saml without scim is intentionally not tested.)
        for_ (Map.elems state.allScims) $ \(scimId, tok) -> do
          let mIdp = (state.allIdps !) <$> Map.lookup scimId state.allScimAssocs

          scimUser <- randomScimUser
          email <- scimUser %. "externalId" >>= asString
          uid <- bindResponse (createScimUser owner tok scimUser) $ \resp -> do
            resp.status `shouldMatchInt` 201
            resp.json %. "id" >>= asString
          when (isNothing mIdp) $ do
            registerUser OwnDomain tid email

          maybe (loginWithPassword 200 scimUser) (loginWithSaml True owner tid scimUser) mIdp

          bindResponse (deleteScimUser owner tok uid) $ \resp -> do
            resp.status `shouldMatchInt` 204

          maybe (loginWithPassword 403 scimUser) (loginWithSaml False owner tid scimUser) mIdp

    validateError :: Response -> Int -> String -> App ()
    validateError resp errStatus errLabel = do
      do
        resp.status `shouldMatchInt` errStatus
        resp.json %. "code" `shouldMatchInt` errStatus
        resp.json %. "label" `shouldMatch` errLabel

loginWithSaml :: (HasCallStack) => Bool -> Value -> String -> Value -> (String, SAML.SignPrivCreds) -> App ()
loginWithSaml expectSuccess owner tid scimUser idpId = do
  let apiVer = "v2"
  {-
    (idp, (_, privcreds)) <- registerTestIdPWithMeta owner
    liftIO $ fromMaybe defWireIdPAPIVersion (idp ^. idpExtraInfo . apiVersion) `shouldBe` apiVer
    spmeta <- getTestSPMetadata tid
    authnreq <- negotiateAuthnRequest idp
    let audiencePath = case apiVer of
          WireIdPAPIV1 -> "/sso/finalize-login"
          WireIdPAPIV2 -> "/sso/finalize-login/" <> toByteString' tid
    liftIO $ authnreq ^. rqIssuer . fromIssuer . to URI.uriPath `shouldBe` audiencePath
    authnresp <- runSimpleSP $ mkAuthnResponse privcreds idp spmeta authnreq True
    loginSuccess =<< submitAuthnResponse tid authnresp
  -}
  pure ()

loginWithPassword :: (HasCallStack) => Int -> Value -> App ()
loginWithPassword expectedStatus scimUser = do
  email <- scimUser %. "emails" >>= asList >>= assertOne >>= (%. "value") >>= asString
  bindResponse (login OwnDomain email defPassword) $ \resp -> do
    resp.status `shouldMatchInt` expectedStatus

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.SAML2.WebSSO.SPSpec
  ( spec,
  )
where

import Control.Concurrent.MVar
import Control.Lens
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map qualified as Map
import SAML2.WebSSO
import SAML2.WebSSO.API.Example (AssertionStore)
import SAML2.WebSSO.Test.Lenses
import SAML2.WebSSO.Test.Util
import Test.Hspec
import URI.ByteString.QQ

instance HasConfig IO where
  getConfig = configIO

instance HasLogger IO

instance HasCreateUUID IO

instance HasNow IO

spec :: Spec
spec = describe "SP" $ do
  describe "just making sure..." $ do
    describe "instance Ord Time works" $ do
      it "ago <  now" $ (timeLongAgo < timeNow) `shouldBe` True
      it "ago <= now" $ (timeLongAgo <= timeNow) `shouldBe` True
      it "now <= now" $ (timeNow <= timeNow) `shouldBe` True
  specStoreAssertion
  specCreateAuthnRequest
  specJudgeT

specStoreAssertion :: Spec
specStoreAssertion = describe "storeAssertion" . before mkTestCtxSimple $ do
  let peek :: CtxV -> IO AssertionStore
      peek ctx = ((^. ctxAssertionStore) <$> readMVar ctx)
  context "id is new" $ do
    it "stores id and returns True" $ \ctx -> do
      ioFromTestSP ctx (storeAssertion (mkID "phoo") timeIn10minutes)
        `shouldReturn` True
      peek ctx
        `shouldReturn` Map.fromList [(mkID "phoo", timeIn10minutes)]
  context "id is already in the map, but life time is exceeded" $ do
    it "stores id and returns True" $ \ctx -> do
      _ <- ioFromTestSP ctx $ storeAssertion (mkID "phoo") timeLongAgo -- warmup
      ioFromTestSP ctx (storeAssertion (mkID "phoo") timeIn10minutes)
        `shouldReturn` True
      peek ctx
        `shouldReturn` Map.fromList [(mkID "phoo", timeIn10minutes)]
  context "id is already in the map and still alive" $ do
    it "keeps map unchanged and returns False" $ \ctx -> do
      _ <- ioFromTestSP ctx $ storeAssertion (mkID "phoo") timeIn20minutes -- warmup
      bef <- peek ctx
      ioFromTestSP ctx (storeAssertion (mkID "phoo") timeIn10minutes)
        `shouldReturn` False
      aft <- peek ctx
      bef
        `shouldBe` aft

specCreateAuthnRequest :: Spec
specCreateAuthnRequest = do
  describe "createAuthnRequest" $ do
    it "works" $ do
      let reqid :: ID AuthnRequest
          reqid = mkID "66aaea58-db59-11e8-ba03-2bb52c9e2973"
          idpIssuer = Issuer [uri|https://sp.net/|]
      ctxv <- mkTestCtxSimple
      modifyMVar_ ctxv $ \ctx -> pure $ ctx & ctxRequestStore .~ Map.singleton reqid (idpIssuer, timeIn10minutes)
      (req, mbIdpIssuerFromReq) <- ioFromTestSP ctxv $ do
        req <- createAuthnRequest 30 (Issuer [uri|https://sp.net/|]) (Issuer [uri|https://idp.net/|])
        (req,) <$> getIdpIssuer (req ^. rqID)
      mbIdpIssuerFromReq `shouldBe` Just (Issuer [uri|https://idp.net/|])
      req ^. rqIssueInstant `shouldBe` timeNow
      req ^. rqIssuer `shouldBe` Issuer [uri|https://sp.net/|]

specJudgeT :: Spec
specJudgeT = do
  describe "JudgeT" $ do
    let emptyUserID = UserRef (Issuer [uri|http://example.com/|]) (unspecifiedNameID "me")
    it "no msgs" $ do
      verdict <- runJudgeT undefined $ pure $ AccessGranted (UserRef (Issuer [uri|http://example.com/|]) (unspecifiedNameID "me"))
      verdict `shouldBe` AccessGranted emptyUserID
    it "1 msg" $ do
      verdict <- runJudgeT undefined $ do
        deny DeniedStatusFailure
        pure $ AccessGranted emptyUserID
      verdict `shouldBe` AccessDenied [DeniedStatusFailure]
    it "2 msg" $ do
      verdict <- runJudgeT undefined $ do
        deny DeniedStatusFailure
        deny DeniedNoAuthnStatement
        pure $ AccessGranted emptyUserID
      verdict `shouldBe` AccessDenied [DeniedStatusFailure, DeniedNoAuthnStatement]
    it "1 msg, then giveup, then send another message" $ do
      verdict <- runJudgeT undefined $ do
        deny DeniedStatusFailure
        () <- giveup DeniedNoStatements
        deny DeniedNoAuthnStatement
        pure $ AccessGranted emptyUserID
      verdict `shouldBe` AccessDenied [DeniedNoStatements, DeniedStatusFailure]
  describe "judge" $ do
    let -- request issued at timeNow; response issued at timeIn5seconds; judge executed at timeIn10seconds

        grants :: (HasCallStack) => (Ctx -> Ctx) -> AuthnResponse -> Spec
        grants updctx aresp = do
          it "grants" $ do
            ctxv <- mkTestCtxSimple
            modifyMVar_ ctxv $ \ctx ->
              pure $
                ctx
                  & ctxNow .~ timeIn10seconds
                  & ctxRequestStore .~ Map.singleton reqid (aresp ^. assertionL . assIssuer, timeIn10minutes)
                  & updctx
            (`shouldSatisfy` has _AccessGranted) =<< ioFromTestSP ctxv (judge (aresp ^. rspPayload) StatusSuccess jctx)
        denies :: (HasCallStack) => (Ctx -> Ctx) -> AuthnResponse -> Spec
        denies updctx aresp = do
          it "denies" $ do
            ctxv <- mkTestCtxSimple
            modifyMVar_ ctxv $ \ctx ->
              pure $
                ctx
                  & ctxNow .~ timeIn10seconds
                  & ctxRequestStore .~ Map.singleton reqid (aresp ^. assertionL . assIssuer, timeIn10minutes)
                  & updctx
            (`shouldSatisfy` has _AccessDenied) =<< ioFromTestSP ctxv (judge (aresp ^. rspPayload) StatusSuccess jctx)
        jctx :: JudgeCtx
        jctx = JudgeCtx (Issuer [uri|https://sp.net/sso/authnresp|]) [uri|https://sp.net/sso/authnresp|]
        authnresp :: AuthnResponse
        authnresp =
          Response
            { _rspID = respid,
              _rspInRespTo = Just reqid,
              _rspIssueInstant = timeIn5seconds,
              _rspDestination = Just [uri|https://sp.net/sso/authnresp|],
              _rspIssuer = Just $ assertion ^. assIssuer,
              _rspStatus = StatusSuccess,
              _rspPayload = assertion :| []
            }
        respid :: ID AuthnResponse
        respid = mkID "49afd274-db59-11e8-b0be-e3130e26594d"
        reqid :: ID AuthnRequest
        reqid = mkID "66aaea58-db59-11e8-ba03-2bb52c9e2973"
        assertion :: Assertion
        assertion =
          Assertion
            { _assID = mkID "00c224c0-db5b-11e8-ba50-5b03ff78040f",
              _assIssueInstant = authnresp ^. rspIssueInstant,
              _assIssuer = Issuer [uri|https://idp.net/sso/login/480748de-db5a-11e8-b20f-031d2337a741|],
              _assConditions =
                Just
                  Conditions
                    { _condNotBefore = Just $ authnresp ^. rspIssueInstant,
                      _condNotOnOrAfter = Just timeIn20minutes,
                      _condOneTimeUse = False,
                      _condAudienceRestriction = [[uri|https://sp.net/sso/authnresp|] :| []]
                    },
              _assContents = SubjectAndStatements subject (statement :| [])
            }
        subject :: Subject
        subject =
          Subject
            { _subjectID = unspecifiedNameID "adac78fc-db5b-11e8-98d4-fbb67527aa01",
              _subjectConfirmations = [subjectconf]
            }
        subjectconf :: SubjectConfirmation
        subjectconf =
          SubjectConfirmation
            { _scMethod = SubjectConfirmationMethodBearer,
              _scData =
                Just
                  SubjectConfirmationData
                    { _scdNotBefore = Nothing,
                      _scdNotOnOrAfter = timeIn20minutes,
                      _scdRecipient = [uri|https://sp.net/sso/authnresp|],
                      _scdInResponseTo = Just reqid,
                      _scdAddress = Nothing
                    }
            }
        statement :: Statement
        statement =
          AuthnStatement
            { _astAuthnInstant = authnresp ^. rspIssueInstant,
              _astSessionIndex = Nothing,
              _astSessionNotOnOrAfter = Nothing,
              _astSubjectLocality = Nothing
            }
    context "vanilla response, matching request found" $ do
      --   grants id authnresp
      context "no matching request" $ do
        let updctx = ctxRequestStore .~ mempty
        denies updctx authnresp
      context "global inResponseTo missing" $ do
        grants id $ authnresp & rspInRespTo .~ Nothing
      context "inResponseTo in subject confirmation missing" $ do
        denies id $ authnresp & scdataL . scdInResponseTo .~ Nothing
      context "mismatch between global and subject confirmation inResponseTo" $ do
        -- wire does not parse unsigned data from the authentication response, so this will
        -- pass (but the mandatory inResponseTo field in the *signed* assertion will be
        -- considered).
        grants id $
          authnresp
            & rspInRespTo ?~ (mkID "89f926a4-dc4a-11e8-a44d-ab6b5be7205f")
        denies id $
          authnresp
            & scdataL . scdInResponseTo ?~ (mkID "89f926a4-dc4a-11e8-a44d-ab6b5be7205f")
      context "issue instant in the future" $ do
        let violations :: [AuthnResponse -> AuthnResponse]
            violations =
              [ assertionL . assIssueInstant .~ timeInALongTime,
                statementL . astAuthnInstant .~ timeInALongTime
              ]
        let meh :: [AuthnResponse -> AuthnResponse]
            meh =
              [ rspIssueInstant .~ timeInALongTime
              ]
        denies id `mapM_` (($ authnresp) <$> violations)
        -- wire does not test unsigned data in the authentication response, so issue instant
        -- will be ignored
        grants id `mapM_` (($ authnresp) <$> meh)
      context "SSO URL, recipient URL, destination URL, audience" $ do
        -- wire does not test unsigned data in the authentication response, so response
        -- destination will be ignored (in favor of the redundant info in the signed
        -- assertion)
        let good :: [AuthnResponse -> AuthnResponse]
            good =
              [ conditionsL . condAudienceRestriction
                  .~ [ {- (inner "or" succeeding) -} ([uri|https://other.io/sso|] :| [[uri|https://sp.net/sso/authnresp|]])
                     ],
                conditionsL . condAudienceRestriction
                  .~ [ {- (outer "and" succeeding) -} [uri|https://other.io/sso|] :| [[uri|https://sp.net/sso/authnresp|]],
                       [uri|https://sp.net/sso/authnresp|] :| []
                     ]
              ]
            bad :: [AuthnResponse -> AuthnResponse]
            bad =
              -- wire does not test unsigned data in the authentication response, so response
              -- destination will be ignored (in favor of the redundant info in the signed
              [ conditionsL . condAudienceRestriction .~ [[uri|https://other.io/sso|] :| []],
                scdataL . scdRecipient .~ [uri|https://other.io/sso|],
                conditionsL . condAudienceRestriction .~ [],
                -- "The resulting assertion(s) MUST contain a <saml:AudienceRestriction> element
                -- referencing the requester as an acceptable relying party." [1/3.4.1.4]
                conditionsL . condAudienceRestriction
                  .~ [ {- (inner "or" failing) -} [uri|https://other.io/sso|] :| [[uri|https://yetanother.net/stillwrong|]]
                     ],
                conditionsL . condAudienceRestriction
                  .~ [ {- (outer "and" failing) -} [uri|https://other.io/sso|] :| [[uri|https://sp.net/sso/authnresp|]],
                       [uri|https://yetanother.net/stillwrong|] :| []
                     ]
              ]
        grants id `mapM_` (($ authnresp) <$> good)
        denies id `mapM_` (($ authnresp) <$> bad)
      context "status failure" $ do
        -- wire does not test unsigned data in the authentication response, so the global
        -- status will be ignored in favor of the more fine-grained data in the assertion(s).
        grants id (authnresp & rspStatus .~ StatusFailure)
      context "time constraint violation" $ do
        let violations :: [AuthnResponse -> AuthnResponse]
            violations =
              [ conditionsL . condNotBefore ?~ timeInALongTime,
                conditionsL . condNotOnOrAfter ?~ timeLongAgo,
                scdataL . scdNotBefore ?~ timeInALongTime,
                scdataL . scdNotOnOrAfter .~ timeLongAgo
              ]
        denies id `mapM_` (($ authnresp) <$> violations)
      context "time constraint violation within tolerance" $ do
        let okviolations :: [AuthnResponse -> AuthnResponse]
            okviolations =
              [ conditionsL . condNotBefore ?~ (addTime (1 - tolerance) timeNow),
                conditionsL . condNotOnOrAfter ?~ (addTime (tolerance - 1) timeNow),
                scdataL . scdNotBefore ?~ (addTime (1 - tolerance) timeNow),
                scdataL . scdNotOnOrAfter .~ (addTime (tolerance - 1) timeNow)
              ]
        grants id `mapM_` (($ authnresp) <$> okviolations)
      context "response, assertion issuer" $ do
        let good :: [AuthnResponse -> AuthnResponse]
            good =
              [ rspIssuer .~ Nothing
              ]
            bad :: [AuthnResponse -> AuthnResponse]
            bad =
              [ rspIssuer ?~ (Issuer [uri|http://other.io/sso|]),
                assertionL . assIssuer .~ Issuer [uri|http://other.io/sso|]
              ]
        grants id `mapM_` (($ authnresp) <$> good)
        -- wire does not test unsigned data in the authentication response, so a mismatch
        -- between that and the assertion will be ignored.
        grants id `mapM_` (($ authnresp) <$> bad)

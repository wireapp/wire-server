{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}

module Test.SAML2.WebSSO.RoundtripSpec
  ( spec,
  )
where

import Control.Lens
import Data.Aeson as Aeson
import Data.List (sort)
import qualified Data.List.NonEmpty as NL
import Data.Proxy
import Hedgehog
import Hedgehog.Gen as Gen
import qualified SAML2.Core as HS
import SAML2.WebSSO
import SAML2.WebSSO.Test.Arbitrary
import SAML2.WebSSO.Test.Util
import Servant
import Test.Hspec

spec :: Spec
spec = hedgehog . checkParallel $ Group "hedgehog roundtrips" tests

numTests :: TestLimit
numTests = 120

scaledprop :: PropertyT IO () -> Property
scaledprop = withTests numTests . property

tests :: [(PropertyName, Property)]
tests =
  [ ( "instance HasXML Document",
      mkpropHasXML' normalizeDoc genXMLDocument
    ),
    ( "instance HasXML NameID",
      mkpropHasXML genNameID
    ),
    ( "instance HasXML IdPMetadata",
      mkpropHasXML genIdPMetadata
    ),
    ( "instance HasXML SPMetadata",
      mkpropHasXML genSPMetadata
    ),
    ( "instance HasXML AuthnRequest",
      mkpropHasXML genAuthnRequest
    ),
    ( "instance HasXML Conditions",
      mkpropHasXML' canonicalizeConditions . Gen.prune $ genConditions
    ),
    ( "instance HasXMLImport Conditions",
      mkpropHasXMLImport' canonicalizeConditions (Proxy @HS.Conditions) . Gen.prune $ genConditions
    ),
    ( "instance HasXML AuthnResponse",
      mkpropHasXML' canonicalizeAuthnResponse . Gen.prune $ genAuthnResponse
      -- without the 'prune', this triggers https://github.com/hedgehogqa/haskell-hedgehog/issues/174
    ),
    ( "instance {From,To}JSON Config",
      mkpropJSON genConfig
    ),
    ( "instance {From,To}JSON IdPId",
      mkpropJSON genIdPId
    ),
    ( "instance {From,To}JSON Issuer",
      mkpropJSON genIssuer
    ),
    ( "instance {From,To}JSON URI",
      mkpropJSON genHttps
    ),
    ( "instance {From,To}JSON SignedCertificate",
      mkpropJSON genSignedCertificate
    ),
    ( "instance FromHttpApiData IdPId",
      mkpropHttpApiData genIdPId
    ),
    ( "instance FromHttpApiData (SimpleSetCookie (name :: Symbol))",
      mkpropHttpApiData (genSimpleSetCookie @"gnfz")
    ),
    ( "instance Mime{Render,Unrender} HTML (FormRedirect AuthnRequest)",
      mkpropMimeRender (Proxy @HTML) (genFormRedirect genAuthnRequest)
    )
    -- this one *almost* type-checks, and i'm not sure it's worth it...
    -- , ( "instance FromMultipart Mem AuthnResponseBody <=> authnResponseBodyToMultipart"
    --   , scaledprop $ Hedgehog.forAll genAuthnResponseBody >>=
    --       \a -> Hedgehog.tripping a authnResponseBodyToMultipart fromMultipart
    --   )
  ]

canonicalizeConditions :: Conditions -> Conditions
canonicalizeConditions = condAudienceRestriction %~ sort . fmap NL.sort

canonicalizeAuthnResponse :: AuthnResponse -> AuthnResponse
canonicalizeAuthnResponse = rspPayload %~ fmap (assConditions . _Just %~ canonicalizeConditions)

mkpropHasXML ::
  forall a.
  (Eq a, Show a, HasXML a) =>
  Gen a ->
  Property
mkpropHasXML = mkpropHasXML' id

mkpropHasXML' ::
  forall a.
  (Eq a, Show a, HasXML a) =>
  (a -> a) ->
  Gen a ->
  Property
mkpropHasXML' canon gen = scaledprop $ do
  v <- forAll (canon <$> gen)
  tripping v encodeElem (fmap canon . decodeElem @a @(Either String))

mkpropHasXMLImport ::
  forall them us.
  (Eq us, Show us, Show them, HasXMLImport us them) =>
  Proxy them ->
  Gen us ->
  Property
mkpropHasXMLImport = mkpropHasXMLImport' id

mkpropHasXMLImport' ::
  forall them us.
  (Eq us, Show us, Show them, HasXMLImport us them) =>
  (us -> us) ->
  Proxy them ->
  Gen us ->
  Property
mkpropHasXMLImport' canon _ gen = scaledprop $ do
  v <- forAll (canon <$> gen)
  tripping v exportXml (fmap canon . importXml @us @them @(Either String))

mkpropMimeRender ::
  forall t a.
  (Eq a, Show a, MimeRender t a, MimeUnrender t a) =>
  Proxy t ->
  Gen a ->
  Property
mkpropMimeRender proxy gen =
  scaledprop $
    Hedgehog.forAll gen >>= \a -> Hedgehog.tripping a (mimeRender proxy) (mimeUnrender proxy)

mkpropJSON ::
  forall a.
  (Eq a, Show a, FromJSON a, ToJSON a) =>
  Gen a ->
  Property
mkpropJSON gen =
  scaledprop $
    Hedgehog.forAll gen >>= \a -> Hedgehog.tripping a Aeson.encode Aeson.decode

mkpropHttpApiData ::
  forall a.
  (Eq a, Show a, FromHttpApiData a, ToHttpApiData a) =>
  Gen a ->
  Property
mkpropHttpApiData gen =
  scaledprop $
    Hedgehog.forAll gen >>= \a -> Hedgehog.tripping a toUrlPiece parseUrlPiece

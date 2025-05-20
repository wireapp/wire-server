{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module SAML2.WebSSO.Test.MockResponse where

import Control.Lens
import Control.Monad.IO.Class
import Data.Generics.Uniplate.Data
import Data.String.Conversions
import Data.UUID as UUID
import GHC.Stack
import SAML2.Util
import SAML2.WebSSO
import Text.Hamlet.XML (xml)
import Text.XML
import Text.XML.DSig

newtype SignedAuthnResponse = SignedAuthnResponse {fromSignedAuthnResponse :: Document}
  deriving (Eq, Show)

-- | See tests on how this is used.
mkAuthnResponse ::
  (HasCallStack, HasMonadSign m, HasLogger m, HasCreateUUID m, HasNow m) =>
  SignPrivCreds ->
  IdPConfig extra ->
  SPMetadata ->
  Maybe AuthnRequest ->
  Bool ->
  m SignedAuthnResponse
mkAuthnResponse creds idp spmeta mbareq grant = do
  subj <- unspecifiedNameID . UUID.toText <$> createUUID
  mkAuthnResponseWithSubj subj creds idp spmeta mbareq grant

-- | Replace the 'NameID' child of the 'Subject' with a given one.
--
-- (There is some code sharing between this and 'mkAuthnResponseWithRawSubj', but reducing it would
-- make both functions more complex.)
mkAuthnResponseWithSubj ::
  forall extra m.
  (HasCallStack, HasMonadSign m, HasCreateUUID m, HasNow m) =>
  NameID ->
  SignPrivCreds ->
  IdPConfig extra ->
  SPMetadata ->
  Maybe AuthnRequest ->
  Bool ->
  m SignedAuthnResponse
mkAuthnResponseWithSubj subj = mkAuthnResponseWithModif modif id
  where
    modif =
      transformBis
        [ [ transformer $ \case
              el@(Element "{urn:oasis:names:tc:SAML:2.0:assertion}Subject" _ _) ->
                case parse [NodeElement el] of
                  Right (Subject _ sc) -> nodesToElem . render $ Subject subj sc
                  Left bad -> error $ show bad
              other -> other
          ]
        ]

-- | Delete all children of 'Subject' and insert some new ones.
mkAuthnResponseWithRawSubj ::
  forall extra m.
  (HasCallStack, HasMonadSign m, HasCreateUUID m, HasNow m) =>
  [Node] ->
  SignPrivCreds ->
  IdPConfig extra ->
  SPMetadata ->
  Maybe AuthnRequest ->
  Bool ->
  m SignedAuthnResponse
mkAuthnResponseWithRawSubj subj = mkAuthnResponseWithModif modif id
  where
    modif =
      transformBis
        [ [ transformer $ \case
              (Element tag@"{urn:oasis:names:tc:SAML:2.0:assertion}Subject" attrs _) ->
                Element tag attrs subj
              other -> other
          ]
        ]

mkAuthnResponseWithModif ::
  (HasCallStack, HasMonadSign m, HasCreateUUID m, HasNow m) =>
  ([Node] -> [Node]) ->
  ([Node] -> [Node]) ->
  SignPrivCreds ->
  IdPConfig extra ->
  SPMetadata ->
  Maybe AuthnRequest ->
  Bool ->
  m SignedAuthnResponse
mkAuthnResponseWithModif modifUnsignedAssertion modifAll creds idp sp mbauthnreq grantAccess = do
  let freshNCName = ("_" <>) . UUID.toText <$> createUUID
  assertionUuid <- freshNCName
  respUuid <- freshNCName
  now <- getNow
  let issueInstant = renderTime now
      expires = renderTime $ 3600 `addTime` now
      idpissuer :: ST = idp ^. idpMetadata . edIssuer . fromIssuer . to renderURI
      recipient :: ST = sp ^. spResponseURL . to renderURI
      mbspissuer :: Maybe ST = (^. rqIssuer . fromIssuer . to renderURI) <$> mbauthnreq
      mbinResponseTo :: Maybe ST = fromID . (^. rqID) <$> mbauthnreq
      status
        | grantAccess = "urn:oasis:names:tc:SAML:2.0:status:Success"
        | otherwise = "urn:oasis:names:tc:SAML:2.0:status:Requester"
  assertion :: [Node] <-
    liftIO $
      signElementIOAt 1 creds . modifUnsignedAssertion . repairNamespaces $
        [xml|
        <Assertion
          xmlns="urn:oasis:names:tc:SAML:2.0:assertion"
          Version="2.0"
          ID="#{assertionUuid}"
          IssueInstant="#{issueInstant}">
            <Issuer>
                #{idpissuer}
            <Subject>
                <NameID Format="urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress">
                    #{"emil@email.com"}
                <SubjectConfirmation Method="urn:oasis:names:tc:SAML:2.0:cm:bearer">
                    $maybe inResponseTo <- mbinResponseTo
                      <SubjectConfirmationData
                        InResponseTo="#{inResponseTo}"
                        NotOnOrAfter="#{expires}"
                        Recipient="#{recipient}">
                    $nothing
                      <SubjectConfirmationData
                        NotOnOrAfter="#{expires}"
                        Recipient="#{recipient}">
            <Conditions NotBefore="#{issueInstant}" NotOnOrAfter="#{expires}">
                $maybe spissuer <- mbspissuer
                    <AudienceRestriction>
                        <Audience>
                            #{spissuer}
            <AuthnStatement AuthnInstant="#{issueInstant}" SessionIndex="_e9ae1025-bc03-4b5a-943c-c9fcb8730b21">
                <AuthnContext>
                    <AuthnContextClassRef>
                        urn:oasis:names:tc:SAML:2.0:ac:classes:Password
      |]
  let authnResponse :: Element
      [NodeElement authnResponse] =
        -- it's safe to skip the `inResponseTo` attribute here because it is optional and
        -- wire-server ignores it: it's not signed, and shouldn't be considered trustworthy no
        -- matter what it contains.
        modifAll
          . repairNamespaces
          $ [xml|
          <samlp:Response
            xmlns:samlp="urn:oasis:names:tc:SAML:2.0:protocol"
            ID="#{respUuid}"
            Version="2.0"
            Destination="#{recipient}"
            IssueInstant="#{issueInstant}">
              <Issuer xmlns="urn:oasis:names:tc:SAML:2.0:assertion">
                  #{idpissuer}
              <samlp:Status>
                  <samlp:StatusCode Value="#{status}">
              ^{assertion}
        |]
  pure . SignedAuthnResponse $ mkDocument authnResponse

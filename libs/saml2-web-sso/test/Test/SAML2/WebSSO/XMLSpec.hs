{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-binds -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module Test.SAML2.WebSSO.XMLSpec
  ( spec,
  )
where

import Data.ByteString as SBS
import Data.ByteString.Base64 qualified
import Data.ByteString.Lazy as LBS
import Data.Either
import Data.Default
import Imports

import qualified Data.ByteString.Lazy.Char8 as CS
import qualified Data.ByteString.Lazy            as BS
import qualified Data.ByteString.Lazy.UTF8       as LBSUTF8

import Data.String.Conversions
import Data.Text.Lazy qualified as LT
import Data.Tree.NTree.TypeDefs
import SAML2.Core qualified as HS
-- import Debug.Trace
import SAML2.Util
import SAML2.WebSSO
import SAML2.XML qualified as HS
import SAML2.XML.Canonical
import Test.Hspec
import Text.XML as XMLC
import Text.XML.HXT.DOM.TypeDefs
import URI.ByteString.QQ (uri)

-- | embed an email into a valid NameID context
xmlWithName :: Maybe LT -> LT -> LT
xmlWithName mformat txt =
  "<NameID " <> namespaces <> format <> " " <> namespaces2 <> ">" <> txt <> "</NameID>"
  where
    namespaces =
      LT.unwords
        [ "xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\"",
          "xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\"",
          "xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\"",
          "xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\""
        ]
    namespaces2 =
      LT.unwords
        [ "xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\""
        ]
    format = maybe "" (\f -> " Format=\"" <> f <> "\"") mformat

emailFormat :: LT
emailFormat = "urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress"

unspecifiedFormat :: LT
unspecifiedFormat = "urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified"

entityFormat :: LT
entityFormat = "urn:oasis:names:tc:SAML:2.0:nameid-format:entity"

spec :: Spec
spec = describe "XML Sanitization" $ do
  describe "decodeElem" $ do
    it "should decode a valid email" $ do
      decodeElem (xmlWithName (Just emailFormat) "somebody@example.org")
        `shouldBe` (emailNameID "somebody@example.org" :: Either String NameID)
    it "should fail to decode an invalid email" $ do
      decodeElem @NameID (xmlWithName (Just emailFormat) "&lt;somebody@example.org&gt;")
        `shouldBe` Left "HasXML: could not parse NameID: \"\\\": Failed reading: satisfyWith\""
    -- the "\": Failed reading: satisfyWith" part is from 'Text.Email.Validate.validate'

    it "should decode an escaped name if format is unspecified" $ do
      decodeElem (xmlWithName (Just unspecifiedFormat) "&lt;somebody@example.org&gt;")
        `shouldBe` Right (unspecifiedNameID "<somebody@example.org>")
    it "should unescape names" $ do
      decodeElem (xmlWithName (Just unspecifiedFormat) "&lt;somebody@example.org&gt;")
        `shouldBe` Right (unspecifiedNameID "<somebody@example.org>")
    it "should not unescape more than once" $ do
      decodeElem
        (xmlWithName (Just unspecifiedFormat) "&amp;lt;somebody@example.org&amp;gt;")
        `shouldBe` ( mkNameID
                       ( UNameIDUnspecified
                           (mkXmlText "&lt;somebody@example.org&gt;")
                       )
                       Nothing
                       Nothing
                       Nothing ::
                       Either String NameID
                   )
    it "should not unescape text multiple times" $ do
      decodeElem (xmlWithName (Just entityFormat) "https://www.google.com/search?q=hello&lt;world&gt;")
        `shouldBe` ( mkNameID
                       ( UNameIDEntity
                           ( fromRight (error "bad uri in tests") $
                               parseURI' "https://www.google.com/search?q=hello<world>"
                           )
                       )
                       Nothing
                       Nothing
                       Nothing ::
                       Either String NameID
                   )
    it "rendering doesn't double escape" $ do
      encodeElem (unspecifiedNameID "<something>")
        `shouldBe` (xmlWithName Nothing "&lt;something&gt;")
    it "rendering escapes emails correctly" $ do
      encodeElem (fromRight (error "bad test case") $ emailNameID "a&@b.c")
        `shouldBe` (xmlWithName (Just emailFormat) "a&amp;@b.c")
    it "rendering escapes urls correctly" $ do
      encodeElem (entityNameID [uri|http://example.com/?<&>|])
        `shouldBe` (xmlWithName (Just entityFormat) "http://example.com/?%3C=&amp;%3E=")
    it "sadly, hsaml2 does not escape unsafe strings" $ do
      -- this test case reproduces an issue with hsaml2 that motivates us manually escaping
      -- the 'XmlText's in the serialization functions here in saml2-web-sso.

      -- it really shouldn't, though!
      HS.samlToXML (HS.simpleNameID HS.NameIDFormatUnspecified "<something>")
        `shouldBe` "<NameID xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\"><something></NameID>"

      -- this is good!
      HS.xmlToSAML @HS.NameID "<NameID xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\"><something></NameID>"
        `shouldSatisfy` isLeft

      -- this is good!
      HS.xmlToSAML @HS.NameID "<NameID xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">&lt;something&gt;</NameID>"
        `shouldBe` Right (HS.simpleNameID HS.NameIDFormatUnspecified "<something>")

    it "counter-example base case" $ do
      let xin = "<NameID xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">Caro</NameID>"
          xout = "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">Caro</NameID>"
      (fmap encodeElem . decodeElem @NameID) xin `shouldBe` Right xout

    ----------------------------------------------------------------------

    focus . it "counter-example unicode" $ do
      let xin :: LT.Text = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><NameID xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">Căro</NameID>"
          xout :: LT.Text = "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">Căro</NameID>"

          xmlcIn :: Either SomeException Document = XMLC.parseText def xin
          xmlcInExpected = Document {documentPrologue = Prologue {prologueBefore = [], prologueDoctype = Nothing, prologueAfter = []},
                                     documentRoot = Element {elementName = Name {nameLocalName = "NameID", nameNamespace = Just "urn:oasis:names:tc:SAML:2.0:assertion", namePrefix = Nothing}, elementAttributes = M.fromList [], elementNodes = [NodeContent "Căro"]},
                                     documentEpilogue = []}
          Right decodeElemExpected = mkNameID (UNameIDUnspecified (mkXmlText "Căro")) Nothing Nothing Nothing

          xmlcOut :: LByteString = XMLC.renderLBS def (either (error . show) id xmlcIn)
          xmlcOutExpected :: LT.Text = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><NameID xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">Căro</NameID>"

      either (error . show) id xmlcIn `shouldBe` xmlcInExpected
      xmlcOut `shouldBe` cs xmlcOutExpected

      -- do {en,de}codeElem work?
      (decodeElem @NameID) xin `shouldBe` Right decodeElemExpected

      {-
          hypothesis: we're running encodeString from utf8-string in some places, but not in others.
      -}

      encodeElem decodeElemExpected `shouldBe` xout

    ----------------------------------------------------------------------

    focus . it "bla" $ do
      (i, o) <- canonicalizeCounterExample "PGE+w6Q8L2E+"
      o `shouldBe` i

    focus . it "hihi-utf8" $ do
      LBSUTF8.toString (either (error "badcase") SBS.fromStrict $ Data.ByteString.Base64.decode "PGE+w6Q8L2E+")
        `shouldBe` "<a>ä</a>"

    focus . it "hihi-char8" $ do
      CS.unpack (either (error "badcase") SBS.fromStrict $ Data.ByteString.Base64.decode "PGE+w6Q8L2E+")
        `shouldBe` "<a>ä</a>"

canonicalizeCounterExample :: (HasCallStack) => SBS.ByteString -> IO (LBS.ByteString, LBS.ByteString)
canonicalizeCounterExample base64input = do
  let inbs :: LBS.ByteString
      inbs = either (error "badcase") BS.fromStrict $ Data.ByteString.Base64.decode base64input

      tree :: XmlTree
      tree = fromMaybe (error "badcase") $ HS.xmlToDoc inbs

      algo :: CanonicalizationAlgorithm
      algo = CanonicalXMLExcl10 {canonicalWithComments = True}

  outbs :: LBS.ByteString <- SBS.fromStrict <$> canonicalize algo Nothing Nothing (NTree (XTag (mkQName "" "" "root") []) [tree])

  pure (inbs, outbs)

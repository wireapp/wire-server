{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-binds -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module Test.SAML2.WebSSO.XMLSpec
  ( spec,
  )
where

import Data.Default
import Data.Either
import Data.String.Conversions
import Data.Text.Lazy qualified as LT
import Imports
import SAML2.Core qualified as HS
import SAML2.Util
import SAML2.WebSSO
import SAML2.XML qualified as HS
import Test.Hspec
import Text.XML as XMLC
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
                       (UNameIDUnspecified "&lt;somebody@example.org&gt;")
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
    describe "these tests are weird and caused by the XmlText type removed in may 2025" $ do
      it "rendering doesn't double escape" $ do
        encodeElem (unspecifiedNameID "<something>")
          `shouldBe` (xmlWithName Nothing "&lt;something&gt;")

      it "rendering escapes emails correctly" $ do
        encodeElem (fromRight (error "bad test case") $ emailNameID "a&@b.c")
          `shouldBe` (xmlWithName (Just emailFormat) "a&amp;@b.c")

      it "rendering escapes urls correctly" $ do
        encodeElem (entityNameID [uri|http://example.com/?<&>|])
          `shouldBe` (xmlWithName (Just entityFormat) "http://example.com/?%3C=&amp;%3E=")

    describe "unicode, utf8, and xml string escaping (this was quite a journey...)" $ do
      it "hsaml2 does the right thing: bad xml." $ do
        HS.xmlToSAML @HS.NameID "<NameID xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\"><something></NameID>"
          `shouldSatisfy` isLeft

      it "hsaml2 does the right thing: properly escaped xml." $ do
        HS.xmlToSAML @HS.NameID "<NameID xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">&lt;something&gt;</NameID>"
          `shouldBe` Right (HS.simpleNameID HS.NameIDFormatUnspecified "<something>")

      it "counter-example base case" $ do
        let xin = "<NameID xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">Caro</NameID>"
            xout = "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">Caro</NameID>"
        (fmap encodeElem . decodeElem @NameID) xin `shouldBe` Right xout

      it "counter-example unicode" $ do
        let xin :: LT.Text = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><NameID xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">Căro</NameID>"
            xout :: LT.Text = "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">Căro</NameID>"

            xmlcIn :: Either SomeException Document = XMLC.parseText def xin
            xmlcInExpected =
              Document
                { documentPrologue = Prologue {prologueBefore = [], prologueDoctype = Nothing, prologueAfter = []},
                  documentRoot = Element {elementName = Name {nameLocalName = "NameID", nameNamespace = Just "urn:oasis:names:tc:SAML:2.0:assertion", namePrefix = Nothing}, elementAttributes = mempty, elementNodes = [NodeContent "Căro"]},
                  documentEpilogue = []
                }
            Right decodeElemExpected = mkNameID (UNameIDUnspecified "Căro") Nothing Nothing Nothing

            xmlcOut :: LByteString = XMLC.renderLBS def (either (error . show) id xmlcIn)
            xmlcOutExpected :: LT.Text = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><NameID xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">Căro</NameID>"

        either (error . show) id xmlcIn `shouldBe` xmlcInExpected
        xmlcOut `shouldBe` cs xmlcOutExpected

        -- do {en,de}codeElem work?
        (decodeElem @NameID) xin `shouldBe` Right decodeElemExpected
        encodeElem decodeElemExpected `shouldBe` xout

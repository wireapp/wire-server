{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-binds -Wno-incomplete-patterns -Wno-incomplete-uni-patterns -Wno-orphans #-}

module Test.Text.XML.DSigSpec
  ( spec,
  )
where

import Data.ByteString.Base64.Lazy qualified as EL
import Data.Either
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map qualified as Map
import Data.String.Conversions
import Data.UUID qualified as UUID
import Data.X509 qualified as X509
import Imports
import SAML2.WebSSO.Test.Util
import SAML2.XML
import SAML2.XML.Signature qualified as HS
import Samples qualified
import Test.Hspec
import Text.Hamlet.XML (xml)
import Text.XML
import Text.XML.DSig
import Text.XML.HXT.DOM.TypeDefs qualified as HXTC

spec :: Spec
spec = describe "xml:dsig" $ do
  describe "parseKeyInfo" $ do
    it "works(1)" $ do
      keyinfo <- readSampleIO "microsoft-idp-keyinfo.xml"
      let want = Samples.microsoftIdpKeyinfo
          Right (SignCreds _ (SignKeyRSA have)) = certToCreds =<< parseKeyInfo True keyinfo
      have `shouldBe` want
    it "works(2)" $ do
      keyinfo <- readSampleIO "okta-keyinfo-1.xml"
      (certToCreds =<< parseKeyInfo True keyinfo) `shouldSatisfy` isRight
    it "works against mkSignCredsWithCert" $ do
      (_privcreds, creds, cert) <- mkSignCredsWithCert Nothing 192
      verifySelfSignature cert `shouldBe` Right ()
      certToCreds cert `shouldBe` Right creds

  describe "parseKeyInfo / renderKeyInfo roundtrip" $ do
    let check :: (HasCallStack) => Int -> Expectation
        check size = do
          (_, _, x :: X509.SignedCertificate) <- mkSignCredsWithCert Nothing size
          let y :: LT = renderKeyInfo x
          let z :: X509.SignedCertificate = either error id $ parseKeyInfo True y
          x `shouldBe` z
    it "works (96 bytes)" $ replicateM_ 10 (check 96)
    it "works (128 bytes)" $ check 128
    it "works (256 bytes)" $ check 256
    it "works (512 bytes)" $ check 512

  describe "verify" $ do
    it "works" $ do
      Right keyinfo <- (parseKeyInfo True >=> certToCreds) <$> readSampleIO "microsoft-idp-keyinfo.xml"
      raw <- cs <$> readSampleIO "microsoft-authnresponse-2.xml"
      verify (keyinfo :| []) raw "_c79c3ec8-1c26-4752-9443-1f76eb7d5dd6" `shouldSatisfy` isRight
    it "works with more than one key" $ do
      SampleIdP _ _ cert _ <- makeSampleIdPMetadata
      Right keyinfo <- (parseKeyInfo True >=> certToCreds) <$> readSampleIO "microsoft-idp-keyinfo.xml"
      raw <- cs <$> readSampleIO "microsoft-authnresponse-2.xml"
      verify (keyinfo :| [cert]) raw "_c79c3ec8-1c26-4752-9443-1f76eb7d5dd6" `shouldSatisfy` isRight

  describe "verifyRoot (verify without the subtree picker)" $ do
    it "works" $ do
      Right keyinfo <- (parseKeyInfo True >=> certToCreds) <$> readSampleIO "microsoft-idp-keyinfo.xml"
      raw <- cs <$> readSampleIO "microsoft-meta-2.xml"
      verifyRoot (keyinfo :| []) raw `shouldSatisfy` isRight

  describe "verifyRoot vs. signRoot" $ do
    let check :: (HasCallStack) => Bool -> Bool -> (Either String HXTC.XmlTree -> Bool) -> Spec
        check withMatchingCreds withID expected =
          it (show (withMatchingCreds, withID)) $ do
            (privCreds, pubCreds) <- mkcrds withMatchingCreds
            signature <- runMonadSign $ renderLBS def <$> signRootAt 0 privCreds (doc withID)
            (verifyRoot (pubCreds :| []) =<< signature) `shouldSatisfy` expected
        mkcrds :: Bool -> IO (SignPrivCreds, SignCreds)
        mkcrds = \case
          True -> mkSignCreds keysize
          False -> (,) <$> (fst <$> mkSignCreds keysize) <*> (snd <$> mkSignCreds keysize)
        keysize = 192 -- not long enough for security, but hopefully long enough for swift testing
        someID withID = Map.fromList [("ID", UUID.toText UUID.nil) | withID]
        doc withID = Document (Prologue [] Nothing []) (Element "root" (someID withID) root) []
        root =
          [xml|
                  <bloo hign="___">
                    <ack hoghn="true">
                      <nonack>
                    hackach
                |]
    check True True isRight
    check True False isRight
    check False True isLeft
    check False False isLeft
    it "keeps data intact" $ do
      (privCreds, _pubCreds) <- mkcrds True
      Right outcome <- runMonadSign (cs . renderLBS def <$> signRootAt 0 privCreds (doc False))
      (outcome `shouldContain`) `mapM_` ["bloo", "ack", "hackach", "hackach"]
    it "honors non-0 signature position." $ do
      (privCreds, _pubCreds) <- mkcrds True
      Right signed <- runMonadSign $ signRootAt 1 privCreds (doc False)
      case signed of
        Document
          _
          ( Element
              "root"
              _
              [ NodeElement (Element "bloo" _ _),
                NodeElement (Element "{http://www.w3.org/2000/09/xmldsig#}Signature" _ _)
                ]
            )
          _ ->
            pure ()
        bad -> error $ show bad
    it "throws an error is signature position points outside the children list." $ do
      (privCreds, _pubCreds) <- mkcrds True
      outcome <- runMonadSign $ signRootAt 2 privCreds (doc False)
      outcome `shouldSatisfy` isLeft

  describe "signature verification helpers cloned from hsaml2" $ do
    runVerifyExample `mapM_` examples

data VerifyExample
  = VerifyExample
      LByteString -- keyinfo from metadata of idp
      LByteString -- signed response (in the base64 encoded form from the multipart body)
      String -- identifier of the sub-tree of which the signature is to be verified
      (forall a. Either HS.SignatureError a -> Bool) -- expected result
      Int -- serial number

runVerifyExample :: VerifyExample -> Spec
runVerifyExample (VerifyExample keys xmltree refid want examplenumber) = it (show examplenumber) $ do
  let keys' = either (error . show) id $ prsKey keys
  let xmltree' = either (error . show) id $ (EL.decode >=> xmlToDocE) xmltree
  have <- verifySignatureUnenvelopedSigs keys' refid xmltree'
  void have `shouldSatisfy` want
  where
    prsKey :: LByteString -> Either String HS.PublicKeys
    prsKey = getCert >=> getKeys
      where
        getCert :: LByteString -> Either String X509.SignedCertificate
        getCert raw = case xmlToSAML @HS.KeyInfo raw of
          (Right (HS.keyInfoElements -> HS.X509Data (HS.X509Certificate cert :| []) :| [])) -> Right cert
          bad -> Left $ "unsupported: " ++ show bad

        getKeys :: X509.SignedCertificate -> Either String HS.PublicKeys
        getKeys cert = do
          case X509.certPubKey . X509.signedObject $ X509.getSigned cert of
            X509.PubKeyDSA pk -> Right $ HS.PublicKeys (Just pk) Nothing
            X509.PubKeyRSA pk -> Right $ HS.PublicKeys Nothing (Just pk)
            bad -> Left $ "unsupported: " ++ show bad

examples :: [VerifyExample]
examples = zipWith ($) xs [1 ..]
  where
    xs :: [Int -> VerifyExample]
    xs =
      [ VerifyExample
          "<KeyInfo xmlns=\"http://www.w3.org/2000/09/xmldsig#\"><X509Data><X509Certificate>MIIDBTCCAe2gAwIBAgIQev76BWqjWZxChmKkGqoAfDANBgkqhkiG9w0BAQsFADAtMSswKQYDVQQDEyJhY2NvdW50cy5hY2Nlc3Njb250cm9sLndpbmRvd3MubmV0MB4XDTE4MDIxODAwMDAwMFoXDTIwMDIxOTAwMDAwMFowLTErMCkGA1UEAxMiYWNjb3VudHMuYWNjZXNzY29udHJvbC53aW5kb3dzLm5ldDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAMgmGiRfLh6Fdi99XI2VA3XKHStWNRLEy5Aw/gxFxchnh2kPdk/bejFOs2swcx7yUWqxujjCNRsLBcWfaKUlTnrkY7i9x9noZlMrijgJy/Lk+HH5HX24PQCDf+twjnHHxZ9G6/8VLM2e5ZBeZm+t7M3vhuumEHG3UwloLF6cUeuPdW+exnOB1U1fHBIFOG8ns4SSIoq6zw5rdt0CSI6+l7b1DEjVvPLtJF+zyjlJ1Qp7NgBvAwdiPiRMU4l8IRVbuSVKoKYJoyJ4L3eXsjczoBSTJ6VjV2mygz96DC70MY3avccFrk7tCEC6ZlMRBfY1XPLyldT7tsR3EuzjecSa1M8CAwEAAaMhMB8wHQYDVR0OBBYEFIks1srixjpSLXeiR8zES5cTY6fBMA0GCSqGSIb3DQEBCwUAA4IBAQCKthfK4C31DMuDyQZVS3F7+4Evld3hjiwqu2uGDK+qFZas/D/eDunxsFpiwqC01RIMFFN8yvmMjHphLHiBHWxcBTS+tm7AhmAvWMdxO5lzJLS+UWAyPF5ICROe8Mu9iNJiO5JlCo0Wpui9RbB1C81Xhax1gWHK245ESL6k7YWvyMYWrGqr1NuQcNS0B/AIT1Nsj1WY7efMJQOmnMHkPUTWryVZlthijYyd7P2Gz6rY5a81DAFqhDNJl2pGIAE6HWtSzeUEh3jCsHEkoglKfm4VrGJEuXcALmfCMbdfTvtu4rlsaP2hQad+MG/KJFlenoTK34EMHeBPDCpqNDz8UVNk</X509Certificate></X509Data></KeyInfo>"
          "PHNhbWxwOlJlc3BvbnNlIElEPSJfM2FlYjMwNTQtZTg1Zi00MWZhLWEyMGYtMGYyNzhiMzI3ZjRlIiBWZXJzaW9uPSIyLjAiIElzc3VlSW5zdGFudD0iMjAxOC0wNC0xNFQwOTo1ODo1OC40NTdaIiBEZXN0aW5hdGlvbj0iaHR0cHM6Ly96YjIuemVyb2J1enoubmV0OjYwNDQzL2F1dGhyZXNwIiBJblJlc3BvbnNlVG89ImlkY2YyMjk5YWM1NTFiNDJmMWFhOWI4ODgwNGVkMzA4YzIiIHhtbG5zOnNhbWxwPSJ1cm46b2FzaXM6bmFtZXM6dGM6U0FNTDoyLjA6cHJvdG9jb2wiPjxJc3N1ZXIgeG1sbnM9InVybjpvYXNpczpuYW1lczp0YzpTQU1MOjIuMDphc3NlcnRpb24iPmh0dHBzOi8vc3RzLndpbmRvd3MubmV0LzY4MmZlYmU4LTAyMWItNGZkZS1hYzA5LWU2MDA4NWYwNTE4MS88L0lzc3Vlcj48c2FtbHA6U3RhdHVzPjxzYW1scDpTdGF0dXNDb2RlIFZhbHVlPSJ1cm46b2FzaXM6bmFtZXM6dGM6U0FNTDoyLjA6c3RhdHVzOlN1Y2Nlc3MiLz48L3NhbWxwOlN0YXR1cz48QXNzZXJ0aW9uIElEPSJfYzc5YzNlYzgtMWMyNi00NzUyLTk0NDMtMWY3NmViN2Q1ZGQ2IiBJc3N1ZUluc3RhbnQ9IjIwMTgtMDQtMTRUMDk6NTg6NTguNDQyWiIgVmVyc2lvbj0iMi4wIiB4bWxucz0idXJuOm9hc2lzOm5hbWVzOnRjOlNBTUw6Mi4wOmFzc2VydGlvbiI+PElzc3Vlcj5odHRwczovL3N0cy53aW5kb3dzLm5ldC82ODJmZWJlOC0wMjFiLTRmZGUtYWMwOS1lNjAwODVmMDUxODEvPC9Jc3N1ZXI+PFNpZ25hdHVyZSB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC8wOS94bWxkc2lnIyI+PFNpZ25lZEluZm8+PENhbm9uaWNhbGl6YXRpb25NZXRob2QgQWxnb3JpdGhtPSJodHRwOi8vd3d3LnczLm9yZy8yMDAxLzEwL3htbC1leGMtYzE0biMiLz48U2lnbmF0dXJlTWV0aG9kIEFsZ29yaXRobT0iaHR0cDovL3d3dy53My5vcmcvMjAwMS8wNC94bWxkc2lnLW1vcmUjcnNhLXNoYTI1NiIvPjxSZWZlcmVuY2UgVVJJPSIjX2M3OWMzZWM4LTFjMjYtNDc1Mi05NDQzLTFmNzZlYjdkNWRkNiI+PFRyYW5zZm9ybXM+PFRyYW5zZm9ybSBBbGdvcml0aG09Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvMDkveG1sZHNpZyNlbnZlbG9wZWQtc2lnbmF0dXJlIi8+PFRyYW5zZm9ybSBBbGdvcml0aG09Imh0dHA6Ly93d3cudzMub3JnLzIwMDEvMTAveG1sLWV4Yy1jMTRuIyIvPjwvVHJhbnNmb3Jtcz48RGlnZXN0TWV0aG9kIEFsZ29yaXRobT0iaHR0cDovL3d3dy53My5vcmcvMjAwMS8wNC94bWxlbmMjc2hhMjU2Ii8+PERpZ2VzdFZhbHVlPmxrV25SSUlBRm1IVmVXSVpWWGJhZGoxTzRhN05nNDRwL2NIQkZNOHFhYk09PC9EaWdlc3RWYWx1ZT48L1JlZmVyZW5jZT48L1NpZ25lZEluZm8+PFNpZ25hdHVyZVZhbHVlPmRhZ2VaWTV3aWdWVUpSeVg0bUZDZ0dMOVBhajRuVXpsTmFoQ2d4SkcybVU2M0RhTHpldm1qeWRITHVMWnhGR3MrNmxDRDhpb0xjNUpMckp3OFBlKzB3d1hZV0huTFAvWU53eVFSNWI2bVpUZWpOOUQvcFpORGNSdVRiQmZNeEdsTjhWVU5oaWg3OHRVL24xQmxiZE5oSGpBTmVTbGdPVUNlWWlIZWVzekRHaERYMi9RZ1p6OEJDL3FHa2ZBNUlIbHlSVHJBZkRoTmhGNFRpQ1F5N1haaVRqMXZ4WlE2ZDBBcTVGaWhFc0JtVW9qYnI1WW5KSjA2WjZ2KzRCS1lVWXNkUkY5RklWdlVtRCszckZORlJBQjdBMnp6c0RxYk82UUdham5YNURKaWtaNmtvTmFreFBsM3Jqd29lRWxwTzBDSG5oWXcyMEN1U09kMnVhK2ppeHRvdz09PC9TaWduYXR1cmVWYWx1ZT48S2V5SW5mbz48WDUwOURhdGE+PFg1MDlDZXJ0aWZpY2F0ZT5NSUlEQlRDQ0FlMmdBd0lCQWdJUWV2NzZCV3FqV1p4Q2htS2tHcW9BZkRBTkJna3Foa2lHOXcwQkFRc0ZBREF0TVNzd0tRWURWUVFERXlKaFkyTnZkVzUwY3k1aFkyTmxjM05qYjI1MGNtOXNMbmRwYm1SdmQzTXVibVYwTUI0WERURTRNREl4T0RBd01EQXdNRm9YRFRJd01ESXhPVEF3TURBd01Gb3dMVEVyTUNrR0ExVUVBeE1pWVdOamIzVnVkSE11WVdOalpYTnpZMjl1ZEhKdmJDNTNhVzVrYjNkekxtNWxkRENDQVNJd0RRWUpLb1pJaHZjTkFRRUJCUUFEZ2dFUEFEQ0NBUW9DZ2dFQkFNZ21HaVJmTGg2RmRpOTlYSTJWQTNYS0hTdFdOUkxFeTVBdy9neEZ4Y2huaDJrUGRrL2JlakZPczJzd2N4N3lVV3F4dWpqQ05Sc0xCY1dmYUtVbFRucmtZN2k5eDlub1psTXJpamdKeS9MaytISDVIWDI0UFFDRGYrdHdqbkhIeFo5RzYvOFZMTTJlNVpCZVptK3Q3TTN2aHV1bUVIRzNVd2xvTEY2Y1VldVBkVytleG5PQjFVMWZIQklGT0c4bnM0U1NJb3E2enc1cmR0MENTSTYrbDdiMURFalZ2UEx0SkYrenlqbEoxUXA3TmdCdkF3ZGlQaVJNVTRsOElSVmJ1U1ZLb0tZSm95SjRMM2VYc2pjem9CU1RKNlZqVjJteWd6OTZEQzcwTVkzYXZjY0Zyazd0Q0VDNlpsTVJCZlkxWFBMeWxkVDd0c1IzRXV6amVjU2ExTThDQXdFQUFhTWhNQjh3SFFZRFZSME9CQllFRklrczFzcml4anBTTFhlaVI4ekVTNWNUWTZmQk1BMEdDU3FHU0liM0RRRUJDd1VBQTRJQkFRQ0t0aGZLNEMzMURNdUR5UVpWUzNGNys0RXZsZDNoaml3cXUydUdESytxRlphcy9EL2VEdW54c0ZwaXdxQzAxUklNRkZOOHl2bU1qSHBoTEhpQkhXeGNCVFMrdG03QWhtQXZXTWR4TzVsekpMUytVV0F5UEY1SUNST2U4TXU5aU5KaU81SmxDbzBXcHVpOVJiQjFDODFYaGF4MWdXSEsyNDVFU0w2azdZV3Z5TVlXckdxcjFOdVFjTlMwQi9BSVQxTnNqMVdZN2VmTUpRT21uTUhrUFVUV3J5VlpsdGhpall5ZDdQMkd6NnJZNWE4MURBRnFoRE5KbDJwR0lBRTZIV3RTemVVRWgzakNzSEVrb2dsS2ZtNFZyR0pFdVhjQUxtZkNNYmRmVHZ0dTRybHNhUDJoUWFkK01HL0tKRmxlbm9USzM0RU1IZUJQRENwcU5EejhVVk5rPC9YNTA5Q2VydGlmaWNhdGU+PC9YNTA5RGF0YT48L0tleUluZm8+PC9TaWduYXR1cmU+PFN1YmplY3Q+PE5hbWVJRCBGb3JtYXQ9InVybjpvYXNpczpuYW1lczp0YzpTQU1MOjIuMDpuYW1laWQtZm9ybWF0OnBlcnNpc3RlbnQiPnhKeGRxUzhXMlVYYXdiWlpxcEdGWEtHNHVFbU81R2ppaktEMlJrTWlwQm88L05hbWVJRD48U3ViamVjdENvbmZpcm1hdGlvbiBNZXRob2Q9InVybjpvYXNpczpuYW1lczp0YzpTQU1MOjIuMDpjbTpiZWFyZXIiPjxTdWJqZWN0Q29uZmlybWF0aW9uRGF0YSBJblJlc3BvbnNlVG89ImlkY2YyMjk5YWM1NTFiNDJmMWFhOWI4ODgwNGVkMzA4YzIiIE5vdE9uT3JBZnRlcj0iMjAxOC0wNC0xNFQxMDowMzo1OC40NDJaIiBSZWNpcGllbnQ9Imh0dHBzOi8vemIyLnplcm9idXp6Lm5ldDo2MDQ0My9hdXRocmVzcCIvPjwvU3ViamVjdENvbmZpcm1hdGlvbj48L1N1YmplY3Q+PENvbmRpdGlvbnMgTm90QmVmb3JlPSIyMDE4LTA0LTE0VDA5OjUzOjU4LjQ0MloiIE5vdE9uT3JBZnRlcj0iMjAxOC0wNC0xNFQxMDo1Mzo1OC40NDJaIj48QXVkaWVuY2VSZXN0cmljdGlvbj48QXVkaWVuY2U+aHR0cHM6Ly96YjIuemVyb2J1enoubmV0OjYwNDQzL2F1dGhyZXNwPC9BdWRpZW5jZT48L0F1ZGllbmNlUmVzdHJpY3Rpb24+PC9Db25kaXRpb25zPjxBdHRyaWJ1dGVTdGF0ZW1lbnQ+PEF0dHJpYnV0ZSBOYW1lPSJodHRwOi8vc2NoZW1hcy5taWNyb3NvZnQuY29tL2lkZW50aXR5L2NsYWltcy90ZW5hbnRpZCI+PEF0dHJpYnV0ZVZhbHVlPjY4MmZlYmU4LTAyMWItNGZkZS1hYzA5LWU2MDA4NWYwNTE4MTwvQXR0cmlidXRlVmFsdWU+PC9BdHRyaWJ1dGU+PEF0dHJpYnV0ZSBOYW1lPSJodHRwOi8vc2NoZW1hcy5taWNyb3NvZnQuY29tL2lkZW50aXR5L2NsYWltcy9vYmplY3RpZGVudGlmaWVyIj48QXR0cmlidXRlVmFsdWU+Y2NmYjM3ODgtODI0MS00YWZlLTg4OTctZjMxM2YzNWY5ZTM3PC9BdHRyaWJ1dGVWYWx1ZT48L0F0dHJpYnV0ZT48QXR0cmlidXRlIE5hbWU9Imh0dHA6Ly9zY2hlbWFzLnhtbHNvYXAub3JnL3dzLzIwMDUvMDUvaWRlbnRpdHkvY2xhaW1zL25hbWUiPjxBdHRyaWJ1dGVWYWx1ZT5maXN4dDFAYXp1cmV3aXJlLm9ubWljcm9zb2Z0LmNvbTwvQXR0cmlidXRlVmFsdWU+PC9BdHRyaWJ1dGU+PEF0dHJpYnV0ZSBOYW1lPSJodHRwOi8vc2NoZW1hcy5taWNyb3NvZnQuY29tL2lkZW50aXR5L2NsYWltcy9kaXNwbGF5bmFtZSI+PEF0dHJpYnV0ZVZhbHVlPmZpc3h0MTwvQXR0cmlidXRlVmFsdWU+PC9BdHRyaWJ1dGU+PEF0dHJpYnV0ZSBOYW1lPSJodHRwOi8vc2NoZW1hcy5taWNyb3NvZnQuY29tL2lkZW50aXR5L2NsYWltcy9pZGVudGl0eXByb3ZpZGVyIj48QXR0cmlidXRlVmFsdWU+aHR0cHM6Ly9zdHMud2luZG93cy5uZXQvNjgyZmViZTgtMDIxYi00ZmRlLWFjMDktZTYwMDg1ZjA1MTgxLzwvQXR0cmlidXRlVmFsdWU+PC9BdHRyaWJ1dGU+PEF0dHJpYnV0ZSBOYW1lPSJodHRwOi8vc2NoZW1hcy5taWNyb3NvZnQuY29tL2NsYWltcy9hdXRobm1ldGhvZHNyZWZlcmVuY2VzIj48QXR0cmlidXRlVmFsdWU+aHR0cDovL3NjaGVtYXMubWljcm9zb2Z0LmNvbS93cy8yMDA4LzA2L2lkZW50aXR5L2F1dGhlbnRpY2F0aW9ubWV0aG9kL3Bhc3N3b3JkPC9BdHRyaWJ1dGVWYWx1ZT48L0F0dHJpYnV0ZT48L0F0dHJpYnV0ZVN0YXRlbWVudD48QXV0aG5TdGF0ZW1lbnQgQXV0aG5JbnN0YW50PSIyMDE4LTA0LTE0VDA5OjU4OjU1LjYxM1oiIFNlc3Npb25JbmRleD0iX2M3OWMzZWM4LTFjMjYtNDc1Mi05NDQzLTFmNzZlYjdkNWRkNiI+PEF1dGhuQ29udGV4dD48QXV0aG5Db250ZXh0Q2xhc3NSZWY+dXJuOm9hc2lzOm5hbWVzOnRjOlNBTUw6Mi4wOmFjOmNsYXNzZXM6UGFzc3dvcmQ8L0F1dGhuQ29udGV4dENsYXNzUmVmPjwvQXV0aG5Db250ZXh0PjwvQXV0aG5TdGF0ZW1lbnQ+PC9Bc3NlcnRpb24+PC9zYW1scDpSZXNwb25zZT4K"
          "_c79c3ec8-1c26-4752-9443-1f76eb7d5dd6"
          isRight,
        -- two assertions with the same identifier; only the second one is signed.
        VerifyExample
          "<KeyInfo xmlns=\"http://www.w3.org/2000/09/xmldsig#\"><X509Data><X509Certificate>MIIDBTCCAe2gAwIBAgIQev76BWqjWZxChmKkGqoAfDANBgkqhkiG9w0BAQsFADAtMSswKQYDVQQDEyJhY2NvdW50cy5hY2Nlc3Njb250cm9sLndpbmRvd3MubmV0MB4XDTE4MDIxODAwMDAwMFoXDTIwMDIxOTAwMDAwMFowLTErMCkGA1UEAxMiYWNjb3VudHMuYWNjZXNzY29udHJvbC53aW5kb3dzLm5ldDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAMgmGiRfLh6Fdi99XI2VA3XKHStWNRLEy5Aw/gxFxchnh2kPdk/bejFOs2swcx7yUWqxujjCNRsLBcWfaKUlTnrkY7i9x9noZlMrijgJy/Lk+HH5HX24PQCDf+twjnHHxZ9G6/8VLM2e5ZBeZm+t7M3vhuumEHG3UwloLF6cUeuPdW+exnOB1U1fHBIFOG8ns4SSIoq6zw5rdt0CSI6+l7b1DEjVvPLtJF+zyjlJ1Qp7NgBvAwdiPiRMU4l8IRVbuSVKoKYJoyJ4L3eXsjczoBSTJ6VjV2mygz96DC70MY3avccFrk7tCEC6ZlMRBfY1XPLyldT7tsR3EuzjecSa1M8CAwEAAaMhMB8wHQYDVR0OBBYEFIks1srixjpSLXeiR8zES5cTY6fBMA0GCSqGSIb3DQEBCwUAA4IBAQCKthfK4C31DMuDyQZVS3F7+4Evld3hjiwqu2uGDK+qFZas/D/eDunxsFpiwqC01RIMFFN8yvmMjHphLHiBHWxcBTS+tm7AhmAvWMdxO5lzJLS+UWAyPF5ICROe8Mu9iNJiO5JlCo0Wpui9RbB1C81Xhax1gWHK245ESL6k7YWvyMYWrGqr1NuQcNS0B/AIT1Nsj1WY7efMJQOmnMHkPUTWryVZlthijYyd7P2Gz6rY5a81DAFqhDNJl2pGIAE6HWtSzeUEh3jCsHEkoglKfm4VrGJEuXcALmfCMbdfTvtu4rlsaP2hQad+MG/KJFlenoTK34EMHeBPDCpqNDz8UVNk</X509Certificate></X509Data></KeyInfo>"
          "PHNhbWxwOlJlc3BvbnNlIElEPSJfM2FlYjMwNTQtZTg1Zi00MWZhLWEyMGYtMGYyNzhiMzI3ZjRlIiBWZXJzaW9uPSIyLjAiIElzc3VlSW5zdGFudD0iMjAxOC0wNC0xNFQwOTo1ODo1OC40NTdaIiBEZXN0aW5hdGlvbj0iaHR0cHM6Ly96YjIuemVyb2J1enoubmV0OjYwNDQzL2F1dGhyZXNwIiBJblJlc3BvbnNlVG89ImlkY2YyMjk5YWM1NTFiNDJmMWFhOWI4ODgwNGVkMzA4YzIiIHhtbG5zOnNhbWxwPSJ1cm46b2FzaXM6bmFtZXM6dGM6U0FNTDoyLjA6cHJvdG9jb2wiPjxJc3N1ZXIgeG1sbnM9InVybjpvYXNpczpuYW1lczp0YzpTQU1MOjIuMDphc3NlcnRpb24iPmh0dHBzOi8vc3RzLndpbmRvd3MubmV0LzY4MmZlYmU4LTAyMWItNGZkZS1hYzA5LWU2MDA4NWYwNTE4MS88L0lzc3Vlcj48c2FtbHA6U3RhdHVzPjxzYW1scDpTdGF0dXNDb2RlIFZhbHVlPSJ1cm46b2FzaXM6bmFtZXM6dGM6U0FNTDoyLjA6c3RhdHVzOlN1Y2Nlc3MiLz48L3NhbWxwOlN0YXR1cz48QXNzZXJ0aW9uIElEPSJfYzc5YzNlYzgtMWMyNi00NzUyLTk0NDMtMWY3NmViN2Q1ZGQ2IiBJc3N1ZUluc3RhbnQ9IjIwMTgtMDQtMTRUMDk6NTg6NTguNDQyWiIgVmVyc2lvbj0iMi4wIiB4bWxucz0idXJuOm9hc2lzOm5hbWVzOnRjOlNBTUw6Mi4wOmFzc2VydGlvbiI+PElzc3Vlcj5odHRwczovL3N0cy53aW5kb3dzLm5ldC82ODJmZWJlOC0wMjFiLTRmZGUtYWMwOS1lNjAwODVmMDUxODEvPC9Jc3N1ZXI+PEtleUluZm8+PFg1MDlEYXRhPjxYNTA5Q2VydGlmaWNhdGU+TUlJREJUQ0NBZTJnQXdJQkFnSVFldjc2QldxaldaeENobUtrR3FvQWZEQU5CZ2txaGtpRzl3MEJBUXNGQURBdE1Tc3dLUVlEVlFRREV5SmhZMk52ZFc1MGN5NWhZMk5sYzNOamIyNTBjbTlzTG5kcGJtUnZkM011Ym1WME1CNFhEVEU0TURJeE9EQXdNREF3TUZvWERUSXdNREl4T1RBd01EQXdNRm93TFRFck1Da0dBMVVFQXhNaVlXTmpiM1Z1ZEhNdVlXTmpaWE56WTI5dWRISnZiQzUzYVc1a2IzZHpMbTVsZERDQ0FTSXdEUVlKS29aSWh2Y05BUUVCQlFBRGdnRVBBRENDQVFvQ2dnRUJBTWdtR2lSZkxoNkZkaTk5WEkyVkEzWEtIU3RXTlJMRXk1QXcvZ3hGeGNobmgya1Bkay9iZWpGT3Myc3djeDd5VVdxeHVqakNOUnNMQmNXZmFLVWxUbnJrWTdpOXg5bm9abE1yaWpnSnkvTGsrSEg1SFgyNFBRQ0RmK3R3am5ISHhaOUc2LzhWTE0yZTVaQmVabSt0N00zdmh1dW1FSEczVXdsb0xGNmNVZXVQZFcrZXhuT0IxVTFmSEJJRk9HOG5zNFNTSW9xNnp3NXJkdDBDU0k2K2w3YjFERWpWdlBMdEpGK3p5amxKMVFwN05nQnZBd2RpUGlSTVU0bDhJUlZidVNWS29LWUpveUo0TDNlWHNqY3pvQlNUSjZWalYybXlnejk2REM3ME1ZM2F2Y2NGcms3dENFQzZabE1SQmZZMVhQTHlsZFQ3dHNSM0V1emplY1NhMU04Q0F3RUFBYU1oTUI4d0hRWURWUjBPQkJZRUZJa3Mxc3JpeGpwU0xYZWlSOHpFUzVjVFk2ZkJNQTBHQ1NxR1NJYjNEUUVCQ3dVQUE0SUJBUUNLdGhmSzRDMzFETXVEeVFaVlMzRjcrNEV2bGQzaGppd3F1MnVHREsrcUZaYXMvRC9lRHVueHNGcGl3cUMwMVJJTUZGTjh5dm1NakhwaExIaUJIV3hjQlRTK3RtN0FobUF2V01keE81bHpKTFMrVVdBeVBGNUlDUk9lOE11OWlOSmlPNUpsQ28wV3B1aTlSYkIxQzgxWGhheDFnV0hLMjQ1RVNMNms3WVd2eU1ZV3JHcXIxTnVRY05TMEIvQUlUMU5zajFXWTdlZk1KUU9tbk1Ia1BVVFdyeVZabHRoaWpZeWQ3UDJHejZyWTVhODFEQUZxaEROSmwycEdJQUU2SFd0U3plVUVoM2pDc0hFa29nbEtmbTRWckdKRXVYY0FMbWZDTWJkZlR2dHU0cmxzYVAyaFFhZCtNRy9LSkZsZW5vVEszNEVNSGVCUERDcHFORHo4VVZOazwvWDUwOUNlcnRpZmljYXRlPjwvWDUwOURhdGE+PC9LZXlJbmZvPjwvU2lnbmF0dXJlPjxTdWJqZWN0PjxOYW1lSUQgRm9ybWF0PSJ1cm46b2FzaXM6bmFtZXM6dGM6U0FNTDoyLjA6bmFtZWlkLWZvcm1hdDpwZXJzaXN0ZW50Ij54SnhkcVM4VzJVWGF3YlpacXBHRlhLRzR1RW1PNUdqaWpLRDJSa01pcEJvPC9OYW1lSUQ+PFN1YmplY3RDb25maXJtYXRpb24gTWV0aG9kPSJ1cm46b2FzaXM6bmFtZXM6dGM6U0FNTDoyLjA6Y206YmVhcmVyIj48U3ViamVjdENvbmZpcm1hdGlvbkRhdGEgSW5SZXNwb25zZVRvPSJpZGNmMjI5OWFjNTUxYjQyZjFhYTliODg4MDRlZDMwOGMyIiBOb3RPbk9yQWZ0ZXI9IjIwMTgtMDQtMTRUMTA6MDM6NTguNDQyWiIgUmVjaXBpZW50PSJodHRwczovL3piMi56ZXJvYnV6ei5uZXQ6NjA0NDMvYXV0aHJlc3AiLz48L1N1YmplY3RDb25maXJtYXRpb24+PC9TdWJqZWN0PjxDb25kaXRpb25zIE5vdEJlZm9yZT0iMjAxOC0wNC0xNFQwOTo1Mzo1OC40NDJaIiBOb3RPbk9yQWZ0ZXI9IjIwMTgtMDQtMTRUMTA6NTM6NTguNDQyWiI+PEF1ZGllbmNlUmVzdHJpY3Rpb24+PEF1ZGllbmNlPmh0dHBzOi8vemIyLnplcm9idXp6Lm5ldDo2MDQ0My9hdXRocmVzcDwvQXVkaWVuY2U+PC9BdWRpZW5jZVJlc3RyaWN0aW9uPjwvQ29uZGl0aW9ucz48QXR0cmlidXRlU3RhdGVtZW50PjxBdHRyaWJ1dGUgTmFtZT0iaHR0cDovL3NjaGVtYXMubWljcm9zb2Z0LmNvbS9pZGVudGl0eS9jbGFpbXMvdGVuYW50aWQiPjxBdHRyaWJ1dGVWYWx1ZT42ODJmZWJlOC0wMjFiLTRmZGUtYWMwOS1lNjAwODVmMDUxODE8L0F0dHJpYnV0ZVZhbHVlPjwvQXR0cmlidXRlPjxBdHRyaWJ1dGUgTmFtZT0iaHR0cDovL3NjaGVtYXMubWljcm9zb2Z0LmNvbS9pZGVudGl0eS9jbGFpbXMvb2JqZWN0aWRlbnRpZmllciI+PEF0dHJpYnV0ZVZhbHVlPmNjZmIzNzg4LTgyNDEtNGFmZS04ODk3LWYzMTNmMzVmOWUzNzwvQXR0cmlidXRlVmFsdWU+PC9BdHRyaWJ1dGU+PEF0dHJpYnV0ZSBOYW1lPSJodHRwOi8vc2NoZW1hcy54bWxzb2FwLm9yZy93cy8yMDA1LzA1L2lkZW50aXR5L2NsYWltcy9uYW1lIj48QXR0cmlidXRlVmFsdWU+ZmlzeHQxQGF6dXJld2lyZS5vbm1pY3Jvc29mdC5jb208L0F0dHJpYnV0ZVZhbHVlPjwvQXR0cmlidXRlPjxBdHRyaWJ1dGUgTmFtZT0iaHR0cDovL3NjaGVtYXMubWljcm9zb2Z0LmNvbS9pZGVudGl0eS9jbGFpbXMvZGlzcGxheW5hbWUiPjxBdHRyaWJ1dGVWYWx1ZT5maXN4dDE8L0F0dHJpYnV0ZVZhbHVlPjwvQXR0cmlidXRlPjxBdHRyaWJ1dGUgTmFtZT0iaHR0cDovL3NjaGVtYXMubWljcm9zb2Z0LmNvbS9pZGVudGl0eS9jbGFpbXMvaWRlbnRpdHlwcm92aWRlciI+PEF0dHJpYnV0ZVZhbHVlPmh0dHBzOi8vc3RzLndpbmRvd3MubmV0LzY4MmZlYmU4LTAyMWItNGZkZS1hYzA5LWU2MDA4NWYwNTE4MS88L0F0dHJpYnV0ZVZhbHVlPjwvQXR0cmlidXRlPjxBdHRyaWJ1dGUgTmFtZT0iaHR0cDovL3NjaGVtYXMubWljcm9zb2Z0LmNvbS9jbGFpbXMvYXV0aG5tZXRob2RzcmVmZXJlbmNlcyI+PEF0dHJpYnV0ZVZhbHVlPmh0dHA6Ly9zY2hlbWFzLm1pY3Jvc29mdC5jb20vd3MvMjAwOC8wNi9pZGVudGl0eS9hdXRoZW50aWNhdGlvbm1ldGhvZC9wYXNzd29yZDwvQXR0cmlidXRlVmFsdWU+PC9BdHRyaWJ1dGU+PC9BdHRyaWJ1dGVTdGF0ZW1lbnQ+PEF1dGhuU3RhdGVtZW50IEF1dGhuSW5zdGFudD0iMjAxOC0wNC0xNFQwOTo1ODo1NS42MTNaIiBTZXNzaW9uSW5kZXg9Il9jNzljM2VjOC0xYzI2LTQ3NTItOTQ0My0xZjc2ZWI3ZDVkZDYiPjxBdXRobkNvbnRleHQ+PEF1dGhuQ29udGV4dENsYXNzUmVmPnVybjpvYXNpczpuYW1lczp0YzpTQU1MOjIuMDphYzpjbGFzc2VzOlBhc3N3b3JkPC9BdXRobkNvbnRleHRDbGFzc1JlZj48L0F1dGhuQ29udGV4dD48L0F1dGhuU3RhdGVtZW50PjwvQXNzZXJ0aW9uPjxBc3NlcnRpb24gSUQ9Il9jNzljM2VjOC0xYzI2LTQ3NTItOTQ0My0xZjc2ZWI3ZDVkZDYiIElzc3VlSW5zdGFudD0iMjAxOC0wNC0xNFQwOTo1ODo1OC40NDJaIiBWZXJzaW9uPSIyLjAiIHhtbG5zPSJ1cm46b2FzaXM6bmFtZXM6dGM6U0FNTDoyLjA6YXNzZXJ0aW9uIj48SXNzdWVyPmh0dHBzOi8vc3RzLndpbmRvd3MubmV0LzY4MmZlYmU4LTAyMWItNGZkZS1hYzA5LWU2MDA4NWYwNTE4MS88L0lzc3Vlcj48U2lnbmF0dXJlIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwLzA5L3htbGRzaWcjIj48U2lnbmVkSW5mbz48Q2Fub25pY2FsaXphdGlvbk1ldGhvZCBBbGdvcml0aG09Imh0dHA6Ly93d3cudzMub3JnLzIwMDEvMTAveG1sLWV4Yy1jMTRuIyIvPjxTaWduYXR1cmVNZXRob2QgQWxnb3JpdGhtPSJodHRwOi8vd3d3LnczLm9yZy8yMDAxLzA0L3htbGRzaWctbW9yZSNyc2Etc2hhMjU2Ii8+PFJlZmVyZW5jZSBVUkk9IiNfYzc5YzNlYzgtMWMyNi00NzUyLTk0NDMtMWY3NmViN2Q1ZGQ2Ij48VHJhbnNmb3Jtcz48VHJhbnNmb3JtIEFsZ29yaXRobT0iaHR0cDovL3d3dy53My5vcmcvMjAwMC8wOS94bWxkc2lnI2VudmVsb3BlZC1zaWduYXR1cmUiLz48VHJhbnNmb3JtIEFsZ29yaXRobT0iaHR0cDovL3d3dy53My5vcmcvMjAwMS8xMC94bWwtZXhjLWMxNG4jIi8+PC9UcmFuc2Zvcm1zPjxEaWdlc3RNZXRob2QgQWxnb3JpdGhtPSJodHRwOi8vd3d3LnczLm9yZy8yMDAxLzA0L3htbGVuYyNzaGEyNTYiLz48RGlnZXN0VmFsdWU+bGtXblJJSUFGbUhWZVdJWlZYYmFkajFPNGE3Tmc0NHAvY0hCRk04cWFiTT08L0RpZ2VzdFZhbHVlPjwvUmVmZXJlbmNlPjwvU2lnbmVkSW5mbz48U2lnbmF0dXJlVmFsdWU+ZGFnZVpZNXdpZ1ZVSlJ5WDRtRkNnR0w5UGFqNG5VemxOYWhDZ3hKRzJtVTYzRGFMemV2bWp5ZEhMdUxaeEZHcys2bENEOGlvTGM1SkxySnc4UGUrMHd3WFlXSG5MUC9ZTnd5UVI1YjZtWlRlak45RC9wWk5EY1J1VGJCZk14R2xOOFZVTmhpaDc4dFUvbjFCbGJkTmhIakFOZVNsZ09VQ2VZaUhlZXN6REdoRFgyL1FnWno4QkMvcUdrZkE1SUhseVJUckFmRGhOaEY0VGlDUXk3WFppVGoxdnhaUTZkMEFxNUZpaEVzQm1Vb2picjVZbkpKMDZaNnYrNEJLWVVZc2RSRjlGSVZ2VW1EKzNyRk5GUkFCN0EyenpzRHFiTzZRR2Fqblg1REppa1o2a29OYWt4UGwzcmp3b2VFbHBPMENIbmhZdzIwQ3VTT2QydWEraml4dG93PT08L1NpZ25hdHVyZVZhbHVlPjxLZXlJbmZvPjxYNTA5RGF0YT48WDUwOUNlcnRpZmljYXRlPk1JSURCVENDQWUyZ0F3SUJBZ0lRZXY3NkJXcWpXWnhDaG1La0dxb0FmREFOQmdrcWhraUc5dzBCQVFzRkFEQXRNU3N3S1FZRFZRUURFeUpoWTJOdmRXNTBjeTVoWTJObGMzTmpiMjUwY205c0xuZHBibVJ2ZDNNdWJtVjBNQjRYRFRFNE1ESXhPREF3TURBd01Gb1hEVEl3TURJeE9UQXdNREF3TUZvd0xURXJNQ2tHQTFVRUF4TWlZV05qYjNWdWRITXVZV05qWlhOelkyOXVkSEp2YkM1M2FXNWtiM2R6TG01bGREQ0NBU0l3RFFZSktvWklodmNOQVFFQkJRQURnZ0VQQURDQ0FRb0NnZ0VCQU1nbUdpUmZMaDZGZGk5OVhJMlZBM1hLSFN0V05STEV5NUF3L2d4RnhjaG5oMmtQZGsvYmVqRk9zMnN3Y3g3eVVXcXh1ampDTlJzTEJjV2ZhS1VsVG5ya1k3aTl4OW5vWmxNcmlqZ0p5L0xrK0hINUhYMjRQUUNEZit0d2puSEh4WjlHNi84VkxNMmU1WkJlWm0rdDdNM3ZodXVtRUhHM1V3bG9MRjZjVWV1UGRXK2V4bk9CMVUxZkhCSUZPRzhuczRTU0lvcTZ6dzVyZHQwQ1NJNitsN2IxREVqVnZQTHRKRit6eWpsSjFRcDdOZ0J2QXdkaVBpUk1VNGw4SVJWYnVTVktvS1lKb3lKNEwzZVhzamN6b0JTVEo2VmpWMm15Z3o5NkRDNzBNWTNhdmNjRnJrN3RDRUM2WmxNUkJmWTFYUEx5bGRUN3RzUjNFdXpqZWNTYTFNOENBd0VBQWFNaE1COHdIUVlEVlIwT0JCWUVGSWtzMXNyaXhqcFNMWGVpUjh6RVM1Y1RZNmZCTUEwR0NTcUdTSWIzRFFFQkN3VUFBNElCQVFDS3RoZks0QzMxRE11RHlRWlZTM0Y3KzRFdmxkM2hqaXdxdTJ1R0RLK3FGWmFzL0QvZUR1bnhzRnBpd3FDMDFSSU1GRk44eXZtTWpIcGhMSGlCSFd4Y0JUUyt0bTdBaG1BdldNZHhPNWx6SkxTK1VXQXlQRjVJQ1JPZThNdTlpTkppTzVKbENvMFdwdWk5UmJCMUM4MVhoYXgxZ1dISzI0NUVTTDZrN1lXdnlNWVdyR3FyMU51UWNOUzBCL0FJVDFOc2oxV1k3ZWZNSlFPbW5NSGtQVVRXcnlWWmx0aGlqWXlkN1AyR3o2clk1YTgxREFGcWhETkpsMnBHSUFFNkhXdFN6ZVVFaDNqQ3NIRWtvZ2xLZm00VnJHSkV1WGNBTG1mQ01iZGZUdnR1NHJsc2FQMmhRYWQrTUcvS0pGbGVub1RLMzRFTUhlQlBEQ3BxTkR6OFVWTms8L1g1MDlDZXJ0aWZpY2F0ZT48L1g1MDlEYXRhPjwvS2V5SW5mbz48L1NpZ25hdHVyZT48U3ViamVjdD48TmFtZUlEIEZvcm1hdD0idXJuOm9hc2lzOm5hbWVzOnRjOlNBTUw6Mi4wOm5hbWVpZC1mb3JtYXQ6cGVyc2lzdGVudCI+eEp4ZHFTOFcyVVhhd2JaWnFwR0ZYS0c0dUVtTzVHamlqS0QyUmtNaXBCbzwvTmFtZUlEPjxTdWJqZWN0Q29uZmlybWF0aW9uIE1ldGhvZD0idXJuOm9hc2lzOm5hbWVzOnRjOlNBTUw6Mi4wOmNtOmJlYXJlciI+PFN1YmplY3RDb25maXJtYXRpb25EYXRhIEluUmVzcG9uc2VUbz0iaWRjZjIyOTlhYzU1MWI0MmYxYWE5Yjg4ODA0ZWQzMDhjMiIgTm90T25PckFmdGVyPSIyMDE4LTA0LTE0VDEwOjAzOjU4LjQ0MloiIFJlY2lwaWVudD0iaHR0cHM6Ly96YjIuemVyb2J1enoubmV0OjYwNDQzL2F1dGhyZXNwIi8+PC9TdWJqZWN0Q29uZmlybWF0aW9uPjwvU3ViamVjdD48Q29uZGl0aW9ucyBOb3RCZWZvcmU9IjIwMTgtMDQtMTRUMDk6NTM6NTguNDQyWiIgTm90T25PckFmdGVyPSIyMDE4LTA0LTE0VDEwOjUzOjU4LjQ0MloiPjxBdWRpZW5jZVJlc3RyaWN0aW9uPjxBdWRpZW5jZT5odHRwczovL3piMi56ZXJvYnV6ei5uZXQ6NjA0NDMvYXV0aHJlc3A8L0F1ZGllbmNlPjwvQXVkaWVuY2VSZXN0cmljdGlvbj48L0NvbmRpdGlvbnM+PEF0dHJpYnV0ZVN0YXRlbWVudD48QXR0cmlidXRlIE5hbWU9Imh0dHA6Ly9zY2hlbWFzLm1pY3Jvc29mdC5jb20vaWRlbnRpdHkvY2xhaW1zL3RlbmFudGlkIj48QXR0cmlidXRlVmFsdWU+NjgyZmViZTgtMDIxYi00ZmRlLWFjMDktZTYwMDg1ZjA1MTgxPC9BdHRyaWJ1dGVWYWx1ZT48L0F0dHJpYnV0ZT48QXR0cmlidXRlIE5hbWU9Imh0dHA6Ly9zY2hlbWFzLm1pY3Jvc29mdC5jb20vaWRlbnRpdHkvY2xhaW1zL29iamVjdGlkZW50aWZpZXIiPjxBdHRyaWJ1dGVWYWx1ZT5jY2ZiMzc4OC04MjQxLTRhZmUtODg5Ny1mMzEzZjM1ZjllMzc8L0F0dHJpYnV0ZVZhbHVlPjwvQXR0cmlidXRlPjxBdHRyaWJ1dGUgTmFtZT0iaHR0cDovL3NjaGVtYXMueG1sc29hcC5vcmcvd3MvMjAwNS8wNS9pZGVudGl0eS9jbGFpbXMvbmFtZSI+PEF0dHJpYnV0ZVZhbHVlPmZpc3h0MUBhenVyZXdpcmUub25taWNyb3NvZnQuY29tPC9BdHRyaWJ1dGVWYWx1ZT48L0F0dHJpYnV0ZT48QXR0cmlidXRlIE5hbWU9Imh0dHA6Ly9zY2hlbWFzLm1pY3Jvc29mdC5jb20vaWRlbnRpdHkvY2xhaW1zL2Rpc3BsYXluYW1lIj48QXR0cmlidXRlVmFsdWU+ZmlzeHQxPC9BdHRyaWJ1dGVWYWx1ZT48L0F0dHJpYnV0ZT48QXR0cmlidXRlIE5hbWU9Imh0dHA6Ly9zY2hlbWFzLm1pY3Jvc29mdC5jb20vaWRlbnRpdHkvY2xhaW1zL2lkZW50aXR5cHJvdmlkZXIiPjxBdHRyaWJ1dGVWYWx1ZT5odHRwczovL3N0cy53aW5kb3dzLm5ldC82ODJmZWJlOC0wMjFiLTRmZGUtYWMwOS1lNjAwODVmMDUxODEvPC9BdHRyaWJ1dGVWYWx1ZT48L0F0dHJpYnV0ZT48QXR0cmlidXRlIE5hbWU9Imh0dHA6Ly9zY2hlbWFzLm1pY3Jvc29mdC5jb20vY2xhaW1zL2F1dGhubWV0aG9kc3JlZmVyZW5jZXMiPjxBdHRyaWJ1dGVWYWx1ZT5odHRwOi8vc2NoZW1hcy5taWNyb3NvZnQuY29tL3dzLzIwMDgvMDYvaWRlbnRpdHkvYXV0aGVudGljYXRpb25tZXRob2QvcGFzc3dvcmQ8L0F0dHJpYnV0ZVZhbHVlPjwvQXR0cmlidXRlPjwvQXR0cmlidXRlU3RhdGVtZW50PjxBdXRoblN0YXRlbWVudCBBdXRobkluc3RhbnQ9IjIwMTgtMDQtMTRUMDk6NTg6NTUuNjEzWiIgU2Vzc2lvbkluZGV4PSJfYzc5YzNlYzgtMWMyNi00NzUyLTk0NDMtMWY3NmViN2Q1ZGQ2Ij48QXV0aG5Db250ZXh0PjxBdXRobkNvbnRleHRDbGFzc1JlZj51cm46b2FzaXM6bmFtZXM6dGM6U0FNTDoyLjA6YWM6Y2xhc3NlczpQYXNzd29yZDwvQXV0aG5Db250ZXh0Q2xhc3NSZWY+PC9BdXRobkNvbnRleHQ+PC9BdXRoblN0YXRlbWVudD48L0Fzc2VydGlvbj48L3NhbWxwOlJlc3BvbnNlPg=="
          "_c79c3ec8-1c26-4752-9443-1f76eb7d5dd6"
          isLeft
      ]

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SAML2.WebSSO.Test.Util.Misc where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Base64.Lazy as EL (encode)
import Data.EitherR
import Data.Generics.Uniplate.Data
import Data.List (sort)
import Data.String
import Data.String.Conversions
import qualified Data.Text.Lazy.IO as LT
import Data.Typeable
import Data.UUID as UUID
import GHC.Stack
import SAML2.WebSSO
import Servant
import Shelly (run, setStdin, shelly, silently)
import System.Directory (doesFileExist)
import System.FilePath
import System.FilePath.TH
import System.IO.Temp
import System.Process (system)
import Test.Hspec
import Text.Show.Pretty
import Text.XML as XML

-- | pipe the output of `curl https://.../initiate-login/...` into this to take a look.
readAuthReq :: String -> IO ()
readAuthReq raw = do
  print $ mimeUnrender @HTML @(FormRedirect Document) Proxy (cs raw)

render' :: Document -> LT
render' =
  renderText $
    def
      { rsPretty = True
      --  , rsNamespaces :: [(Text, Text)]
      --  , rsAttrOrder :: Name -> Map.Map Name Text -> [(Name, Text)]
      --  , rsUseCDATA :: Content -> Bool
      --  , rsXMLDeclaration :: Bool
      }

rerender' :: LT -> LT
rerender' = render' . parseText_ def

showFile :: FilePath -> IO String
showFile fp = cs . rerender' . cs . dropWhile (/= '<') <$> Prelude.readFile fp

dumpFile :: FilePath -> IO ()
dumpFile = showFile >=> putStrLn

rerenderFile :: FilePath -> IO ()
rerenderFile fp = showFile fp >>= Prelude.writeFile (fp <> "-")

hedgehog :: IO Bool -> Spec
hedgehog = it "hedgehog tests" . (`shouldReturn` True)

readSampleIO :: (MonadIO m) => FilePath -> m LT
readSampleIO fpath =
  liftIO $
    LT.readFile $
      $(fileRelativeToProject "test/samples") </> fpath

doesSampleExistIO :: (MonadIO m) => FilePath -> m Bool
doesSampleExistIO fpath =
  liftIO $
    doesFileExist $
      $(fileRelativeToProject "test/samples") </> fpath

roundtrip :: forall a. (Eq a, Show a, HasXMLRoot a) => Int -> IO LT -> a -> Spec
roundtrip serial mkrendered parsed = describe ("roundtrip-" <> show serial) $ do
  let tweak = fmapL show . parseText def
  it "encode" $ do
    rendered <- mkrendered
    tweak rendered `assertXmlRoundtrip` tweak (encode parsed)
  it "decode" $ do
    rendered <- mkrendered
    Right parsed `shouldBe` fmapL show (decode rendered)

-- | If we get two XML structures that differ, compute the diff.
assertXmlRoundtrip ::
  (HasCallStack) =>
  Either String Document ->
  Either String Document ->
  Expectation
assertXmlRoundtrip (Right (normalizeDocument -> x)) (Right (normalizeDocument -> y)) =
  assertXmlRoundtripFailWithDiff x y
assertXmlRoundtrip x y =
  x `shouldBe` y

assertXmlRoundtripFailWithDiff ::
  (HasCallStack) =>
  Document ->
  Document ->
  Expectation
assertXmlRoundtripFailWithDiff x y = unless (x == y)
  . withSystemTempDirectory "saml.web.sso.tmp"
  $ \tmpdir -> do
    let tmpx = tmpdir <> "/x"
        tmpy = tmpdir <> "/y"
        tmpd = tmpdir <> "/xy"
    x `seq` Prelude.writeFile tmpx (ppShow x)
    y `seq` Prelude.writeFile tmpy (ppShow y)
    _ <- system $ "diff " <> tmpx <> " " <> tmpy <> " > " <> tmpd
    diff <- Prelude.readFile tmpd
    expectationFailure ("non-empty diff:\n" <> diff <> "\n\nyour output:\n" <> ppShow y)

-- | Make two 'Document' values that are supposed to be equal easier to compare:
--
-- * render and parse back to normalize the locations where namespaces are declared
-- * sort all children and remove digital signatures
-- * remove all namespace prefices
normalizeDocument :: (HasCallStack) => Document -> Document
normalizeDocument =
  renderAndParse
    . transformBis
      [ [transformer $ \(Name nm nmspace _prefix) -> Name nm nmspace Nothing],
        [transformer $ \(Element nm attrs nodes) -> Element nm attrs (sort . filter (not . isSignature) $ nodes)]
      ]

renderAndParse :: (HasCallStack) => Document -> Document
renderAndParse doc = case parseText def $ renderText def {rsPretty = True} doc of
  Right doc' -> doc'
  bad@(Left _) -> error $ "impossible: " <> show bad

isSignature :: Node -> Bool
isSignature (NodeElement (Element name _ _)) = name == "{http://www.w3.org/2000/09/xmldsig#}Signature"
isSignature _ = False

----------------------------------------------------------------------
-- helpers

passes :: (MonadIO m) => m ()
passes = liftIO $ True `shouldBe` True

newtype SomeSAMLRequest = SomeSAMLRequest {fromSomeSAMLRequest :: XML.Document}
  deriving (Eq, Show)

instance HasFormRedirect SomeSAMLRequest where
  formRedirectFieldName _ = "SAMLRequest"

instance HasXML SomeSAMLRequest where
  nameSpaces Proxy = []
  parse = fmap SomeSAMLRequest . parse

instance HasXMLRoot SomeSAMLRequest where
  renderRoot (SomeSAMLRequest doc) = renderRoot doc

base64ours, base64theirs :: (HasCallStack) => SBS -> IO SBS
base64ours = pure . cs . EL.encode . cs
base64theirs sbs = shelly . silently $ cs <$> (setStdin (cs sbs) >> run "/usr/bin/env" ["base64", "--wrap", "0"])

----------------------------------------------------------------------
-- orphans

instance IsString IdPId where
  fromString piece = maybe (error $ "no valid UUID" <> piece) (IdPId) . UUID.fromString $ piece

{-# LANGUAGE OverloadedStrings #-}

module Text.XML.Util where

import Control.Monad.Except
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.UTF8 qualified as BSLUTF8
import Data.Char (isSpace)
import Data.Generics.Uniplate.Data qualified as Uniplate
import Data.Kind (Type)
import Data.Map as Map
import Data.Proxy
import Data.String.Conversions
import Data.Text qualified as ST
import Data.Tree.NTree.TypeDefs qualified as HXT
import Data.Typeable
import GHC.Stack
import SAML2.XML qualified as HS
import Text.XML
import Text.XML.HXT.Arrow.Pickle.Xml qualified as XP
import Text.XML.HXT.Core qualified as HXT

die :: forall (a :: Type) b c m. (HasCallStack, Typeable a, Show b, MonadError String m) => Proxy a -> b -> m c
die = die' Nothing

die' :: forall (a :: Type) b c m. (HasCallStack, Typeable a, Show b, MonadError String m) => Maybe String -> Proxy a -> b -> m c
die' mextra Proxy msg =
  throwError $
    "HasXML: could not parse " <> show (typeOf @a undefined) <> ": " <> show msg <> maybe "" ("; " <>) mextra

type Attrs = Map.Map Name ST

elemToNodes :: (HasCallStack) => Element -> [Node]
elemToNodes = (: []) . NodeElement

nodesToElem :: (HasCallStack) => [Node] -> Element
nodesToElem [NodeElement el] = el
nodesToElem bad = error $ show bad

docToNodes :: (HasCallStack) => Document -> [Node]
docToNodes (Document _ el _) = elemToNodes el

mkDocument :: Element -> Document
mkDocument el = Document defPrologue el defMiscellaneous

defPrologue :: Prologue
defPrologue = Prologue [] Nothing []

defMiscellaneous :: [Miscellaneous]
defMiscellaneous = []

hxtToConduit :: (MonadError String m) => HXT.XmlTree -> m Document
hxtToConduit = either (throwError . ("hxtToConduit: parseLBS failed: " <>) . show) pure . parseLBS def . ourDocToXMLWithRoot

conduitToHxt :: (MonadError String m) => Document -> m HXT.XmlTree
conduitToHxt = either (throwError . ("conduitToHxt: xmlToDoc' failed: " <>)) pure . xmlToDoc' . renderLBS def {rsXMLDeclaration = False}

samlToConduit :: (MonadError String m, HXT.XmlPickler a) => a -> m Document
samlToConduit = either (throwError . ("samlToConduit: parseLBS failed: " <>) . show) pure . parseLBS def . ourSamlToXML

ourSamlToXML :: (XP.XmlPickler a) => a -> BSL.ByteString
ourSamlToXML = ourDocToXMLWithoutRoot . HS.samlToDoc

-- | Direct usage of `xshowBlob` breaks non-Latin-1 encodings (e.g. UTF-8,
-- Unicode)! This helper function works around these issues.
ourDocToXMLWithoutRoot :: (HasCallStack) => HXT.XmlTree -> BSL.ByteString
ourDocToXMLWithoutRoot t = case HXT.runLA (HXT.writeDocumentToString []) t of
  [xmlContent] -> BSLUTF8.fromString xmlContent
  other -> error $ "Expected one element. Got: " ++ show other

-- | Direct usage of `xshowBlob` breaks non-Latin-1 encodings (e.g. UTF-8,
-- Unicode)! This helper function works around these issues.
ourDocToXMLWithRoot :: HXT.XmlTree -> BSL.ByteString
ourDocToXMLWithRoot t = ourDocToXMLWithoutRoot $ HXT.NTree (HXT.XText "throw-me-away") [t]

-- | This is subtly different from HS.xmlToDoc' and should probably be moved to hsaml2.
xmlToDoc' :: (MonadError String m) => BSL.ByteString -> m HXT.XmlTree
xmlToDoc' xml = case HXT.runLA HXT.xread (cs xml) of
  [HXT.NTree (HXT.XError _errcode errmsg) _] -> throwError errmsg
  [t] -> pure t
  [] -> throwError "no root elements"
  bad@(_ : _ : _) -> throwError $ "more than one root element: " <> show (length bad)

-- | Remove all whitespace in the text nodes of the xml document.
stripWhitespace :: Document -> Document
stripWhitespace =
  Uniplate.transformBis
    [ [ Uniplate.transformer $ \case
          (NodeContent txt) -> NodeContent $ ST.filter (not . isSpace) txt
          other -> other
      ],
      [ Uniplate.transformer $ \case
          (Element nm attrs nodes) -> Element nm attrs (Prelude.filter (/= NodeContent "") $ nodes)
      ]
    ]

-- | if two content nodes are next to each other, concatenate them into one.  NB: if you call
-- 'stripWhitespace' it should be called *after* 'mergeContentSiblings', or some two words will be
-- merged into one.
mergeContentSiblings :: Document -> Document
mergeContentSiblings =
  Uniplate.transformBis
    [ [ Uniplate.transformer $ \case
          (Element nm attrs nodes) -> Element nm attrs (go nodes)
      ]
    ]
  where
    go [] = []
    go (NodeContent s : NodeContent t : xs) = go $ NodeContent (s <> t) : xs
    go (x : xs) = x : go xs

normalizeDoc :: Document -> Document
normalizeDoc = stripWhitespace . mergeContentSiblings

-- | https://github.com/snoyberg/xml/issues/137
repairNamespaces :: (HasCallStack) => [Node] -> [Node]
repairNamespaces = fmap $ \case
  NodeElement el -> NodeElement $ repairNamespacesEl el
  other -> other

-- | https://github.com/snoyberg/xml/issues/137
repairNamespacesEl :: (HasCallStack) => Element -> Element
repairNamespacesEl el = unwrap . parseText def . renderText def . mkDocument $ el
  where
    unwrap (Right (Document _ el' _)) = el'
    unwrap (Left msg) = error $ show msg

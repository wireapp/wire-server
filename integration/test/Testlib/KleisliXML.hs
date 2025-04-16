-- | Helpers to apply the Kleisli operator (`>=>`) on XML (xml-conduit)
-- structures.
module Testlib.KleisliXML where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Testlib.Prelude
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML

findElement :: XML.Name -> XML.Cursor -> Maybe XML.Cursor
findElement name = listToMaybe . (XML.$// XML.element name)

getAttribute :: XML.Name -> XML.Cursor -> Maybe T.Text
getAttribute name = listToMaybe . (XML.$| XML.attribute name)

getContent :: XML.Cursor -> Maybe T.Text
getContent = listToMaybe . (XML.$// XML.content)

parseXml :: LT.Text -> XML.Cursor
parseXml = XML.fromDocument . XML.parseText_ XML.def

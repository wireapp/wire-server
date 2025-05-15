{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- FUTUREWORK: consider using http://hackage.haskell.org/package/xml-conduit-parse

module SAML2.WebSSO.XML.Hack where

import Data.AssocList
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.UTF8 qualified as LBSUTF8
import Data.Maybe
import Data.Tree.Class
import Data.Tree.NTree.TypeDefs
import Text.Regex.XMLSchema.Generic (sed)
import Text.XML.HXT.Core qualified as HXT
import Text.XML.HXT.DOM.QualifiedName
import Text.XML.HXT.DOM.TypeDefs hiding (stringToBlob)
import Text.XML.HXT.DOM.XmlKeywords
import Text.XML.HXT.DOM.XmlNode (getDTDAttrl, mkDTDElem)
import Prelude hiding (showChar, showString)

docToXMLWithRoot :: HXT.XmlTree -> BSL.ByteString
docToXMLWithRoot = xshowBlob . (: [])

-- | this is called in `xshowBlob` below and used to come from "Text.XML.HXT.DOM.TypeDefs".
-- the original in hxt used Data.ByteString.Lazy.Char8.pack for rendering XText, which caused
-- some valid unicode characters to get corrupted.
stringToBlob :: String -> BSL.ByteString
stringToBlob = LBSUTF8.fromString

----------------------------------------------------------------------
-- [copied verbatim from hxt, thanks uwe!!  :)]
--
--   Module     : Text.XML.HXT.DOM.ShowXml
--   Copyright  : Copyright (C) 2008-9 Uwe Schmidt
--   License    : MIT
--
--   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
--   Stability  : stable
--   Portability: portable
--
--   XML tree conversion to external string representation

-- -----------------------------------------------------------------------------
--
-- the toString conversion functions

-- |
-- convert a list of trees into a string
--
-- see also : 'xmlTreesToText' for filter version, 'Text.XML.HXT.Parser.XmlParsec.xread' for the inverse operation
xshow :: XmlTrees -> String
xshow [(NTree (XText s) _)] = s -- special case optimisation
xshow [(NTree (XBlob b) _)] = blobToString b -- special case optimisation
xshow ts = showXmlTrees showString showString ts ""

-- | convert an XML tree into a binary large object (a bytestring)
xshowBlob :: XmlTrees -> Blob
xshowBlob [(NTree (XBlob b) _)] = b -- special case optimisation
xshowBlob [(NTree (XText s) _)] = stringToBlob s -- special case optimisation
xshowBlob ts = stringToBlob $ xshow ts

-- |
-- convert a list of trees into a blob.
--
-- Apply a quoting function for XML quoting of content,
-- a 2. quoting funtion for attribute values
-- and an encoding function after tree conversion
xshow' ::
  (Char -> StringFct) ->
  (Char -> StringFct) ->
  (Char -> StringFct) ->
  XmlTrees ->
  Blob
xshow' cquot aquot enc ts = stringToBlob $ (concatMap' enc (showTrees ts "")) ""
  where
    showTrees = showXmlTrees (concatMap' cquot) (concatMap' aquot)

xshow'' ::
  (Char -> StringFct) ->
  (Char -> StringFct) ->
  XmlTrees ->
  String
xshow'' cquot aquot ts = showTrees ts ""
  where
    showTrees = showXmlTrees (concatMap' cquot) (concatMap' aquot)

-- ------------------------------------------------------------

type StringFct = String -> String

-- ------------------------------------------------------------

showXmlTrees ::
  (String -> StringFct) ->
  (String -> StringFct) ->
  XmlTrees ->
  StringFct
showXmlTrees cf af =
  showTrees
  where
    -- ------------------------------------------------------------

    showTrees :: XmlTrees -> StringFct
    showTrees = foldr (.) id . map showXmlTree
    {-# INLINE showTrees #-}

    showTrees' :: XmlTrees -> StringFct
    showTrees' = foldr (\x y -> x . showNL . y) id . map showXmlTree
    {-# INLINE showTrees' #-}

    -- ------------------------------------------------------------

    showXmlTree :: XmlTree -> StringFct
    showXmlTree (NTree (XText s) _) -- common cases first
      =
      cf s
    showXmlTree (NTree (XTag t al) []) =
      showLt . showQName t . showTrees al . showSlash . showGt
    showXmlTree (NTree (XTag t al) cs) =
      showLt
        . showQName t
        . showTrees al
        . showGt
        . showTrees cs
        . showLt
        . showSlash
        . showQName t
        . showGt
    showXmlTree (NTree (XAttr an) cs) =
      showBlank
        . showQName an
        . showEq
        . showQuot
        . af (xshow cs)
        . showQuot
    showXmlTree (NTree (XBlob b) _) =
      cf . blobToString $ b
    showXmlTree (NTree (XCharRef i) _) =
      showString "&#" . showString (show i) . showChar ';'
    showXmlTree (NTree (XEntityRef r) _) =
      showString "&" . showString r . showChar ';'
    showXmlTree (NTree (XCmt c) _) =
      showString "<!--" . showString c . showString "-->"
    showXmlTree (NTree (XCdata d) _) =
      showString "<![CDATA[" . showString d' . showString "]]>"
      where
        -- quote "]]>" in CDATA contents
        d' = sed (const "]]&gt;") "\\]\\]>" d
    showXmlTree (NTree (XPi n al) _) =
      showString "<?"
        . showQName n
        . (foldr (.) id . map showPiAttr) al
        . showString "?>"
      where
        showPiAttr :: XmlTree -> StringFct
        showPiAttr a@(NTree (XAttr an) cs)
          | qualifiedName an == a_value =
              -- <?some-pi ... ?>
              -- no XML quoting of PI value
              showBlank . showXmlTrees showString showString cs
          | otherwise =
              -- <?xml version="..." ... ?>
              showXmlTree a
        showPiAttr a =
          showXmlTree a -- id
    showXmlTree (NTree (XDTD de al) cs) =
      showXmlDTD de al cs
    showXmlTree (NTree (XError l e) _) =
      showString "<!-- ERROR ("
        . shows l
        . showString "):\n"
        . showString e
        . showString "\n-->"

    -- ------------------------------------------------------------

    showXmlDTD :: DTDElem -> Attributes -> XmlTrees -> StringFct

    showXmlDTD DOCTYPE al cs =
      showString "<!DOCTYPE "
        . showAttr a_name al
        . showExternalId al
        . showInternalDTD cs
        . showString ">"
      where
        showInternalDTD [] = id
        showInternalDTD ds =
          showString " [\n"
            . showTrees' ds
            . showChar ']'
    showXmlDTD ELEMENT al cs =
      showString "<!ELEMENT "
        . showAttr a_name al
        . showBlank
        . showElemType (lookup1 a_type al) cs
        . showString " >"
    showXmlDTD ATTLIST al cs =
      showString "<!ATTLIST "
        . ( if isNothing . lookup a_name $ al
              then showTrees cs
              else
                showAttr a_name al
                  . showBlank
                  . ( case lookup a_value al of
                        Nothing ->
                          ( showPEAttr
                              . fromMaybe []
                              . getDTDAttrl
                              . head
                          )
                            cs
                        Just a ->
                          ( showString a
                              . showAttrType (lookup1 a_type al)
                              . showAttrKind (lookup1 a_kind al)
                          )
                    )
          )
        . showString " >"
      where
        showAttrType t
          | t == k_peref =
              showBlank . showPEAttr al
          | t == k_enumeration =
              showAttrEnum
          | t == k_notation =
              showBlank . showString k_notation . showAttrEnum
          | otherwise =
              showBlank . showString t

        showAttrEnum =
          showString " ("
            . foldr1
              (\s1 s2 -> s1 . showString " | " . s2)
              (map (getEnum . fromMaybe [] . getDTDAttrl) cs)
            . showString ")"
          where
            getEnum :: Attributes -> StringFct
            getEnum l = showAttr a_name l . showPEAttr l

        showAttrKind k
          | k == k_default =
              showBlank
                . showQuoteString (lookup1 a_default al)
          | k == k_fixed =
              showBlank
                . showString k_fixed
                . showBlank
                . showQuoteString (lookup1 a_default al)
          | k == "" =
              id
          | otherwise =
              showBlank
                . showString k
    showXmlDTD NOTATION al _cs =
      showString "<!NOTATION "
        . showAttr a_name al
        . showExternalId al
        . showString " >"
    showXmlDTD PENTITY al cs = showEntity "% " al cs
    showXmlDTD ENTITY al cs = showEntity "" al cs
    showXmlDTD PEREF al _cs = showPEAttr al
    showXmlDTD CONDSECT _ (c1 : cs) =
      showString "<![ "
        . showXmlTree c1
        . showString " [\n"
        . showTrees cs
        . showString "]]>"
    showXmlDTD CONTENT al cs = showContent (mkDTDElem CONTENT al cs)
    showXmlDTD NAME al _cs = showAttr a_name al
    showXmlDTD de al _cs =
      showString "NOT YET IMPLEMETED: "
        . showString (show de)
        . showBlank
        . showString (show al)
        . showString " [...]\n"

    -- ------------------------------------------------------------

    showEntity :: String -> Attributes -> XmlTrees -> StringFct
    showEntity kind al cs =
      showString "<!ENTITY "
        . showString kind
        . showAttr a_name al
        . showExternalId al
        . showNData al
        . showEntityValue cs
        . showString " >"

    showEntityValue :: XmlTrees -> StringFct
    showEntityValue [] = id
    showEntityValue cs =
      showBlank
        . showQuot
        . af (xshow cs)
        . showQuot

    -- ------------------------------------------------------------

    showContent :: XmlTree -> StringFct
    showContent (NTree (XDTD de al) cs) =
      cont2String de
      where
        cont2String :: DTDElem -> StringFct
        cont2String NAME = showAttr a_name al
        cont2String PEREF = showPEAttr al
        cont2String CONTENT =
          showLpar
            . foldr1
              (combine (lookup1 a_kind al))
              (map showContent cs)
            . showRpar
            . showAttr a_modifier al
        cont2String n = error ("cont2string " ++ show n ++ " is undefined")
        combine k s1 s2 =
          s1
            . showString
              ( if k == v_seq
                  then ", "
                  else " | "
              )
            . s2
    showContent n = showXmlTree n

    -- ------------------------------------------------------------

    showElemType :: String -> XmlTrees -> StringFct
    showElemType t cs
      | t == v_pcdata = showLpar . showString v_pcdata . showRpar
      | t == v_mixed
          && (not . null) cs =
          showLpar
            . showString v_pcdata
            . ( foldr (.) id
                  . map (mixedContent . selAttrl . getNode)
              )
              cs1
            . showRpar
            . showAttr a_modifier al1
      | t == v_mixed -- incorrect tree, e.g. after erronius pe substitution
        =
          showLpar
            . showRpar
      | t == v_children
          && (not . null) cs =
          showContent (head cs)
      | t == v_children =
          showLpar
            . showRpar
      | t == k_peref =
          foldr (.) id
            . map showContent
            $ cs
      | otherwise = showString t
      where
        [(NTree (XDTD CONTENT al1) cs1)] = cs

        mixedContent :: Attributes -> StringFct
        mixedContent l = showString " | " . showAttr a_name l . showPEAttr l

        selAttrl (XDTD _ as) = as
        selAttrl (XText tex) = [(a_name, tex)]
        selAttrl _ = []

-- ------------------------------------------------------------

showQName :: QName -> StringFct
showQName = qualifiedName'
{-# INLINE showQName #-}

-- ------------------------------------------------------------

showQuoteString :: String -> StringFct
showQuoteString s = showQuot . showString s . showQuot

-- ------------------------------------------------------------

showAttr :: String -> Attributes -> StringFct
showAttr k al = showString (fromMaybe "" . lookup k $ al)

-- ------------------------------------------------------------

showPEAttr :: Attributes -> StringFct
showPEAttr al = showPE (lookup a_peref al)
  where
    showPE (Just pe) =
      showChar '%'
        . showString pe
        . showChar ';'
    showPE Nothing = id

-- ------------------------------------------------------------

showExternalId :: Attributes -> StringFct
showExternalId al = id2Str (lookup k_system al) (lookup k_public al)
  where
    id2Str Nothing Nothing = id
    id2Str (Just s) Nothing =
      showBlank
        . showString k_system
        . showBlank
        . showQuoteString s
    id2Str Nothing (Just p) =
      showBlank
        . showString k_public
        . showBlank
        . showQuoteString p
    id2Str (Just s) (Just p) =
      showBlank
        . showString k_public
        . showBlank
        . showQuoteString p
        . showBlank
        . showQuoteString s

-- ------------------------------------------------------------

showNData :: Attributes -> StringFct
showNData al = nd2Str (lookup k_ndata al)
  where
    nd2Str Nothing = id
    nd2Str (Just v) =
      showBlank
        . showString k_ndata
        . showBlank
        . showString v

-- ------------------------------------------------------------

showBlank,
  showEq,
  showLt,
  showGt,
  showSlash,
  showQuot,
  showLpar,
  showRpar,
  showNL ::
    StringFct
showBlank = showChar ' '
{-# INLINE showBlank #-}
showEq = showChar '='
{-# INLINE showEq #-}
showLt = showChar '<'
{-# INLINE showLt #-}
showGt = showChar '>'
{-# INLINE showGt #-}
showSlash = showChar '/'
{-# INLINE showSlash #-}
showQuot = showChar '\"'
{-# INLINE showQuot #-}
showLpar = showChar '('
{-# INLINE showLpar #-}
showRpar = showChar ')'
{-# INLINE showRpar #-}
showNL = showChar '\n'
{-# INLINE showNL #-}

showChar :: Char -> StringFct
showChar = (:)
{-# INLINE showChar #-}

showString :: String -> StringFct
showString = (++)
{-# INLINE showString #-}

concatMap' :: (Char -> StringFct) -> String -> StringFct
concatMap' f = foldr (\x r -> f x . r) id
{-# INLINE concatMap' #-}

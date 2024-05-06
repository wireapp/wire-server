{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module FindTests.Parse
  ( FoundTestCase (..),
    parseTestCases,
  )
where

import Data.Aeson
import Data.List.Extra
import Data.Map qualified as Map
import GHC
import GHC.Driver.Ppr
import GHC.Types.Name.Occurrence
import GHC.Utils.Outputable
import Imports as I

----------------------------------------------------------------------
-- types

data FoundTestCase = FoundTestCase
  { testCaseFileName :: FilePath, -- the file containing the haskell module
    testCaseName :: String, -- the haskell identifier of the function
    testCaseDocs :: Maybe SrcSpan, -- comments associated with this function
    testCaseSigD :: Maybe SrcSpan, -- type signature
    testCaseValD :: Maybe SrcSpan -- value declaration
  }
  deriving (Eq, Show)

instance ToJSON FoundTestCase where
  toJSON (FoundTestCase {..}) =
    object
      [ "filename" .= testCaseFileName,
        "name" .= testCaseName,
        "docs" .= (srcSpanToJSON <$> testCaseDocs),
        "sigd" .= (srcSpanToJSON <$> testCaseSigD),
        "vald" .= (srcSpanToJSON <$> testCaseValD)
      ]

srcSpanToJSON :: SrcSpan -> Value
srcSpanToJSON (RealSrcSpan real _) =
  object
    [ "loc"
        .= object
          [ "startLine" .= srcSpanStartLine real,
            "endLine" .= srcSpanEndLine real,
            "startCol" .= srcSpanStartCol real,
            "endCol" .= srcSpanEndCol real
          ]
    ]
srcSpanToJSON bad@(UnhelpfulSpan _) =
  object ["noloc" .= show bad]

----------------------------------------------------------------------
-- find

-- | Get the mapping of top-level decls including exactly what we need (more or less).
parseTestCases :: DynFlags -> FilePath -> ParsedSource -> Map String FoundTestCase
parseTestCases dflags filename hsmod = I.foldl' addDecl mempty (findDecls dflags hsmod)
  where
    addDecl :: Map String FoundTestCase -> (WhatDecl String, SrcSpan) -> Map String FoundTestCase
    addDecl mapping (WhatDeclT name, loc) =
      Map.alter (Just . maybe (FoundTestCase filename name Nothing (Just loc) Nothing) (\tc -> tc {testCaseSigD = Just loc})) name mapping
    addDecl mapping (WhatDeclF name, loc) =
      Map.alter (Just . maybe (FoundTestCase filename name Nothing Nothing (Just loc)) (\tc -> tc {testCaseValD = Just loc})) name mapping

----------------------------------------------------------------------
-- decls

data WhatDecl a = WhatDeclT a | WhatDeclF a

findDecls :: DynFlags -> ParsedSource -> [(WhatDecl String, SrcSpan)]
findDecls dflags = join . map q . hsmodDecls . unLoc
  where
    q (L loc (ValD _ (FunBind _ i _ _))) = [(WhatDeclF $ s $ unLoc i, locA loc)]
    q (L loc (SigD _ (TypeSig _ [i] _))) = [(WhatDeclT $ s $ unLoc i, locA loc)]
    q (L loc (SigD _ (TypeSig _ (_ : _ : _) _))) = error $ "multiple TypeSig nodes found: " ++ showSDoc dflags (ppr loc) -- this can be ignored, ie., return [].  just adding this error here out of curiosity.
    q _ = []

    s = showSDoc dflags . pprOccName . h

    h (Unqual o) = o
    h (Qual _ o) = o
    h bad = error $ "unexpected RdrName: " I.<> s bad

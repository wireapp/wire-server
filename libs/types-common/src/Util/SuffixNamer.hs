module Util.SuffixNamer where

import Control.Lens
import Imports
import Language.Haskell.TH (mkName, nameBase)

suffixNamer :: FieldNamer
suffixNamer _ _ n =
  let name = nameBase n
   in [TopName (mkName (name <> "Lens"))]

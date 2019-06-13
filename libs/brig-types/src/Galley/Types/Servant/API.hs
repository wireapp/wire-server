{-# OPTIONS_GHC -Wno-orphans #-}
-- FUTUREWORK: move the 'ToSchema' instances to their home modules (where the data types
-- live), and turn warning about orphans back on.

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- it's ok to not warn about unmatched patterns; 'validateEveryToJSON' will crash on them
-- before too long.

-- | Import this qualified.
--
-- NOTE: this is in brig-types because of cyclical import deps between brig-types and
-- galley-types.
--
-- TODO: mv /libs/brig-types to /libs/brig-galley-types, mv the modules from galley-types into
-- it under exactly the same module names (and paths), and remove libs/galley-types.
module Galley.Types.Servant.API where

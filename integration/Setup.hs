import Data.Foldable
import qualified Data.Map as Map
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Types.UnqualComponentName
import System.FilePath
import System.Directory

testHooks :: UserHooks -> UserHooks
testHooks hooks =
  hooks
    { buildHook = \p l h f -> generate p l >> buildHook hooks p l h f,
      haddockHook = \p l h f -> generate p l >> haddockHook hooks p l h f,
      replHook = \p l h f args -> generate p l >> replHook hooks p l h f args
    }
  where
    generate p l = do
      let cname = CExeName (mkUnqualComponentName "integration-main")
      for_ (Map.lookup cname (componentNameMap l)) $ \compBIs ->
        for_ compBIs $ \compBI -> do
          let dest = autogenComponentModulesDir l compBI </> "Moo.hs"
          createDirectoryIfMissing True (takeDirectory dest)
          writeFile
            dest
            "\n\
            \module Moo where\n\
            \import Prelude\n\
            \x :: Int\n\
            \x = 3\n"
          pure ()

main :: IO ()
main = do
  defaultMainWithHooks (testHooks simpleUserHooks)

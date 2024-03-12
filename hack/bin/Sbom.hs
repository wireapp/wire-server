{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Sbom where

import Control.Arrow ((&&&))
import Data.Aeson
import Data.Aeson.Key qualified as KM
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (typeMismatch)
import Data.Bifunctor (first)
import Data.Bitraversable (bitraverse)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as C8L
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.Functor.Identity
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time.Clock.POSIX
import Data.Traversable (for)
import Data.Tree
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as V4
import Debug.Trace
import GHC.Generics hiding (Meta)
import GHC.IsList (IsList (fromList, toList))
import Numeric.Natural (Natural)
import Options.Applicative (customExecParser, fullDesc, help, long, prefs, progDesc, showHelpOnEmpty, strOption, value)
import Options.Applicative qualified as Opt
import System.Directory
import System.Process

data License = MkLicense
  { id :: Maybe Text,
    name :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

sadSbomMeta :: Text -> Text -> [Text] -> SBomMeta Identity
sadSbomMeta drvPath outPath directDeps =
  MkSBomMeta
    { drvPath = drvPath,
      outPath = Identity outPath,
      directDeps = Identity directDeps,
      description = Nothing,
      homepage = Nothing,
      licenseSpdxId = [],
      name = Nothing,
      typ = Nothing,
      urls = [],
      version = Nothing
    }

data SBomMeta f = MkSBomMeta
  { drvPath :: Text,
    description :: Maybe Text,
    homepage :: Maybe Text,
    licenseSpdxId :: [Maybe License],
    name :: Maybe Text,
    typ :: Maybe Text,
    urls :: [Maybe Text],
    version :: Maybe Text,
    outPath :: f Text,
    directDeps :: f [Text]
  }

deriving stock instance (Eq (f [Text]), Eq (f Text)) => Eq (SBomMeta f)

deriving stock instance (Ord (f [Text]), Ord (f Text)) => Ord (SBomMeta f)

deriving stock instance (Show (f [Text]), Show (f Text)) => Show (SBomMeta f)

type Meta = SBomMeta Proxy

instance FromJSON Meta where
  parseJSON (Object val) =
    MkSBomMeta
      <$> val
      .: "drvPath"
      <*> val
      .: "description"
      <*> val
      .: "homepage"
      <*> val
      .: "licenseSpdxId"
      <*> val
      .: "name"
      <*> val
      .: "type"
      <*> val
      .: "urls"
      <*> val
      .: "version"
      <*> pure Proxy
      <*> pure Proxy
  parseJSON invalid = typeMismatch "Object" invalid

type SBom = Map Text (SBomMeta Identity)

type MetaDB = Map Text (SBomMeta Proxy)

type ClosureInfo = Tree ByteString

type PathInfo = [(Text, (Text, [Text]))]

data Visit a = Seen a | Unseen a
  deriving stock (Eq, Ord, Show)

data SerializeSBom = MkSerializeSBom
  { -- | the version of the SBom; this is version of the old SBom + 1
    sbom'version :: Natural,
    -- | name of the component the SBom is generated for
    sbom'component :: Text,
    -- | the creator of the component the SBom is generated for
    sbom'manufacture :: Text,
    -- | the supplier (manufacturer or repackager or distributor)
    sbom'supplier :: Maybe Text,
    -- | (spdxids of) licenses of the product
    sbom'licenses :: [Text]
  }

defaultSerializeSBom :: SerializeSBom
defaultSerializeSBom =
  MkSerializeSBom
    { sbom'version = 1,
      sbom'component = "wire-server",
      sbom'manufacture = "wire",
      sbom'supplier = Nothing,
      sbom'licenses = ["AGPL-3.0-or-later"]
    }

-- FUTUREWORK(mangoiv): we can also have
--
-- - qualifiers: extra qualifying data for a package such as an OS, architecture, a distro, etc. Optional and type-specific.
-- - subpath: extra subpath within a package, relative to the package root. Optional.
-- - use heuristics based approach to finding original repositories for packages, e.g. pkg:hackage....
mkPurl :: SBomMeta Identity -> Text
mkPurl meta =
  mconcat
    [ "pkg:",
      repo,
      "/",
      fromMaybe (runIdentity meta.outPath) meta.name,
      maybe "" ("@" <>) meta.version
    ]
  where
    repo
      | any (maybe False (T.isInfixOf "hackage.haskell.org")) meta.urls = "hackage"
      | otherwise = "nixpkgs"

-- | serializes an SBom to JSON format
--   conventions:
--   - bomRef == outPath
serializeSBom :: SerializeSBom -> SBom -> IO LazyByteString
serializeSBom settings bom = do
  uuid <- V4.nextRandom
  curTime <- getCurrentTime
  -- FUTUREWORK(mangoiv): "tools" (the tools used in the creation of the bom)
  let mkDependencies :: SBomMeta Identity -> Array
      mkDependencies meta = do
        let d =
              object
                [ "ref" .= meta.outPath,
                  "dependsOn" .= runIdentity meta.directDeps
                ]
        [d]
      mkComponents :: SBomMeta Identity -> Array
      mkComponents meta = do
        let c :: Value
            c =
              -- FUTUREWORK(mangoiv): swid? https://www.iso.org/standard/65666.html
              -- FUTUREWORK(mangoiv): CPE?
              -- FUTUREWORK(mangoiv): more information in the supplier section
              object
                [ "type" .= meta.typ,
                  "bom-ref" .= String (runIdentity meta.outPath),
                  "supplier" .= object ["url" .= nubOrd (maybeToList meta.homepage <> catMaybes meta.urls)],
                  "name" .= String (fromMaybe (st'name $ splitStorePath $ runIdentity meta.outPath) meta.name),
                  "version" .= meta.version,
                  "description" .= meta.description,
                  "scope" .= String "required",
                  "licenses" .= ((\ln -> object ["license" .= ln]) <$> filter (isJust . (>>= (.id))) meta.licenseSpdxId),
                  "purl" .= mkPurl meta
                ]
        [c]
      (dependencies, components) = foldMap (mkDependencies &&& mkComponents) bom

  pure $
    encode @Value $
      object
        [ "bomFormat" .= String "CycloneDX",
          "specVersion" .= String "1.5",
          "serialNumber" .= String ("urn:uuid:" <> UUID.toText uuid),
          "version" .= Number (fromIntegral settings.sbom'version),
          "metadata"
            .= object
              [ "timestamp" .= String (T.pack (show curTime)),
                "component"
                  .= object
                    [ "name" .= String settings.sbom'component,
                      "type" .= String "application"
                      -- FUTUREWORK(mangoiv): this should be a choice in the settings above
                    ],
                -- FUTUREWORK(mangoiv): "manufacture" can also have url
                "manufacture" .= object ["name" .= String settings.sbom'manufacture],
                "supplier" .= object ["name" .= String (fromMaybe settings.sbom'manufacture settings.sbom'supplier)],
                "licenses" .= Array (fromList $ object . (\n -> ["id" .= n]) . String <$> settings.sbom'licenses)
              ],
          "components" .= Array components,
          -- FUTUREWORK(mangoiv): services: allow to tell the program the name of the services like brig, galley, ...
          "dependencies" .= Array dependencies
        ]

buildMetaDB :: [Meta] -> MetaDB
buildMetaDB = foldMap \MkSBomMeta {..} -> [(drvPath, MkSBomMeta {..})]

discoverSBom :: FilePath -> MetaDB -> IO SBom
discoverSBom outP metaDb = do
  canonicalOutP <- canonicalizePath =<< getSymbolicLinkTarget outP
  info <- pathInfo canonicalOutP
  let go :: (Text, (Text, [Text])) -> IO SBom -> IO SBom
      go (k, (deriver, deps)) = do
        let proxyToIdentity :: SBomMeta Proxy -> SBomMeta Identity
            proxyToIdentity (MkSBomMeta {..}) = MkSBomMeta {directDeps = Identity deps, outPath = Identity k, ..}
        case M.lookup deriver metaDb of
          Nothing -> \x -> do
            T.putStrLn ("no meta found for drv: " <> deriver <> "\ntrying approximate match")
            x >>= maybe
              do
                \m -> do
                  T.putStrLn ("no approximate match found for: " <> deriver)
                  pure $ M.insert k (sadSbomMeta deriver k deps) m
              do \match -> pure . M.insert k (proxyToIdentity match)
              do approximateMatch deriver metaDb
          Just pmeta -> fmap $ M.insert k $ proxyToIdentity pmeta

  foldr go mempty info

data StorePath = MkStorePath
  { st'hash :: Text,
    st'name :: Text,
    st'original :: Text
  }
  deriving stock (Eq, Ord, Show)

-- >>> splitStorePath "/nix/store/m306sk6syihxp80zrr9xs8hi5mjricgh-sop-core-0.5.0.2"
-- MkStorePath {st'hash = "m306sk6syihxp80zrr9xs8hi5mjricgh", st'name = "sop-core-0.5.0.2", st'original = "/nix/store/m306sk6syihxp80zrr9xs8hi5mjricgh-sop-core-0.5.0.2"}
splitStorePath :: Text -> StorePath
splitStorePath stp = do
  let rest = T.drop (T.length "/nix/store/") stp
      (hash, T.drop 1 -> name) = T.breakOn "-" rest
  MkStorePath {st'original = stp, st'hash = hash, st'name = name}

approximateMatch :: Text -> MetaDB -> Maybe (SBomMeta Proxy)
approximateMatch stp db =
  let goal = splitStorePath stp
      metas = first splitStorePath <$> M.toList db
   in case filter (\(m, _) -> m.st'name == goal.st'name) metas of
        [(_stp, meta)] -> pure meta
        _ -> Nothing

parse :: IO (String, String)
parse = customExecParser (prefs showHelpOnEmpty) do
  Opt.info
    do drvAndTlParser
    do
      mconcat
        [ fullDesc,
          progDesc "build an sbom from a derivation and a package set"
        ]

drvAndTlParser :: Opt.Parser (String, String)
drvAndTlParser =
  (,)
    <$> strOption (long "drv" <> help "outpath of the derivation to build the sbom for" <> value "result")
    <*> strOption do
      long "tldfp"
        <> help "path to the derivation containing the output of the allLocalPackages drv"
        <> value "wire-server"

main :: IO ()
main = parse >>= mainNoParse >>= BSL.writeFile "sbom.json"

-- | by not always parsing, we have an easy time to call directly from haskell
mainNoParse :: (String, String) -> IO LazyByteString
mainNoParse (tldFp, drv) = do
  let mkMeta :: LazyByteString -> Maybe Meta
      mkMeta = decodeStrict . BSL.toStrict
  metaDB <- buildMetaDB . mapMaybe mkMeta . C8L.lines <$> BSL.readFile tldFp
  sbom <- discoverSBom drv metaDB
  serializeSBom defaultSerializeSBom sbom

pathInfo :: FilePath -> IO PathInfo
pathInfo path = do
  let nixPathInfo = proc "nix" ["path-info", path, "--json", "--recursive"]
  withCreateProcess nixPathInfo {std_out = CreatePipe} \_in (Just out) _err _ph -> do
    Just refs' <- decodeStrict @Value <$> C8.hGetContents out
    let failureBecauseNixHasZeroContracts = fail "unexpected format: this may be due to the output of `nix path-info` having changed randomly lol"
        tryFindOutpath :: Value -> IO (Key, Value)
        tryFindOutpath val
          | Object pc <- val,
            Just (String k) <- KM.lookup "path" pc =
              pure (KM.fromText k, val)
        tryFindOutpath _ = failureBecauseNixHasZeroContracts
    refs <- case refs' of
      Object refs -> pure $ KM.toList refs
      Array refs -> traverse tryFindOutpath $ toList refs
      _ -> failureBecauseNixHasZeroContracts

    let parseObj :: Value -> Maybe (Text, [Text])
        parseObj info
          | Object mp <- info,
            Just (Array rs) <- KM.lookup "references" mp,
            Just (String deriver) <- KM.lookup "deriver" mp,
            Just rs' <- for rs \case
              String s -> Just s
              _ -> Nothing =
              Just (deriver, toList rs')
        parseObj _ = trace "could not parse object" Nothing
    -- some heuristics based filtering
    pure
      -- remove derivations with the same deriver
      . nubOrdOn (fst . snd)
      -- remove derivations that are just docs
      . filter ((/= "doc") . T.takeEnd 3 . fst)
      . mapMaybe (bitraverse (pure . KM.toText) parseObj)
      $ refs

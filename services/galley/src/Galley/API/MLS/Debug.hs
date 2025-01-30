{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wwarn #-}

module Galley.API.MLS.Debug where

import Control.Comonad
import Data.Aeson qualified as Aeson
import Data.ByteString.Base64.Lazy qualified as B64LBS
import Data.ByteString.Char8 qualified as BSC8
import Data.ByteString.Conversion (fromByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Char qualified as Char
import Data.Domain
import Data.Id
import Data.IntMap qualified as IntMap
import Data.Json.Util
import Data.Qualified
import Data.Text qualified as T
import Galley.API.MLS.IncomingMessage
import Galley.API.MLS.Types
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Error qualified as PError
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as ReadP
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Commit
import Wire.API.MLS.Credential
import Wire.API.MLS.LeafNode
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation
import Wire.API.MLS.CommitBundle
import Wire.API.MLS.Extension
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.HPKEPublicKey
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Message
import Wire.API.Notification
import Galley.API.MLS.Commit.ExternalCommit

data RatchetTree = RatchetTree {nodes :: [Maybe Node]}
  deriving (Show, Eq)

data ParentNode = ParentNode
  { encryptionKey :: HPKEPublicKey,
    parentHash :: ByteString,
    umergedLeaves :: [Word32]
  }
  deriving (Show, Eq)

instance ParseMLS ParentNode where
  parseMLS =
    ParentNode <$> parseMLS <*> parseMLSBytes @VarInt <*> parseMLSVector @VarInt parseMLS

data NodeTag = LeafTag | ParentTag
  deriving (Show, Eq, Enum, Bounded)

data Node = Leaf LeafNode | Parent ParentNode
  deriving (Show, Eq)

instance ParseMLS Node where
  parseMLS =
    parseMLSEnum @Word8 "NodeTag" >>= \case
      LeafTag -> Leaf <$> parseMLS
      ParentTag -> Parent <$> parseMLS

instance ParseMLS RatchetTree where
  parseMLS =
    RatchetTree <$> parseMLSVector @VarInt (parseMLSOptional parseMLS)

data MLSAddMessageNotif = MLSAddMessageNotif
  { notifId :: NotificationId,
    convId :: Qualified ConvId,
    from :: Qualified UserId,
    time :: UTCTimeMillis,
    message :: RawMLS Message
  }
  deriving (Show, Eq)

instance Aeson.FromJSON MLSAddMessageNotif where
  parseJSON = Aeson.withObject "MLSAddMessageNotif" $ \o -> do
    notifId <- o Aeson..: "id"
    payload <- o Aeson..: "payload"
    flip (Aeson.withArray "Payload") payload $ \(toList -> [pv]) -> do
      flip (Aeson.withObject "") pv $ \p -> do
        convId <- p Aeson..: "qualified_conversation"
        from <- p Aeson..: "qualified_from"
        time <- p Aeson..: "time"
        message <- p Aeson..: "data"
        pure MLSAddMessageNotif {..}

getNotifs :: FilePath -> IO [MLSAddMessageNotif]
getNotifs f =
  either error id <$> Aeson.eitherDecodeFileStrict f

getCommits :: [MLSAddMessageNotif] -> [(NotificationId, Qualified UserId, UTCTimeMillis, [String])]
getCommits =
  mapMaybe $ \n ->
    case n.message.value.content of
      MessagePublic p -> case p.content.value.content of
        FramedContentCommit c ->
          Just (n.notifId, n.from, n.time, showCommitProposals c.value)
        _ -> Nothing
      _ -> Nothing

getProposals :: [MLSAddMessageNotif] -> [(UserId, UTCTimeMillis, Sender, [String])]
getProposals =
  mapMaybe $ \n ->
    case n.message.value.content of
      MessagePublic p -> case p.content.value.content of
        FramedContentProposal pr ->
          Just (n.from.qUnqualified, n.time, p.content.value.sender, ["proposal", showProposal $ Inline pr.value])
        FramedContentCommit c ->
          Just (n.from.qUnqualified, n.time, p.content.value.sender, "commit" : showCommitProposals c.value)
        _ -> Nothing
      _ -> Nothing

showCommitProposals :: Commit -> [String]
showCommitProposals c = map showProposal c.proposals

showProposal :: ProposalOrRef -> String
showProposal = \case
  Inline (AddProposal kp) -> "Add: " <> show (keyPackageIdentity kp.value)
  Inline (UpdateProposal ln) -> "Update: " <> show (fst <$> credentialIdentityAndKey ln.value.core.value.credential)
  Inline (RemoveProposal i) -> "Remove: " <> show i
  Inline (PreSharedKeyProposal _) -> "PSK"
  Inline (ReInitProposal _) -> "ReInit"
  Inline (ExternalInitProposal _) -> "ExternalInit"
  Inline (GroupContextExtensionsProposal _) -> "GroupContextExtensions"
  Ref _ -> "ReferencedProposal"

getBundle :: FilePath -> IO IncomingBundle
getBundle f = do
  b64 <- LBS.readFile f
  let Right bin = B64LBS.decode b64
      ebundle :: Either Text (RawMLS CommitBundle) = decodeMLS bin
      bundle = either (error . show) (id) ebundle
      Just incomingBundle = mkIncomingBundle bundle
  pure incomingBundle

getTree :: FilePath -> IO RatchetTree
getTree f = do
  b64 <- LBS.readFile f
  let Right bin = B64LBS.decode b64
      ebundle :: Either Text (RawMLS CommitBundle) = decodeMLS bin
      bundle = either (error . show) (id) ebundle
      Just incomingBundle = mkIncomingBundle bundle
      bs = (head incomingBundle.groupInfo.value.tbs.extensions).extData
      eTree = decodeMLS $ LBS.fromStrict bs
  either (error . show) pure eTree

getTreeFromBinGroupInfo :: FilePath -> IO RatchetTree
getTreeFromBinGroupInfo f = do
  bin <- LBS.readFile f
  let eGI :: Either Text GroupInfo = decodeMLS bin
      gi = either (error . show) id eGI
      eTree :: Either Text RatchetTree = decodeMLS $ LBS.fromStrict (head gi.tbs.extensions).extData
  either (error . show) pure eTree

nodesWithIndices :: RatchetTree -> [(Int, String)]
nodesWithIndices tree =
  zip [0 ..] (map nodeName tree.nodes)

-- THis is wrong
indexMapFromTree :: RatchetTree -> IndexMap
indexMapFromTree t =
  let mIdentities = map (fmap (leafIdentity . assertLeaf)) (takeEvens t.nodes)
      indexedMIdentities = zip [0 ..] mIdentities
   in IndexMap $ foldr (\(idx, mIdent) acc -> IntMap.alter (const mIdent) idx acc) mempty indexedMIdentities

printIndexMap :: IndexMap -> IO ()
printIndexMap (IndexMap im) =
  void $ IntMap.traverseWithKey (\idx ci -> putStrLn $ show idx <> " | " <> T.unpack (idToText ci.ciUser) <> " | " <> T.unpack (clientToText ci.ciClient)) im

parseIndexMap :: FilePath -> IO IndexMap
parseIndexMap f = do
  contents <- readFile f
  case ReadP.readP_to_S (indexMapParser <* ReadP.eof) contents of
    [(im, "")] -> pure im
    x -> error $ show x

indexMapParser :: ReadP IndexMap
indexMapParser = do
  IndexMap . IntMap.fromList <$> ReadP.sepBy indexMapRowParser (ReadP.satisfy (== '\n'))

indexMapRowParser :: ReadP (Int, ClientIdentity)
indexMapRowParser = do
  let delimiter = do
        ReadP.skipSpaces
        _ <- ReadP.char '|'
        ReadP.skipSpaces

  domain <- Domain . T.pack <$> ReadP.munch (not . Char.isSpace)
  delimiter
  uidStr <- ReadP.munch (not . Char.isSpace)
  uid <- either (fail) pure $ parseIdFromText (T.pack uidStr)
  delimiter
  cidStr <- ReadP.munch (not . Char.isSpace)
  cid <- maybe (fail "invalid client id") pure $ fromByteString (BSC8.pack cidStr)
  delimiter
  idx <- read <$> ReadP.munch Char.isDigit
  _ <- ReadP.char ' '
  pure (idx, ClientIdentity domain uid cid)

nodeName :: Maybe Node -> String
nodeName Nothing = "empty"
nodeName (Just Parent {}) = "parent"
nodeName (Just (Leaf l)) =
  case l.core.value.credential of
    BasicCredential b -> BSC8.unpack b
    X509Credential _ -> "x509-cred"

takeEvens :: [a] -> [a]
takeEvens [] = []
takeEvens [x] = [x]
takeEvens (x : _ : xs) = x : takeEvens xs

assertLeaf :: Node -> LeafNode
assertLeaf (Parent _) = error "is not leaf node"
assertLeaf (Leaf l) = l

leafIdentity :: LeafNode -> ClientIdentity
leafIdentity l =
  case credentialIdentityAndKey l.core.value.credential of
    Left e -> error (show e)
    Right (ci, _) -> ci

-- getParentHashes

data ParentHashInput = ParentHashInput
  { encryptionKey :: HPKEPublicKey,
    parentHash :: ByteString,
    originalSibilingTreeHash :: ByteString
  }
  deriving (Show)

ignoreError :: (Show x) => Sem (Error x ': r) a -> Sem r a
ignoreError action = do
  eRes <- PError.runError action
  case eRes of
    Left e -> error $ show e
    Right x -> pure x

ignoreErrorS :: Sem (ErrorS x ': r) a -> Sem r a
ignoreErrorS = ignoreError

verifyCommitBundle :: FilePath -> FilePath -> IO ()
verifyCommitBundle commitBundleFile indexMapFile = do
  b64 <- LBS.readFile commitBundleFile
  indexMapFromProd <- parseIndexMap indexMapFile
  let Right bin = B64LBS.decode b64
      Right (bundle :: RawMLS CommitBundle) = decodeMLS bin
      Just incomingBundle = mkIncomingBundle bundle
      Just cs = cipherSuiteTag incomingBundle.groupInfo.value.groupContext.cipherSuite
      Just leafNode = (.leaf) <$> incomingBundle.commit.value.path
      pub = leafNode.value.signatureKey
      -- Eyeballed information
      -- 8d74979d-1042-4890-a977-efd64e0c2d06:4efd71fad942c43b@wire.com
      Right uid = parseIdFromText "8d74979d-1042-4890-a977-efd64e0c2d06"
      Just cid = fromByteString "4efd71fad942c43b"
      ci = ClientIdentity (Domain "wire.com") uid cid
      -- Eyeballed information ends
      groupId = incomingBundle.groupId
      action =
        run . ignoreError @MLSProtocolError . ignoreErrorS @MLSStaleMessage . ignoreErrorS @MLSUnsupportedProposal . ignoreErrorS @MLSInvalidLeafNodeIndex $
          getExternalCommitData' cs ci indexMapFromProd groupId incomingBundle.commit.value
      signedThing = mkRawMLS $ LeafNodeTBS leafNode.value.core $ LeafNodeTBSExtraCommit groupId action.add
      sig = leafNode.value.signature_
      verification = csVerifySignatureWithLabel cs pub "LeafNodeTBS" signedThing sig
  print verification

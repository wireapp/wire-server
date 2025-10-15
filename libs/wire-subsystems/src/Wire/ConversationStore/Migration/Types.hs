module Wire.ConversationStore.Migration.Types where

import Imports
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.LeafNode
import Wire.ConversationStore.MLS.Types
import Wire.StoredConversation

data ConvMLSDetails = ConvMLSDetails
  { groupInfoData :: GroupInfoData,
    clientMap :: ClientMap LeafIndex,
    indexMap :: IndexMap
  }

data AllSubConvData = AllSubConvData
  { subConv :: SubConversation,
    groupInfoData :: Maybe GroupInfoData
  }

data AllConvData = AllConvData
  { conv :: StoredConversation,
    mlsDetails :: Maybe ConvMLSDetails,
    subConvs :: [AllSubConvData]
  }

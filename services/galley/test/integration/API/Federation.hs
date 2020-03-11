module API.Federation where

import Imports
import Test.Tasty
import Test.Tasty.HUnit
import TestHelpers (test)
import TestSetup (TestM, TestSetup, tsCannon, tsGConf, tsGalley)

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "federation"
    [ testGroup
        "guest accounts"
        [ test s "can join a remote conversation" joinRemoteConversation ,
          test s "can send message to local conversation with remote members" localWithRemoteMembers,
          test s "can send message to remote conversation with local members" remoteWithLocalMembers,
          test s "can send message to remote conversation with remote members" remoteWithRemoteMembers
        ]
    ]

joinRemoteConversation :: TestM ()
joinRemoteConversation = undefined

localWithRemoteMembers :: TestM ()
localWithRemoteMembers = undefined

remoteWithLocalMembers :: TestM ()
remoteWithLocalMembers = undefined

remoteWithRemoteMembers :: TestM ()
remoteWithRemoteMembers = undefined

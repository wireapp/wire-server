{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Wire.API.Golden.Manual.Pagination where

import Data.Default
import Data.Id
import Data.Json.Util
import Data.UUID qualified as UUID
import Data.Vector qualified as Vec
import Imports
import Wire.API.User.Profile
import Wire.API.UserGroup
import Wire.API.UserGroup.Pagination

someUTCTime :: UTCTimeMillis
Just someUTCTime = readUTCTimeMillis "2025-04-16T16:22:21.703Z"

someOtherUTCTime :: UTCTimeMillis
Just someOtherUTCTime = readUTCTimeMillis "2021-12-12T00:00:00.000Z"

ug1 :: UserGroup
ug1 =
  UserGroup_
    { id_ = Id UUID.nil,
      name = either (error . show) id (userGroupNameFromText "*"),
      members = mempty,
      managedBy = ManagedByWire,
      createdAt = someUTCTime
    }

ug2 :: UserGroup
ug2 =
  UserGroup_
    { id_ = Id . fromJust $ UUID.fromString "63dd98c0-552d-11f0-8df7-b3e03cd56036",
      name = either (error . show) id (userGroupNameFromText "##name1##"),
      members =
        Identity . Vec.fromList $
          ( Id . fromJust . UUID.fromString
              <$> [ "1f815fa2-552f-11f0-8642-77f29e68cbc9",
                    "28a9c560-552f-11f0-9082-97e15e952720",
                    "3ec5afe4-552f-11f0-afbb-9b038a8edbd2"
                  ]
          ),
      managedBy = ManagedByWire,
      createdAt = someUTCTime
    }

ug3 :: UserGroup
ug3 =
  UserGroup_
    { id_ = Id . fromJust $ UUID.fromString "60278b50-552d-11f0-892b-ebd66f6c2c30",
      name = either (error . show) id (userGroupNameFromText "!! user group !!"),
      members =
        Identity $ Vec.fromList (Id . fromJust . UUID.fromString <$> ["37b636e2-552f-11f0-abe8-5bf7b2ad08c9"]),
      managedBy = ManagedByScim,
      createdAt = someOtherUTCTime
    }

testObject_PaginationState_1 :: PaginationState
testObject_PaginationState_1 = def

testObject_PaginationState_2 :: PaginationState
testObject_PaginationState_2 =
  PaginationState
    { searchString = Just "***",
      sortBy = SortByName,
      sortOrder = Asc,
      pageSize = pageSizeFromIntUnsafe 39,
      lastSeen =
        Just $
          LastSeen
            (Just . userGroupNameFromTextUnsafe $ "boring group")
            (Just someOtherUTCTime)
            (Id . fromJust . UUID.fromString $ "4d41b282-6887-11f0-b0dc-1f902c75fe84")
    }

testObject_PaginationResult_1 :: PaginationResult
testObject_PaginationResult_1 = PaginationResult [] def

testObject_PaginationResult_2 :: PaginationResult
testObject_PaginationResult_2 =
  PaginationResult
    (userGroupToMeta <$> [ug1, ug2])
    PaginationState
      { searchString = Just "q",
        sortBy = SortByName,
        sortOrder = Desc,
        pageSize = pageSizeFromIntUnsafe 500,
        lastSeen =
          Just
            ( LastSeen
                (Just $ userGroupNameFromTextUnsafe "ugn!")
                Nothing
                (Id . fromJust . UUID.fromString $ "df888158-6606-11f0-8095-9353d5ffded9")
            )
      }

testObject_PaginationResult_3 :: PaginationResult
testObject_PaginationResult_3 =
  PaginationResult
    (userGroupToMeta <$> [ug2])
    PaginationState
      { searchString = Just "rst",
        sortBy = SortByCreatedAt,
        sortOrder = Asc,
        pageSize = pageSizeFromIntUnsafe 1,
        lastSeen =
          Just
            ( LastSeen
                Nothing
                (Just . fromJust . readUTCTimeMillis $ "2021-12-12T00:00:00.000Z")
                (Id . fromJust . UUID.fromString $ "e5b57c16-6606-11f0-ab44-770b115161d3")
            )
      }

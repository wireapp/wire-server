{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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
module Test.Wire.API.Golden.Generated.ClientMismatch_user where

import Data.Id (ClientId (ClientId, client), Id (Id))
import Data.Json.Util (toUTCTimeMillis)
import qualified Data.UUID as UUID (fromString)
import GHC.Exts (IsList (fromList))
import Imports (fromJust, read)
import Wire.API.Message (ClientMismatch (ClientMismatch), UserClients (UserClients, userClients))

testObject_ClientMismatch_user_1 :: ClientMismatch
testObject_ClientMismatch_user_1 =
  ( ClientMismatch
      (toUTCTimeMillis (read "1864-04-12 12:22:43.673 UTC"))
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))),
                    fromList [ClientId {client = "2"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000002"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))), fromList [])
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ((Id (fromJust (UUID.fromString "00000009-0000-005e-0000-002e00000012"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000072-0000-0051-0000-000500000007"))), fromList [])
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000700000000"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000005-0000-0000-0000-000600000008"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000005-0000-0002-0000-000800000001"))),
                    fromList [ClientId {client = "3"}, ClientId {client = "4"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000005-0000-0006-0000-000400000001"))), fromList [])
                ]
          }
      )
  )

testObject_ClientMismatch_user_2 :: ClientMismatch
testObject_ClientMismatch_user_2 =
  ( ClientMismatch
      (toUTCTimeMillis (read "1864-04-19 08:06:54.492 UTC"))
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "0000000d-0000-000e-0000-00170000001c"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "2"}, ClientId {client = "3"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "0000000e-0000-001d-0000-00160000001c"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000016-0000-0005-0000-001b0000001e"))),
                    fromList [ClientId {client = "50"}]
                  )
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000000"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000"))),
                    fromList [ClientId {client = "3"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))), fromList [])
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000001-0000-0006-0000-000800000004"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000005-0000-0000-0000-000100000002"))),
                    fromList [ClientId {client = "2"}, ClientId {client = "4"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000005-0000-0000-0000-000600000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000007-0000-0004-0000-000800000001"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  )
                ]
          }
      )
  )

testObject_ClientMismatch_user_3 :: ClientMismatch
testObject_ClientMismatch_user_3 =
  ( ClientMismatch
      (toUTCTimeMillis (read "1864-05-18 16:25:29.722 UTC"))
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000002-0000-0006-0000-000700000013"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000019-0000-001e-0000-000e0000001e"))),
                    fromList [ClientId {client = "1"}, ClientId {client = "e"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "0000001e-0000-000b-0000-000600000018"))),
                    fromList [ClientId {client = "b"}]
                  )
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000300000002"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000300000002"))),
                    fromList [ClientId {client = "8"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000003"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000000000000"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000000000000"))),
                    fromList [ClientId {client = "3"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000300000002"))),
                    fromList [ClientId {client = "e"}]
                  )
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  )
                ]
          }
      )
  )

testObject_ClientMismatch_user_4 :: ClientMismatch
testObject_ClientMismatch_user_4 =
  ( ClientMismatch
      (toUTCTimeMillis (read "1864-04-20 07:47:05.133 UTC"))
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000800000002"))),
                    fromList [ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000300000000"))),
                    fromList [ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000200000005"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000005-0000-0006-0000-000500000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000006-0000-0001-0000-000100000006"))), fromList [])
                ]
          }
      )
      (UserClients {userClients = fromList []})
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000000-0000-0007-0000-000700000001"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000200000003"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}, ClientId {client = "2"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000800000005"))),
                    fromList [ClientId {client = "b"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000005-0000-0008-0000-000000000001"))), fromList [])
                ]
          }
      )
  )

testObject_ClientMismatch_user_5 :: ClientMismatch
testObject_ClientMismatch_user_5 =
  ( ClientMismatch
      (toUTCTimeMillis (read "1864-04-26 19:31:21.478 UTC"))
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000000"))),
                    fromList [ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))),
                    fromList [ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))),
                    fromList [ClientId {client = "2"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))), fromList [])
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                    fromList [ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000002"))),
                    fromList [ClientId {client = "3"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000"))),
                    fromList [ClientId {client = "1"}]
                  )
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  )
                ]
          }
      )
  )

testObject_ClientMismatch_user_6 :: ClientMismatch
testObject_ClientMismatch_user_6 =
  ( ClientMismatch
      (toUTCTimeMillis (read "1864-05-28 18:24:35.996 UTC"))
      ( UserClients
          { userClients =
              fromList
                [ ((Id (fromJust (UUID.fromString "00000001-0000-0006-0000-000300000003"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000300000005"))),
                    fromList [ClientId {client = "1"}, ClientId {client = "2"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000004-0000-0005-0000-000100000006"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000007-0000-0001-0000-000500000008"))),
                    fromList [ClientId {client = "1a"}]
                  )
                ]
          }
      )
      (UserClients {userClients = fromList []})
      (UserClients {userClients = fromList []})
  )

testObject_ClientMismatch_user_7 :: ClientMismatch
testObject_ClientMismatch_user_7 =
  ( ClientMismatch
      (toUTCTimeMillis (read "1864-05-26 02:38:01.741 UTC"))
      (UserClients {userClients = fromList []})
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))),
                    fromList [ClientId {client = "2"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  )
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                    fromList [ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
                    fromList [ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), fromList [])
                ]
          }
      )
  )

testObject_ClientMismatch_user_8 :: ClientMismatch
testObject_ClientMismatch_user_8 =
  ( ClientMismatch
      (toUTCTimeMillis (read "1864-04-11 13:11:44.951 UTC"))
      ( UserClients
          { userClients =
              fromList
                [ ((Id (fromJust (UUID.fromString "00000013-0000-0067-0000-00540000000f"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "0000001f-0000-0001-0000-007600000041"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}, ClientId {client = "2"}]
                  )
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000400000001"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000400000000"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000300000002"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000004"))),
                    fromList [ClientId {client = "b"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000100000000"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "2"}]
                  )
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "0000003a-0000-0008-0000-00740000006a"))),
                    fromList [ClientId {client = "52f"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "0000003d-0000-0037-0000-005900000042"))),
                    fromList [ClientId {client = "2"}, ClientId {client = "3"}, ClientId {client = "4"}]
                  )
                ]
          }
      )
  )

testObject_ClientMismatch_user_9 :: ClientMismatch
testObject_ClientMismatch_user_9 =
  ( ClientMismatch
      (toUTCTimeMillis (read "1864-04-20 09:37:09.767 UTC"))
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00003597-0000-21f9-0000-74b8000066b6"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}, ClientId {client = "2"}]
                  )
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000019-0000-004b-0000-00300000007e"))),
                    fromList [ClientId {client = "1b"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000057-0000-0020-0000-006600000037"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}, ClientId {client = "4"}]
                  )
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000029-0000-004d-0000-006d0000000a"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "0000006c-0000-002c-0000-006600000027"))), fromList [])
                ]
          }
      )
  )

testObject_ClientMismatch_user_10 :: ClientMismatch
testObject_ClientMismatch_user_10 =
  ( ClientMismatch
      (toUTCTimeMillis (read "1864-06-08 05:23:30.672 UTC"))
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000031-0000-0056-0000-007600000063"))),
                    fromList [ClientId {client = "20"}, ClientId {client = "f"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000051-0000-007d-0000-004600000046"))),
                    fromList [ClientId {client = "1"}, ClientId {client = "8"}, ClientId {client = "e"}]
                  )
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000007-0000-0008-0000-001a00000011"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "3"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000016-0000-0013-0000-000a00000009"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "0000001c-0000-0003-0000-001300000012"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "2"}]
                  )
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "0000004c-0000-006b-0000-000d00000009"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "0000006b-0000-0079-0000-007e00000028"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}, ClientId {client = "2"}]
                  )
                ]
          }
      )
  )

testObject_ClientMismatch_user_11 :: ClientMismatch
testObject_ClientMismatch_user_11 =
  ( ClientMismatch
      (toUTCTimeMillis (read "1864-04-14 22:55:33.894 UTC"))
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000015-0000-0004-0000-000300000006"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000015-0000-000f-0000-00040000000f"))), fromList []),
                  ((Id (fromJust (UUID.fromString "0000001b-0000-0005-0000-001800000000"))), fromList [])
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000002-0000-0016-0000-000d00000017"))),
                    fromList [ClientId {client = "1"}, ClientId {client = "2"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000008-0000-000c-0000-00090000001a"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "b"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "0000001d-0000-001a-0000-00080000000d"))),
                    fromList [ClientId {client = "1e"}]
                  )
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                    fromList [ClientId {client = "3"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000"))),
                    fromList [ClientId {client = "3"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))), fromList [])
                ]
          }
      )
  )

testObject_ClientMismatch_user_12 :: ClientMismatch
testObject_ClientMismatch_user_12 =
  ( ClientMismatch
      (toUTCTimeMillis (read "1864-05-08 01:07:14.883 UTC"))
      (UserClients {userClients = fromList []})
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000068-0000-0004-0000-004800000071"))),
                    fromList [ClientId {client = "dab"}]
                  ),
                  ((Id (fromJust (UUID.fromString "0000007b-0000-003e-0000-002a00000078"))), fromList [])
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))),
                    fromList [ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))),
                    fromList [ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))),
                    fromList [ClientId {client = "2"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))),
                    fromList [ClientId {client = "2"}]
                  )
                ]
          }
      )
  )

testObject_ClientMismatch_user_13 :: ClientMismatch
testObject_ClientMismatch_user_13 =
  ( ClientMismatch
      (toUTCTimeMillis (read "1864-05-09 16:28:56.647 UTC"))
      ( UserClients
          { userClients =
              fromList
                [ ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), fromList [])
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000300000002"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000000000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000300000001"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000400000001"))),
                    fromList [ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000400000002"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000200000004"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000000000003"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  )
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
                    fromList [ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                    fromList [ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                    fromList [ClientId {client = "1"}]
                  )
                ]
          }
      )
  )

testObject_ClientMismatch_user_14 :: ClientMismatch
testObject_ClientMismatch_user_14 =
  ( ClientMismatch
      (toUTCTimeMillis (read "1864-05-08 01:02:42.968 UTC"))
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))),
                    fromList [ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000002"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))),
                    fromList [ClientId {client = "3"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))),
                    fromList [ClientId {client = "3"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))),
                    fromList [ClientId {client = "3"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  )
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000017-0000-0001-0000-005b00000037"))),
                    fromList [ClientId {client = "8a6"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000080-0000-003c-0000-00680000001d"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "2"}, ClientId {client = "4"}]
                  )
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00004cd5-0000-31af-0000-638a000043c3"))),
                    fromList [ClientId {client = "2a5"}, ClientId {client = "3d0"}]
                  )
                ]
          }
      )
  )

testObject_ClientMismatch_user_15 :: ClientMismatch
testObject_ClientMismatch_user_15 =
  ( ClientMismatch
      (toUTCTimeMillis (read "1864-06-02 22:04:34.496 UTC"))
      ( UserClients
          { userClients =
              fromList
                [ ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), fromList [])
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000005"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000500000001"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000003-0000-0008-0000-000600000008"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000007-0000-0003-0000-000600000006"))),
                    fromList [ClientId {client = "1"}, ClientId {client = "2"}]
                  )
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00002695-0000-0695-0000-494f00003295"))),
                    fromList [ClientId {client = "13"}, ClientId {client = "15"}, ClientId {client = "6"}]
                  )
                ]
          }
      )
  )

testObject_ClientMismatch_user_16 :: ClientMismatch
testObject_ClientMismatch_user_16 =
  ( ClientMismatch
      (toUTCTimeMillis (read "1864-06-01 16:55:21.151 UTC"))
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                    fromList [ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))),
                    fromList [ClientId {client = "3"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                    fromList [ClientId {client = "2"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000001"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))),
                    fromList [ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))), fromList [])
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000a6b-0000-5bd1-0000-7ab500002215"))),
                    fromList [ClientId {client = "55"}, ClientId {client = "76"}, ClientId {client = "c0"}]
                  )
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000000-0000-001f-0000-000b0000001c"))),
                    fromList [ClientId {client = "8"}, ClientId {client = "e"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000013-0000-0015-0000-001f0000000a"))),
                    fromList [ClientId {client = "41"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000018-0000-0020-0000-002000000000"))),
                    fromList [ClientId {client = "1"}, ClientId {client = "3"}, ClientId {client = "4"}]
                  )
                ]
          }
      )
  )

testObject_ClientMismatch_user_17 :: ClientMismatch
testObject_ClientMismatch_user_17 =
  ( ClientMismatch
      (toUTCTimeMillis (read "1864-04-23 21:23:53.493 UTC"))
      ( UserClients
          { userClients =
              fromList
                [ ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                    fromList [ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))),
                    fromList [ClientId {client = "0"}]
                  )
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "0000451d-0000-7508-0000-317f0000186b"))),
                    fromList [ClientId {client = "e730"}]
                  )
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000025-0000-0012-0000-002500000033"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "0000003b-0000-005f-0000-00330000007f"))), fromList [])
                ]
          }
      )
  )

testObject_ClientMismatch_user_18 :: ClientMismatch
testObject_ClientMismatch_user_18 =
  ( ClientMismatch
      (toUTCTimeMillis (read "1864-05-14 18:56:29.815 UTC"))
      ( UserClients
          { userClients =
              fromList
                [ ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000400000000"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))),
                    fromList [ClientId {client = "1"}, ClientId {client = "2"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000300000004"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000400000004"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000100000004"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000400000004"))),
                    fromList [ClientId {client = "5"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000300000002"))), fromList [])
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                    fromList [ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  )
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000008"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000007"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000800000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "4"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000800000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  )
                ]
          }
      )
  )

testObject_ClientMismatch_user_19 :: ClientMismatch
testObject_ClientMismatch_user_19 =
  ( ClientMismatch
      (toUTCTimeMillis (read "1864-06-06 11:59:12.981 UTC"))
      ( UserClients
          { userClients =
              fromList
                [ ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000002"))),
                    fromList [ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))),
                    fromList [ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000002"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                    fromList [ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))),
                    fromList [ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002"))),
                    fromList [ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))), fromList [])
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))),
                    fromList [ClientId {client = "3"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                    fromList [ClientId {client = "0"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001"))),
                    fromList [ClientId {client = "3"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), fromList [])
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000013-0000-0002-0000-003500000013"))),
                    fromList [ClientId {client = "11"}, ClientId {client = "9"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "0000003c-0000-000f-0000-003e00000013"))),
                    fromList [ClientId {client = "1"}, ClientId {client = "8"}]
                  )
                ]
          }
      )
  )

testObject_ClientMismatch_user_20 :: ClientMismatch
testObject_ClientMismatch_user_20 =
  ( ClientMismatch
      (toUTCTimeMillis (read "1864-05-20 02:14:30.091 UTC"))
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000004"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000300000002"))),
                    fromList [ClientId {client = "e"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000300000003"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000300000003"))),
                    fromList [ClientId {client = "6"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000400000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  )
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ( (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000200000004"))),
                    fromList [ClientId {client = "f"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000300000003"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000100000003"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ( (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000100000002"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "1"}]
                  ),
                  ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000300000003"))), fromList []),
                  ( (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000000000001"))),
                    fromList [ClientId {client = "0"}, ClientId {client = "2"}]
                  )
                ]
          }
      )
      ( UserClients
          { userClients =
              fromList
                [ ((Id (fromJust (UUID.fromString "00000008-0000-0018-0000-00080000000a"))), fromList []),
                  ((Id (fromJust (UUID.fromString "00000018-0000-0017-0000-00110000000a"))), fromList []),
                  ((Id (fromJust (UUID.fromString "0000001a-0000-0020-0000-000400000016"))), fromList [])
                ]
          }
      )
  )

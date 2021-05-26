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
module Test.Wire.API.Golden.Generated.ConversationCode_user where

import Data.Code (Key (Key, asciiKey), Value (Value, asciiValue))
import Data.Coerce (coerce)
import Data.Misc (HttpsUrl (HttpsUrl))
import Data.Range (unsafeRange)
import Data.Text.Ascii (AsciiChars (validate))
import Imports (Maybe (Just, Nothing), fromRight, undefined)
import URI.ByteString
  ( Authority
      ( Authority,
        authorityHost,
        authorityPort,
        authorityUserInfo
      ),
    Host (Host, hostBS),
    Query (Query, queryPairs),
    Scheme (Scheme, schemeBS),
    URIRef
      ( URI,
        uriAuthority,
        uriFragment,
        uriPath,
        uriQuery,
        uriScheme
      ),
  )
import Wire.API.Conversation.Code (ConversationCode (..))

testObject_ConversationCode_user_1 :: ConversationCode
testObject_ConversationCode_user_1 =
  ConversationCode
    { conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("M0vnbETaqAgL8tv5Z1_x")))))},
      conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("sEG3Y60tIsd9P3")))))},
      conversationUri =
        Just
          ( coerce
              URI
                { uriScheme = Scheme {schemeBS = "https"},
                  uriAuthority =
                    Just
                      ( Authority
                          { authorityUserInfo = Nothing,
                            authorityHost = Host {hostBS = "example.com"},
                            authorityPort = Nothing
                          }
                      ),
                  uriPath = "",
                  uriQuery = Query {queryPairs = []},
                  uriFragment = Nothing
                }
          )
    }

testObject_ConversationCode_user_2 :: ConversationCode
testObject_ConversationCode_user_2 =
  ConversationCode
    { conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("NEN=eLUWHXclTp=_2Nap")))))},
      conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("lLz-9vR8ENum0kI-xWJs")))))},
      conversationUri = Nothing
    }

testObject_ConversationCode_user_3 :: ConversationCode
testObject_ConversationCode_user_3 =
  ConversationCode
    { conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("0=qjmkA4cwBtH_7sh1xk")))))},
      conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("XRuFWme95W-BwO_Ftz")))))},
      conversationUri =
        Just
          ( coerce
              URI
                { uriScheme = Scheme {schemeBS = "https"},
                  uriAuthority =
                    Just
                      ( Authority
                          { authorityUserInfo = Nothing,
                            authorityHost = Host {hostBS = "example.com"},
                            authorityPort = Nothing
                          }
                      ),
                  uriPath = "",
                  uriQuery = Query {queryPairs = []},
                  uriFragment = Nothing
                }
          )
    }

testObject_ConversationCode_user_4 :: ConversationCode
testObject_ConversationCode_user_4 =
  ConversationCode
    { conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("voBZnPMFx8w7qHTJMP7Y")))))},
      conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("fEPoIgD4yWs0mBa-bJ3")))))},
      conversationUri =
        Just
          ( coerce
              URI
                { uriScheme = Scheme {schemeBS = "https"},
                  uriAuthority =
                    Just
                      ( Authority
                          { authorityUserInfo = Nothing,
                            authorityHost = Host {hostBS = "example.com"},
                            authorityPort = Nothing
                          }
                      ),
                  uriPath = "",
                  uriQuery = Query {queryPairs = []},
                  uriFragment = Nothing
                }
          )
    }

testObject_ConversationCode_user_5 :: ConversationCode
testObject_ConversationCode_user_5 =
  ConversationCode
    { conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("05wvfrdJeHp6rJprZgWB")))))},
      conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("3UvuxN9u")))))},
      conversationUri =
        Just
          ( coerce
              URI
                { uriScheme = Scheme {schemeBS = "https"},
                  uriAuthority =
                    Just
                      ( Authority
                          { authorityUserInfo = Nothing,
                            authorityHost = Host {hostBS = "example.com"},
                            authorityPort = Nothing
                          }
                      ),
                  uriPath = "",
                  uriQuery = Query {queryPairs = []},
                  uriFragment = Nothing
                }
          )
    }

testObject_ConversationCode_user_6 :: ConversationCode
testObject_ConversationCode_user_6 =
  ConversationCode
    { conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("-luNrgw7W3X0kFR4izG0")))))},
      conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("ju-=kF9RH8Jd")))))},
      conversationUri =
        Just
          ( coerce
              URI
                { uriScheme = Scheme {schemeBS = "https"},
                  uriAuthority =
                    Just
                      ( Authority
                          { authorityUserInfo = Nothing,
                            authorityHost = Host {hostBS = "example.com"},
                            authorityPort = Nothing
                          }
                      ),
                  uriPath = "",
                  uriQuery = Query {queryPairs = []},
                  uriFragment = Nothing
                }
          )
    }

testObject_ConversationCode_user_7 :: ConversationCode
testObject_ConversationCode_user_7 =
  ConversationCode
    { conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("YUZI4FpWueY0v_l9_JQa")))))},
      conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("L7vTY6vgEeYGzTBtcL")))))},
      conversationUri =
        Just
          ( coerce
              URI
                { uriScheme = Scheme {schemeBS = "https"},
                  uriAuthority =
                    Just
                      ( Authority
                          { authorityUserInfo = Nothing,
                            authorityHost = Host {hostBS = "example.com"},
                            authorityPort = Nothing
                          }
                      ),
                  uriPath = "",
                  uriQuery = Query {queryPairs = []},
                  uriFragment = Nothing
                }
          )
    }

testObject_ConversationCode_user_8 :: ConversationCode
testObject_ConversationCode_user_8 =
  ConversationCode
    { conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("bY-JjZMrTX8teN75SHjn")))))},
      conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("rkSyHahGe=J-lDQ45S")))))},
      conversationUri =
        Just
          ( coerce
              URI
                { uriScheme = Scheme {schemeBS = "https"},
                  uriAuthority =
                    Just
                      ( Authority
                          { authorityUserInfo = Nothing,
                            authorityHost = Host {hostBS = "example.com"},
                            authorityPort = Nothing
                          }
                      ),
                  uriPath = "",
                  uriQuery = Query {queryPairs = []},
                  uriFragment = Nothing
                }
          )
    }

testObject_ConversationCode_user_9 :: ConversationCode
testObject_ConversationCode_user_9 =
  ConversationCode
    { conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("Keb12zuEIzF7IUsB6SfQ")))))},
      conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("-L5q5MU16hmMXL")))))},
      conversationUri =
        Just
          ( coerce
              URI
                { uriScheme = Scheme {schemeBS = "https"},
                  uriAuthority =
                    Just
                      ( Authority
                          { authorityUserInfo = Nothing,
                            authorityHost = Host {hostBS = "example.com"},
                            authorityPort = Nothing
                          }
                      ),
                  uriPath = "",
                  uriQuery = Query {queryPairs = []},
                  uriFragment = Nothing
                }
          )
    }

testObject_ConversationCode_user_10 :: ConversationCode
testObject_ConversationCode_user_10 =
  ConversationCode
    { conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("1zhL_Hp9mlkMM3OW47I8")))))},
      conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("2VrAbmBi1")))))},
      conversationUri =
        Just
          ( coerce
              URI
                { uriScheme = Scheme {schemeBS = "https"},
                  uriAuthority =
                    Just
                      ( Authority
                          { authorityUserInfo = Nothing,
                            authorityHost = Host {hostBS = "example.com"},
                            authorityPort = Nothing
                          }
                      ),
                  uriPath = "",
                  uriQuery = Query {queryPairs = []},
                  uriFragment = Nothing
                }
          )
    }

testObject_ConversationCode_user_11 :: ConversationCode
testObject_ConversationCode_user_11 =
  ConversationCode
    { conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("ReyDOsYF_D5HyDMX81FP")))))},
      conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("FRpchhjXoWB2")))))},
      conversationUri = Nothing
    }

testObject_ConversationCode_user_12 :: ConversationCode
testObject_ConversationCode_user_12 =
  ConversationCode
    { conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("u2EODzrdg6Z7aAgci4MV")))))},
      conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("kMx3bFGd")))))},
      conversationUri =
        Just
          ( coerce
              URI
                { uriScheme = Scheme {schemeBS = "https"},
                  uriAuthority =
                    Just
                      ( Authority
                          { authorityUserInfo = Nothing,
                            authorityHost = Host {hostBS = "example.com"},
                            authorityPort = Nothing
                          }
                      ),
                  uriPath = "",
                  uriQuery = Query {queryPairs = []},
                  uriFragment = Nothing
                }
          )
    }

testObject_ConversationCode_user_13 :: ConversationCode
testObject_ConversationCode_user_13 =
  ConversationCode
    { conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("16xoACse_WkiFLw1S6w7")))))},
      conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("3acAKsrSdrIHN")))))},
      conversationUri =
        Just
          ( coerce
              URI
                { uriScheme = Scheme {schemeBS = "https"},
                  uriAuthority =
                    Just
                      ( Authority
                          { authorityUserInfo = Nothing,
                            authorityHost = Host {hostBS = "example.com"},
                            authorityPort = Nothing
                          }
                      ),
                  uriPath = "",
                  uriQuery = Query {queryPairs = []},
                  uriFragment = Nothing
                }
          )
    }

testObject_ConversationCode_user_14 :: ConversationCode
testObject_ConversationCode_user_14 =
  ConversationCode
    { conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("skREPTCY5lQiIoT5_sCo")))))},
      conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("mCwcpFiVwi=1ORILCy5l")))))},
      conversationUri =
        Just
          ( coerce
              URI
                { uriScheme = Scheme {schemeBS = "https"},
                  uriAuthority =
                    Just
                      ( Authority
                          { authorityUserInfo = Nothing,
                            authorityHost = Host {hostBS = "example.com"},
                            authorityPort = Nothing
                          }
                      ),
                  uriPath = "",
                  uriQuery = Query {queryPairs = []},
                  uriFragment = Nothing
                }
          )
    }

testObject_ConversationCode_user_15 :: ConversationCode
testObject_ConversationCode_user_15 =
  ConversationCode
    { conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("4VKcH84qTo23jJvTUv8O")))))},
      conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("wUEY-g7")))))},
      conversationUri =
        Just
          ( coerce
              URI
                { uriScheme = Scheme {schemeBS = "https"},
                  uriAuthority =
                    Just
                      ( Authority
                          { authorityUserInfo = Nothing,
                            authorityHost = Host {hostBS = "example.com"},
                            authorityPort = Nothing
                          }
                      ),
                  uriPath = "",
                  uriQuery = Query {queryPairs = []},
                  uriFragment = Nothing
                }
          )
    }

testObject_ConversationCode_user_16 :: ConversationCode
testObject_ConversationCode_user_16 =
  ConversationCode
    { conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("2zRLkYlMAPXsQKL1CM3e")))))},
      conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("XXxpZiqgIwPzcfUBdZh0")))))},
      conversationUri =
        Just
          ( coerce
              URI
                { uriScheme = Scheme {schemeBS = "https"},
                  uriAuthority =
                    Just
                      ( Authority
                          { authorityUserInfo = Nothing,
                            authorityHost = Host {hostBS = "example.com"},
                            authorityPort = Nothing
                          }
                      ),
                  uriPath = "",
                  uriQuery = Query {queryPairs = []},
                  uriFragment = Nothing
                }
          )
    }

testObject_ConversationCode_user_17 :: ConversationCode
testObject_ConversationCode_user_17 =
  ConversationCode
    { conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("KXXYAV8HosezGSSpNhqR")))))},
      conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("-aQqXyAyXO21ucrJhBQ")))))},
      conversationUri =
        Just
          ( coerce
              URI
                { uriScheme = Scheme {schemeBS = "https"},
                  uriAuthority =
                    Just
                      ( Authority
                          { authorityUserInfo = Nothing,
                            authorityHost = Host {hostBS = "example.com"},
                            authorityPort = Nothing
                          }
                      ),
                  uriPath = "",
                  uriQuery = Query {queryPairs = []},
                  uriFragment = Nothing
                }
          )
    }

testObject_ConversationCode_user_18 :: ConversationCode
testObject_ConversationCode_user_18 =
  ConversationCode
    { conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("jwP8Otm06gVqxYkvSEmm")))))},
      conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("tSN=4EHIKYVAJz=Ycax")))))},
      conversationUri =
        Just
          ( coerce
              URI
                { uriScheme = Scheme {schemeBS = "https"},
                  uriAuthority =
                    Just
                      ( Authority
                          { authorityUserInfo = Nothing,
                            authorityHost = Host {hostBS = "example.com"},
                            authorityPort = Nothing
                          }
                      ),
                  uriPath = "",
                  uriQuery = Query {queryPairs = []},
                  uriFragment = Nothing
                }
          )
    }

testObject_ConversationCode_user_19 :: ConversationCode
testObject_ConversationCode_user_19 =
  ConversationCode
    { conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("gk_qLO=OSCUiGqJNRfE3")))))},
      conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("FHEnKf")))))},
      conversationUri =
        Just
          ( coerce
              URI
                { uriScheme = Scheme {schemeBS = "https"},
                  uriAuthority =
                    Just
                      ( Authority
                          { authorityUserInfo = Nothing,
                            authorityHost = Host {hostBS = "example.com"},
                            authorityPort = Nothing
                          }
                      ),
                  uriPath = "",
                  uriQuery = Query {queryPairs = []},
                  uriFragment = Nothing
                }
          )
    }

testObject_ConversationCode_user_20 :: ConversationCode
testObject_ConversationCode_user_20 =
  ConversationCode
    { conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("xCco2bPKwJ6xf0DGH2a7")))))},
      conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("3jUkNeT")))))},
      conversationUri =
        Just
          ( coerce
              URI
                { uriScheme = Scheme {schemeBS = "https"},
                  uriAuthority =
                    Just
                      ( Authority
                          { authorityUserInfo = Nothing,
                            authorityHost = Host {hostBS = "example.com"},
                            authorityPort = Nothing
                          }
                      ),
                  uriPath = "",
                  uriQuery = Query {queryPairs = []},
                  uriFragment = Nothing
                }
          )
    }

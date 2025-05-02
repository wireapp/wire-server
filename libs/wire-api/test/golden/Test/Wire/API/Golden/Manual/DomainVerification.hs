module Test.Wire.API.Golden.Manual.DomainVerification where

import Data.Misc
import Data.UUID qualified as UUID
import Imports
import SAML2.WebSSO.Types qualified as SAML
import URI.ByteString
import Wire.API.EnterpriseLogin
import Wire.API.Routes.Public.Brig.DomainVerification

testObject_DomainRedirectResponseV8_1 :: DomainRedirectResponseV8
testObject_DomainRedirectResponseV8_1 =
  DomainRedirectResponse
    { propagateUserExists = False,
      redirect = None
    }

testObject_DomainRedirectResponseV8_2 :: DomainRedirectResponseV8
testObject_DomainRedirectResponseV8_2 =
  DomainRedirectResponse
    { propagateUserExists = False,
      redirect = Locked
    }

testObject_DomainRedirectResponseV8_3 :: DomainRedirectResponseV8
testObject_DomainRedirectResponseV8_3 =
  DomainRedirectResponse
    { propagateUserExists = True,
      redirect =
        SSO
          ( SAML.IdPId $
              fromJust (UUID.fromString "abf7c0b2-f4e6-4588-8fbb-3b4bf2344284")
          )
    }

testObject_DomainRedirectResponseV8_4 :: DomainRedirectResponseV8
testObject_DomainRedirectResponseV8_4 =
  DomainRedirectResponse
    { propagateUserExists = False,
      redirect =
        Backend
          (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://wire.example.com/")))
          Nothing
    }

testObject_DomainRedirectResponseV8_5 :: DomainRedirectResponseV8
testObject_DomainRedirectResponseV8_5 =
  DomainRedirectResponse
    { propagateUserExists = False,
      redirect =
        Backend
          (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://wire.example.com/")))
          (Just (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://webapp.wire.example.com/"))))
    }

testObject_DomainRedirectResponseV8_6 :: DomainRedirectResponseV8
testObject_DomainRedirectResponseV8_6 =
  DomainRedirectResponse
    { propagateUserExists = False,
      redirect = NoRegistration
    }

testObject_DomainRedirectResponseV8_7 :: DomainRedirectResponseV8
testObject_DomainRedirectResponseV8_7 =
  DomainRedirectResponse
    { propagateUserExists = False,
      redirect = PreAuthorized
    }

testObject_DomainRedirectResponseV9_1 :: DomainRedirectResponseV9
testObject_DomainRedirectResponseV9_1 =
  DomainRedirectResponse
    { propagateUserExists = False,
      redirect = None
    }

testObject_DomainRedirectResponseV9_2 :: DomainRedirectResponseV9
testObject_DomainRedirectResponseV9_2 =
  DomainRedirectResponse
    { propagateUserExists = False,
      redirect = Locked
    }

testObject_DomainRedirectResponseV9_3 :: DomainRedirectResponseV9
testObject_DomainRedirectResponseV9_3 =
  DomainRedirectResponse
    { propagateUserExists = True,
      redirect =
        SSO
          ( SAML.IdPId $
              fromJust (UUID.fromString "abf7c0b2-f4e6-4588-8fbb-3b4bf2344284")
          )
    }

testObject_DomainRedirectResponseV9_4 :: DomainRedirectResponseV9
testObject_DomainRedirectResponseV9_4 =
  DomainRedirectResponse
    { propagateUserExists = False,
      redirect =
        Backend
          (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://wire.example.com/")))
          Nothing
    }

testObject_DomainRedirectResponseV9_5 :: DomainRedirectResponseV9
testObject_DomainRedirectResponseV9_5 =
  DomainRedirectResponse
    { propagateUserExists = False,
      redirect =
        Backend
          (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://wire.example.com/")))
          (Just (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://webapp.wire.example.com/"))))
    }

testObject_DomainRedirectResponseV9_6 :: DomainRedirectResponseV9
testObject_DomainRedirectResponseV9_6 =
  DomainRedirectResponse
    { propagateUserExists = False,
      redirect = NoRegistration
    }

testObject_DomainRedirectResponseV9_7 :: DomainRedirectResponseV9
testObject_DomainRedirectResponseV9_7 =
  DomainRedirectResponse
    { propagateUserExists = False,
      redirect = PreAuthorized
    }

testObject_DomainRedirectConfigV8_1 :: DomainRedirectConfigV8
testObject_DomainRedirectConfigV8_1 = DomainRedirectConfigRemoveV8

testObject_DomainRedirectConfigV8_2 :: DomainRedirectConfigV8
testObject_DomainRedirectConfigV8_2 =
  DomainRedirectConfigBackendV8
    (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://wire.example.com/")))
    (Just (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://webapp.wire.example.com/"))))

testObject_DomainRedirectConfigV8_3 :: DomainRedirectConfigV8
testObject_DomainRedirectConfigV8_3 =
  DomainRedirectConfigBackendV8
    (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://wire.example.com/")))
    Nothing

testObject_DomainRedirectConfigV8_4 :: DomainRedirectConfigV8
testObject_DomainRedirectConfigV8_4 = DomainRedirectConfigNoRegistrationV8

testObject_DomainRedirectConfigV9_1 :: DomainRedirectConfigV9
testObject_DomainRedirectConfigV9_1 = DomainRedirectConfigRemoveV9

testObject_DomainRedirectConfigV9_2 :: DomainRedirectConfigV9
testObject_DomainRedirectConfigV9_2 =
  DomainRedirectConfigBackendV9
    (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://wire.example.com/")))
    (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://webapp.wire.example.com/")))

-- The WebApp URL is mandatory. Thus, there's no test for V9 which is analog to testObject_DomainRedirectConfigV8_3.
testObject_DomainRedirectConfigV9_4 :: DomainRedirectConfigV9
testObject_DomainRedirectConfigV9_4 = DomainRedirectConfigNoRegistrationV9

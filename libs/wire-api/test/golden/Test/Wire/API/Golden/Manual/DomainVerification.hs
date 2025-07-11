module Test.Wire.API.Golden.Manual.DomainVerification where

import Data.Misc
import Data.UUID qualified as UUID
import Imports
import SAML2.WebSSO.Types qualified as SAML
import URI.ByteString
import Wire.API.EnterpriseLogin
import Wire.API.Routes.Public.Brig.DomainVerification

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

testObject_DomainRedirectResponseV10_1 :: DomainRedirectResponseV10
testObject_DomainRedirectResponseV10_1 =
  DomainRedirectResponse
    { propagateUserExists = False,
      redirect = None
    }

testObject_DomainRedirectResponseV10_2 :: DomainRedirectResponseV10
testObject_DomainRedirectResponseV10_2 =
  DomainRedirectResponse
    { propagateUserExists = False,
      redirect = Locked
    }

testObject_DomainRedirectResponseV10_3 :: DomainRedirectResponseV10
testObject_DomainRedirectResponseV10_3 =
  DomainRedirectResponse
    { propagateUserExists = True,
      redirect =
        SSO
          ( SAML.IdPId $
              fromJust (UUID.fromString "abf7c0b2-f4e6-4588-8fbb-3b4bf2344284")
          )
    }

testObject_DomainRedirectResponseV10_4 :: DomainRedirectResponseV10
testObject_DomainRedirectResponseV10_4 =
  DomainRedirectResponse
    { propagateUserExists = False,
      redirect =
        Backend
          (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://wire.example.com/")))
          Nothing
    }

testObject_DomainRedirectResponseV10_5 :: DomainRedirectResponseV10
testObject_DomainRedirectResponseV10_5 =
  DomainRedirectResponse
    { propagateUserExists = False,
      redirect =
        Backend
          (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://wire.example.com/")))
          (Just (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://webapp.wire.example.com/"))))
    }

testObject_DomainRedirectResponseV10_6 :: DomainRedirectResponseV10
testObject_DomainRedirectResponseV10_6 =
  DomainRedirectResponse
    { propagateUserExists = False,
      redirect = NoRegistration
    }

testObject_DomainRedirectResponseV10_7 :: DomainRedirectResponseV10
testObject_DomainRedirectResponseV10_7 =
  DomainRedirectResponse
    { propagateUserExists = False,
      redirect = PreAuthorized
    }

testObject_DomainRedirectConfigV9_1 :: DomainRedirectConfigV9
testObject_DomainRedirectConfigV9_1 = DomainRedirectConfigRemoveV9

testObject_DomainRedirectConfigV9_2 :: DomainRedirectConfigV9
testObject_DomainRedirectConfigV9_2 =
  DomainRedirectConfigBackendV9
    (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://wire.example.com/")))
    (Just (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://webapp.wire.example.com/"))))

testObject_DomainRedirectConfigV9_3 :: DomainRedirectConfigV9
testObject_DomainRedirectConfigV9_3 =
  DomainRedirectConfigBackendV9
    (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://wire.example.com/")))
    Nothing

testObject_DomainRedirectConfigV9_4 :: DomainRedirectConfigV9
testObject_DomainRedirectConfigV9_4 = DomainRedirectConfigNoRegistrationV9

testObject_DomainRedirectConfig_1 :: DomainRedirectConfig
testObject_DomainRedirectConfig_1 = DomainRedirectConfigRemove

testObject_DomainRedirectConfig_2 :: DomainRedirectConfig
testObject_DomainRedirectConfig_2 =
  DomainRedirectConfigBackend
    (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://wire.example.com/")))
    (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://webapp.wire.example.com/")))

-- The WebApp URL is mandatory. Thus, there's no test for V9 which is analog to testObject_DomainRedirectConfigV9_3.
testObject_DomainRedirectConfig_4 :: DomainRedirectConfig
testObject_DomainRedirectConfig_4 = DomainRedirectConfigNoRegistration

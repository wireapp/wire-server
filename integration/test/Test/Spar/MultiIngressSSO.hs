module Test.Spar.MultiIngressSSO where

import API.BrigInternal
import API.Common
import API.GalleyInternal
import GHC.Stack
import SetupHelpers
import Testlib.Prelude

testMultiIngressSSO :: (HasCallStack) => App ()
testMultiIngressSSO = do
  withModifiedBackend
    def
      { sparCfg =
          removeField "saml.spSsoUri"
            >=> removeField "saml.spAppUri"
            >=> removeField "saml.contacts"
            >=> setField
              "saml.spDomainConfigs"
              ( object
                  [ "nginz-https.ernie.example.com"
                      .= object
                        [ "spAppUri" .= "https://webapp.ernie.example.com",
                          "spSsoUri" .= "https://nginz-https.ernie.example.com/sso",
                          "contacts" .= [object ["type" .= "ContactTechnical"]]
                        ]
                  ]
              )
      }
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner tid "sso" "enabled"
      emailDomain <- randomDomain

      (idp, idpMeta) <- registerTestIdPWithMetaWithPrivateCreds owner
      idpId <- asString $ idp.json %. "id"

      let email = "user@" <> emailDomain
      void $ loginWithSamlWithZHost (Just "nginz-https.ernie.example.com") domain True tid email (idpId, idpMeta)
      activateEmail domain email
      getUsersByEmail domain [email] `bindResponse` \res -> do
        res.status `shouldMatchInt` 200
        user <- res.json >>= asList >>= assertOne
        user %. "status" `shouldMatch` "active"
        user %. "email" `shouldMatch` email

-- TODO: Assert the non-happy path for Bert

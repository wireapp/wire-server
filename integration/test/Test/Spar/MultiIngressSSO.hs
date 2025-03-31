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
                        ],
                    "nginz-https.bert.example.com"
                      .= object
                        [ "spAppUri" .= "https://webapp.bert.example.com",
                          "spSsoUri" .= "https://nginz-https.bert.example.com/sso",
                          "contacts" .= [object ["type" .= "ContactTechnical"]]
                        ]
                  ]
              )
      }
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner tid "sso" "enabled"

      (idp, idpMeta) <- registerTestIdPWithMetaWithPrivateCreds owner
      idpId <- asString $ idp.json %. "id"

      ernieEmail <- ("ernie@" <>) <$> randomDomain
      void $ loginWithSamlWithZHost (Just "nginz-https.ernie.example.com") domain True tid ernieEmail (idpId, idpMeta)
      activateEmail domain ernieEmail
      getUsersByEmail domain [ernieEmail] `bindResponse` \res -> do
        res.status `shouldMatchInt` 200
        user <- res.json >>= asList >>= assertOne
        user %. "status" `shouldMatch` "active"
        user %. "email" `shouldMatch` ernieEmail

      bertEmail <- ("bert@" <>) <$> randomDomain
      void $ loginWithSamlWithZHost (Just "nginz-https.bert.example.com") domain True tid bertEmail (idpId, idpMeta)
      activateEmail domain bertEmail
      getUsersByEmail domain [bertEmail] `bindResponse` \res -> do
        res.status `shouldMatchInt` 200
        user <- res.json >>= asList >>= assertOne
        user %. "status" `shouldMatch` "active"
        user %. "email" `shouldMatch` bertEmail

-- TODO: Assert the non-happy path for Kermit

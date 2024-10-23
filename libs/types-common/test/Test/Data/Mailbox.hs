module Test.Data.Mailbox (tests) where

import Data.ByteString.UTF8 qualified as UTF8BS
import Data.Mailbox
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Text.Email.Parser

validAddresses :: [(ByteString, Mailbox)]
validAddresses =
  [ ("john@doe.example", Mailbox Nothing $ unsafeEmailAddress "john" "doe.example"),
    ("<john@doe.example>", Mailbox Nothing $ unsafeEmailAddress "john" "doe.example"),
    ("John Doe<john@doe.example>", Mailbox (Just ["John", "Doe"]) $ unsafeEmailAddress "john" "doe.example"),
    ("John Doe <john@doe.example>", Mailbox (Just ["John", "Doe"]) $ unsafeEmailAddress "john" "doe.example"),
    ("John Q. Doe <john@doe.example>", Mailbox (Just ["John", "Q", ".", "Doe"]) $ unsafeEmailAddress "john" "doe.example"),
    ("\"John Doe\" <john@doe.example>", Mailbox (Just ["John Doe"]) $ unsafeEmailAddress "john" "doe.example"),
    ("\"John Doe\" (My Best Friend) <john@doe.example>", Mailbox (Just ["John Doe"]) $ unsafeEmailAddress "john" "doe.example"),
    ("\"John@Doe.Example\" (My Friend @ Doe) <john@doe.example>", Mailbox (Just ["John@Doe.Example"]) $ unsafeEmailAddress "john" "doe.example"),
    ("\"John    Doe\" (My Best Friend) <john@doe.example>", Mailbox (Just ["John    Doe"]) $ unsafeEmailAddress "john" "doe.example"),
    ("\"John \\\"The J\\\" Doe\" <john@doe.example>", Mailbox (Just ["John \"The J\" Doe"]) $ unsafeEmailAddress "john" "doe.example"),
    ("\"John not \\tab\" <john@doe.example>", Mailbox (Just ["John not tab"]) $ unsafeEmailAddress "john" "doe.example"),
    ("\"John [Quoted Special]\" <john@doe.example>", Mailbox (Just ["John [Quoted Special]"]) $ unsafeEmailAddress "john" "doe.example"),
    ("\"John <smart@hacker.example>\" <john@doe.example>", Mailbox (Just ["John <smart@hacker.example>"]) $ unsafeEmailAddress "john" "doe.example"),
    ("\"John \r\n NewLine\" <john@doe.example>", Mailbox (Just ["John \r\n NewLine"]) $ unsafeEmailAddress "john" "doe.example"),
    ("\"John Doe\" <(local comment)john(local trailing comment)@doe.example>", Mailbox (Just ["John Doe"]) $ unsafeEmailAddress "john" "doe.example"),
    ("\"John Doe\" <(local comment)\"john\"(local trailing comment)@doe.example>", Mailbox (Just ["John Doe"]) $ unsafeEmailAddress "\"john\"" "doe.example"),
    ("\"John Doe\" <\"john@funkylocal\"@doe.example>", Mailbox (Just ["John Doe"]) $ unsafeEmailAddress "\"john@funkylocal\"" "doe.example"),
    ("\"John Doe\" <john@doe.example> (trailing comments)", Mailbox (Just ["John Doe"]) $ unsafeEmailAddress "john" "doe.example"),
    ("\"John Doe\" <john@[funky@domain.example]>", Mailbox (Just ["John Doe"]) $ unsafeEmailAddress "john" "[funky@domain.example]"),
    ("\"John Doe\" <john@(domain comment)[doe.example](trailing domain comment)>", Mailbox (Just ["John Doe"]) $ unsafeEmailAddress "john" "[doe.example]"),
    -- This is wrong, but its how the `email-validate` library does it
    ("\"John Doe\" <\"john (not comment)\"@doe.example>", Mailbox (Just ["John Doe"]) $ unsafeEmailAddress "\"john(notcomment)\"" "doe.example")
  ]

invalidAddresses :: [ByteString]
invalidAddresses =
  [ "john",
    "john@",
    "@doe.example",
    "\"john@doe.example\"",
    "(john@doe.example)",
    "\"John UnendingQuote <john@doe.example>",
    "John [Unquoted Special] <john@doe.example>",
    "<first@mail.example> <second@mail.example>",
    "\"John \n NoCR\" <john@doe.example>",
    "\"John \r NoLF\" <john@doe.example>"
  ]

tests :: TestTree
tests =
  testGroup "Mailbox" $
    [ testGroup "valid addressses" $
        map
          ( \(addr, expected) ->
              testCase (UTF8BS.toString addr) $
                Right expected @=? parseMailbox addr
          )
          validAddresses,
      testGroup "invalid addresses" $
        map
          ( \addr ->
              testCase (UTF8BS.toString addr) $
                case parseMailbox addr of
                  Left _ -> pure ()
                  Right mb -> assertFailure $ "Expected to fail parising, but got: " <> show mb
          )
          invalidAddresses
    ]

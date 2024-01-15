module Testlib.XML where

import qualified Data.Array as Array
import Data.Fixed
import Data.Time
import Testlib.Types
import qualified Text.Regex.Base as Regex
import qualified Text.Regex.TDFA.String as Regex
import Text.XML.Light
import Prelude

saveXMLReport :: TestSuiteReport -> FilePath -> IO ()
saveXMLReport report output =
  writeFile output $ showTopElement $ xmlReport report

xmlReport :: TestSuiteReport -> Element
xmlReport report =
  unode
    "testsuites"
    ( Attr (unqual "name") "wire-server",
      testSuiteElements
    )
  where
    testSuiteElements =
      unode
        "testsuite"
        ( attrs,
          map encodeTestCase report.cases
        )
    attrs =
      [ Attr (unqual "name") "integration",
        Attr (unqual "tests") $ show $ length report.cases,
        Attr (unqual "failures") $ show $ length $ filter (\testCase -> testCase.result /= TestSuccess) report.cases,
        Attr (unqual "time") $ showFixed True $ nominalDiffTimeToSeconds $ sum $ map (.time) report.cases
      ]

encodeTestCase :: TestCaseReport -> Element
encodeTestCase TestCaseReport {..} =
  unode "testcase" (attrs, content)
  where
    attrs =
      [ Attr (unqual "name") name,
        Attr (unqual "time") (showFixed True (nominalDiffTimeToSeconds time))
      ]
    content = case result of
      TestSuccess -> []
      TestFailure msg -> [failure msg]
    failure msg = unode "failure" (blank_cdata {cdData = dropConsoleFormatting msg})

    -- Drops ANSI control characters which might be used to set colors.
    -- Including these breaks XML, there is not much point encoding them.
    dropConsoleFormatting input =
      let regex = Regex.makeRegex "\x1b\\[[0-9;]*[mGKHF]" :: Regex.Regex
          matches = Regex.matchAll regex input
          dropMatch (offset, len) input' =
            let (begining, rest) = splitAt offset input'
                (_, end) = splitAt len rest
             in begining <> end
          matchTuples = map (Array.! 0) matches
       in foldr dropMatch input matchTuples

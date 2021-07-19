import Data.ProtoLens.Setup
import Distribution.Simple

main :: IO ()
main =
  defaultMainWithHooks
    . generatingProtos "types-common-journal/proto"
    . generatingProtos "wire-message-proto-lens/generic-message-proto/proto"
    $ simpleUserHooks

import Imports
import Jwt.Tools (testFfi)

main :: IO ()
main = do
  putStrLn "test FFI"
  testFfi
  putStrLn "finished"

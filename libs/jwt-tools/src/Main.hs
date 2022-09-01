import Imports
import Jwt.Tools (testHaskellApi)

main :: IO ()
main = do
  putStrLn "test FFI"
  testHaskellApi
  putStrLn "finished"

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wwarn #-}

module Test.Wire.API.Password where

import Control.Concurrent.Async
import Crypto.KDF.Argon2 qualified as Argon2
import Data.Misc
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Password

tests :: TestTree
tests =
  testGroup "Password" $
    []

-- testCase "hash password argon2id" testHashPasswordArgon2id,
-- testCase "update pwd hash" testUpdateHash,
-- testCase "verify old scrypt password still works" testHashingOldScrypt,
-- testCase "test hash scrypt" testHashPasswordScrypt

defaultOptions :: Argon2.Options
defaultOptions =
  let hashParallelism = 4
   in Argon2.Options
        { variant = Argon2.Argon2id,
          version = Argon2.Version13,
          iterations = 1,
          parallelism = hashParallelism,
          -- This needs to be min 8 * hashParallelism, otherewise we get an
          -- unsafe error
          memory = 8 * hashParallelism
        }

-- testHashPasswordScrypt :: IO ()
-- testHashPasswordScrypt = do
--   pwd <- genPassword
--   hashed <- mkSafePassword defaultOptions pwd
--   let (correct, status) = verifyPasswordWithStatus pwd hashed
--   assertBool "Password could not be verified" correct
--   assertEqual "Password could not be verified" status PasswordStatusOk

-- testHashPasswordArgon2id :: IO ()
-- testHashPasswordArgon2id = do
--   pwd <- genPassword
--   hashed <- mkSafePassword defaultOptions pwd
--   let (correct, status) = verifyPasswordWithStatus pwd hashed
--   assertEqual "Password could not be verified" status PasswordStatusOk
--   assertBool "Password could not be verified" correct

-- testUpdateHash :: IO ()
-- testUpdateHash = do
--   let orig = plainTextPassword8Unsafe "Test password scrypt to argon2id."
--       -- password hashed with scrypt and random salt
--       Right expected = parsePassword "14|8|1|ktYx5i1DMOEfm+tXpw9i7ZVPdeqbxgxYxUbmDVLSAzQ=|Fzy0sNfXQQnJW98ncyN51PUChFWH1tpVJCxjz5JRZEReVa0//zJ6MeopiEh84Ny8lzwdvRPHDqnSS/lkPEB7Ow=="
--       -- password re-hashed with argon2id and re-used salt for simplicity
--       -- verify password with scrypt
--       (correct, status) = verifyPasswordWithStatus orig expected

--   newHash <- either assertFailure pure $ parsePassword "$argon2id$v=19$m=4194304,t=1,p=8$lj6+HdIcCpO1zvz8An56fg$Qx8OzYTq0hDNqGG9tW1dug"
--   assertBool "Password did not match hash." correct
--   assertEqual "Password could not be verified" status PasswordStatusNeedsUpdate

--   -- verify again with argon2id
--   let (correctNew, statusNew) = verifyPasswordWithStatus orig newHash
--   assertBool "Password hash update failed." correctNew
--   assertEqual "Password could not be verified" statusNew PasswordStatusOk

-- testHashingOldScrypt :: IO ()
-- testHashingOldScrypt =
--   forConcurrently_ pwds $ \pwd -> do
--     let orig = plainTextPassword8Unsafe (fst pwd)
--         Right expected = parsePassword (snd pwd)
--         (correct, status) = verifyPasswordWithStatus orig expected
--     assertBool "Password did not match hash." correct
--     assertEqual "Password could not be verified" status PasswordStatusNeedsUpdate
--   where
--     -- Password and hashes generated using the old code, but verified using the new one.
--     pwds =
--       [ ("lwhYzCTR1vT+I+pP", "14|8|1|29yZquOYjEsU69/L/pWGo5gbfVRUT7OnI3gRaUUwQK8=|GEXbL+TScJ6hvF6Xk+bEJLN2Ao0GNz92kjrCHS8WMZi4ZvALwzlLHjfRz5nIVn8v282w3hZrAQp5z1LaZSigDg=="),
--         ("R/ykkySgkOHt1K0J", "14|8|1|1AacEchBw5IW6k1bDgKGqHiqTgz8eAaVsm5Rc6sP+Ys=|CHuLwmIjYYtWMlRcTEp8Wrc6VKx1j0WN1T3LKsbRyMwIKLlW6Q2bPqWTQe8Koqic6ElSwnrhrvMyKP3v6CVzTw=="),
--         ("h/YexcYT0wTFIZ9Z", "14|8|1|0OKhg4G9Kyl1Wv+FjTpF9ZeDSR0NtUvHQBx5JyuXNqI=|ir30m+39b4fDjUkPGq/wxPZp/CbRMH1BJq8xOD7w56chh8+NuVEOHnIRy1WJzwhvOFC3CDg2pWsRUlXkVzc/Wg=="),
--         ("fp7Sy0I6d2eGTfE0", "14|8|1|VOlTLWsX/cHJ/U6krgtZHvhqHaBMangPrn17VmJmbiA=|Hv/DOHjdlI1O1z91RuqdbX07FB8VXPczdbdkCA89AbhDdTrmNXIuPsmRiMH4AT5xFTlVUl3iz1JJglJVC4oGsw=="),
--         ("449YLdcsJBHpSCRo", "14|8|1|t4tH+FEQYotXzHkvJI/eCXo2POnoW8X+QlvuPEKIPwg=|ouZUgnSmX1cKC7IWxDCce+c4eAeV5nNpOSLnVneUuXhHg7OpDh1geGx4ZuQoFh1rgrXABv8rN63tUBRZa9BwpQ=="),
--         ("YM54PEf5tbxvgHDA", "14|8|1|1y60wx8ZUDMvKMotpOq6/wQaNiLaXuxIZ+v0Ta0BBlc=|4nFAKRB2XO7pDFKBgyiGGCzdSrfxToOXGt/7C4Nd0DUAdwtC0xjsxIYDxz4H2a//7+jkOJEtf2ZvErL5bQnj4w=="),
--         ("mJ9ahzt6t7bRyUdn", "14|8|1|X/giroOjUv9rhkg8+LUim74mns8ukzzPkuSlZJ2yh6o=|HSpPoq3MArmUNa1XtlQtaE0nIRvY6NjBBeAlpFRCbYJD24mMD7x8oCruk3g6WhZaqsEJlAOc/4YsXNM2hoqDxg=="),
--         ("Z6xgZY2qZVpbMXiP", "14|8|1|YIndWYeHLhN9+lcfaQhDGX/59Uwp1uk3gNKo6gu8cN8=|S4OlvgGXYmdYbc0ISBeBmQziMzw43DvAQX23I4wen36prNM0+3tlneUk3R9DSpWd7cq4fNJ+86sKBRqluUY4OA=="),
--         ("rGW9WwUA7SF/rXKg", "14|8|1|/yMWRpHNC9W1eWvWq2pw9HZ+tNiYZxkawo8C6MUUejU=|UPQ2Qxg9MWSU+h7T8vDBHO8SS1nu0btOwCLYqUzNUJmKGzMXmqaJIZNfcHjMCcPpOJ5h2kom8OryCNmCT8E8/w=="),
--         ("H/NZgddnVnXnHEvl", "14|8|1|kCB5tpixot7aFdqUCDvgcx7DVJVtB0wkEbcibU4t0Ow=|VYBnPFwQ1RIu3+dl7jMNZ8FhS+cOtAYzEdousq7b7usEYMC1x2X4BpCN1p4HZdOSeUMfyLYLcPWGnDmZV8AxXQ=="),
--         ("+rxjCEFJUlPcTw03", "14|8|1|rh8QqsT7mVK6OFc44KAjND/mnUJ9FVZu3UUAaJg5DtQ=|Vp8yFa+/KRbn9LX6QJtojj9HUD+75+aEJcpKAgZVjgw5VTp4vBHo+o4ODz3iecZOwoqJekdeXhSrxFt1XqZH8Q=="),
--         ("3RhBMjbTXdxswtMM", "14|8|1|w6KBOim6WjBjwwX3BnclATljZr4L/MGs3xHD47XXqgY=|ipORYUItqpATtgIZRyr34i8a7DMIKPHTprj/Ed2E0he26dUnV0baJW/2dT8krHkOmYnxcR34173VttxfXK0nlA=="),
--         ("L7EdjHTHOG0syQ/6", "14|8|1|JvcFlj2dg9MidZwVpd5EPx9/QwXyeXngG8k9PpLbMq4=|2Bq0ZbhxGFzjgEb2BhBMve8DHDTSOG2wRu1MctM4Twl+HJkRLooaae1T3+iTrUUruMepJ9u+h/wcSGxmRbGy5w=="),
--         ("QowqJd9d8rlhbuPi", "14|8|1|HUep9F/ykGh114ydrrQssbO1EuhPTfKJ/exe0ygPqKs=|2FFjKYmeytf+mV7ASOtuCf2/cexG4mouLfv6N/xfodKCdWG9CS0CrP5YB29+G28gZuILcab7BE00rutNctwH1g=="),
--         ("nkLknHOIyrb2QUFJ", "14|8|1|kO5Xx9EWiGnznsVscifdwRnr8c755iTnhl9/qJMTtV0=|uWP+kUY/Xm4XAwTwW9B9lf/CpmLQqjlTdhqcwO9hrqkFnVGcxAMP2x916yqyxSkaqoZr2bVJFiIZ5ZbXrjXttw=="),
--         ("9B0Y7aWla9nPYUbp", "14|8|1|VZSpYCm0Lu67iHcRcEpbKexv9EYY2sal4c30ppPF0Qc=|ko6TXDjthYNCARXdAR8RSYP//Fx7FxcrEGBgRsVPw46Kt3SxKrOpwQYXraQttkvUWMBF5xc8YbFLqSo81G2vqw=="),
--         ("o3VBCRWV0bAbgWnv", "14|8|1|D5aDM6SHFX+FEibenKs5zUiclcz0T56lZpWohDqZJoU=|8p7bQIvx4iTQdlcTNNli0E/wplb0XN+XzDd6uEZveSHwcjeqmIWt1xX/OhHXRcOaC6xKq0eU7oAoUGGiOnt4WQ=="),
--         ("z/6/bTEs+wSvsIUd", "14|8|1|+dpZximHa5f8XS82y2hLbgaKkWXQmS42PQpTP7tLt7A=|lJ7EBwXozSoYQLRyLTtzwWNtHV2VOxE10cnqwTkMQqDdSB/zzFEcTIk/ElgMJZetF7bTjWf7sE1WU3VHN3501w=="),
--         ("vf1i8JYTbpN3V5O5", "14|8|1|CkH8gPbM/M+6fxl8Rms6jfTNGiPZmPT+/Ki4M64U77g=|eqLAEPG+9eZ2quo9TUThPP2NwD8DQ5/dd3aI38Si2PQAkN5MJTfbXRPGrDzLmp051F1GyKqbRBcYRWQgI8HFFA=="),
--         ("pup6bBbVPAb+edar", "14|8|1|E72fxikULVIl54GksditvLKJsPFzt8RNDNcGZbUNmTI=|hCSBoy6ZlfxNO0ChAegfUFxl5IGKF/ZIO2DJKOUG2RBr8n6KImBpgUQSEo1jsR9I/DrzRuCw7UL9fd4cKf+W7Q=="),
--         ("Qrg05H92ntHnN2K+", "14|8|1|d6kJMrJltSAh43q3IX9/X5fFa27D7zYWkIsom0ujUH4=|uN6rLZCwQi2jlKAwbC3Msm8pSJJvlISmij6NIz3jmfaJg7zRdOzzzqvE+5bdrf4fAB5EzIpTOmjPYANUTdCRiA=="),
--         ("oBk+Q+WXzocciKaW", "14|8|1|on4eGwZ6/5xSAltQpBvpkuMDEE0ToF97wW5TC8CsuhQ=|u8iakhgtB3ZQSin5+5ua/44kN/HzBigrWDfTKej/tUG1SBcVmYM7x8kNUjOVMF0Wrn8hSMhrzpKlNQTT7Q40Gg=="),
--         ("vSw7uJWJCbjIOcJ9", "14|8|1|p5iimRt8btc0m2hhLneJ/3xUIH1L78DJWh9RgwKsOCA=|RwVZLSgCzTpW7r1sb7Visit9HKlwf0g/F1x+fCjeJNxyEwSO1r8vYM0zVX/MiQYX7KvW9mao7HAZgEL57hg5mg=="),
--         ("32ujVp96TlY7/qCW", "14|8|1|rleWgMYXT72g66DV9cLEtSLwd+tDwlhynPbOxm5C6fY=|FCs8vrRbTxcfIx+MDWwuYhnjihDXghdmvnNYMKG18vGCJoX4o0AoOaabT4RxOqWl27Iku2/7DAI5xWFmTy8f9g=="),
--         ("eT5vTKpz4xhaH9Ca", "14|8|1|OAQqP4bBYU89MIiJJkfb2lSUYijP9s9QkJP4Uv2mBq0=|JDu+VabatvtrnvpC7sjf61tnz0ESDKEnIlk0bXx9gJXcraUSvxMzu+aXWuo0lIKL+cH32A4jMSXqBWHmnTvnxw=="),
--         ("99ozkfy4O0tMoWgH", "14|8|1|qRgQHFUq/+jP4+7pA9YhgtTXOCpDxJdtsanphRaVdeo=|g/aRgHW6U8EkITttA4nJBtu3Kaz6+Ka0lmkwSKvxyMXWlfDsQ6cDwwtopQN6mnlQabOrvjo5b3YBZcmx+N4Beg=="),
--         ("bpkCrjhCA68cz7zu", "14|8|1|KdnzSkqIMLMVgB1Ykb6CkEf7/XJ4v5JzDj5GukrrPyA=|TL5PpghTgOXkGnZR0a7TZ9AgFYhOW3BKA6sp2u4mcRBK069R0Ohnnx6lRSoNLAKkfcZVP21s/qxE6lZl7kU7Lg=="),
--         ("+OhUUQfp/58Ose7e", "14|8|1|4n3CN6h88DmHN9DbqNlfYGw3Xf7lCY0wwU3YCKkIEZg=|4GfY4TR/gFi7TSRfwh8fA0Ce8Wqk/wxHE5RjhfDu+HuDgklkV8uq2JR8zRco/3GeN8G3slO+hBca+PZBeDXezw=="),
--         ("3y8YelUEHUDTE4CQ", "14|8|1|69CI1CtHh0gEd1dWw6sV67fW4oweLY2fNGRwAEFN4ac=|VPdPMyqUYuBE2BqwC/mnnJ7KHcCVO58pn1NbuJmFxcprRoPW1hYfa4aDIZxQaqJ0J/lq/yi1EC9xrRfR5Kokew=="),
--         ("rBl8TRPoog8agdqS", "14|8|1|72Abdc/fug8PuPknzl3ne1R6km99xSz5V9aBSJ72Cw4=|OPBZVbDaQQdFX8rekXjkGpxfju/tXbx7hE9lfSTTH3PWnE2XDCoENy8IKFY4r1HeYj/rbxpeKDoJ9jBTA3qa0g=="),
--         ("5V2xcDSRkOF/2XbJ", "14|8|1|WEOM8EzH6jja6US61JkEJMCVZUvPpH8J+u2KKsvaI6U=|WvkQVH35PtwZMw/SyzpZcPcppUHBCc0e9q0rwDCw+Iwr2VdyW1S3bmdobNwf5LsaWyNP1WHJRM8EUYUhVtCBYw=="),
--         ("sIOSeNsLdE5VdMqv", "14|8|1|dAk6BZ97zHSBzPeC0lPe/iiJyIRC1aEAGC0V2B+SDUU=|sbOrxZjKHNyI3zs/1O7NU7/J8ggM6E0T2hFAQHlhldF0lOU5T9FOZCtWluPxlGTKXV8k7OoUODG8G2FmriKP5w=="),
--         ("XzgftJKaEss4Kp/R", "14|8|1|C3sZ+MlncOznuy+tsRRfJ+d5seWRfhR0lYd0tiEeANU=|3BcQEgBt6Gj2Uaxi/vkTDKkEnXREu8gpoVRid4v8C2cNZsUqudAXg/xa8vKqTGn83LZn6hyfC3e/NmtikEv3AA=="),
--         ("YHBi9soc2KUag7ru", "14|8|1|mxTzz87MsBpgD+JOCgGvMZaNicI6hN3mFJV7o1K6WOg=|FY+cZkQukCqGCUvlLqZn2558e7UpR7ejUMmcbY9aBI6uph64jEezADWn9STpOt29b6yRAAP3TBGxNSAI29GzQw=="),
--         ("Z3TcCRyn+uiuw6LO", "14|8|1|ssSmWw49rT0zo7ifSGKuM2sdLuf0WtWqMQQc9nofKGQ=|8sJ73YdVLsS+RJzBcXU7QzE1fzO1YIDy3pXZTovMpOZhTZP7YMJ6P6Tz5OlLRecZy6y3BiqcuVXAtEEBBaXsng=="),
--         ("heZyZHzPqxy3ieww", "14|8|1|lFj9SeEfcHxGjYKaMqLSTdzRYM6S1ZUZ5Vp19qpx+fE=|lt3tBxV5xGhD5ig2NO1BnL3prljbibx2NhVltD1/z7lfEUPuzYcQamsRhnY7QRiTv0zgT2CubttLhAeqmXPsJA=="),
--         ("Fdc8/5Vi2EOOFLVP", "14|8|1|q9JKhlMLkfazxhdJUJ1+32P31+B9bWrsMBnPNeVDoSk=|UnEeAGfK93Y0rWv9vLZzpVUjzPvcvFCtM+tq5s/e2fUaFCSfTI+U4RlxUn79+TqsN7Ag+8Gisy+zxJlZ4KUvrA=="),
--         ("W1ljK8hDWUsZSTjN", "14|8|1|XiLRnRHXL6DZ7kGTTh+jHesWYqzR9pImoIlzvQDeJvU=|mptBqgCf5Y/PGCk1NeplHZeqJdQybeQqpajcDNp+1oVC2KUY6zpbd+F3Zh4V2cWF3kQNxhE7KcxCX3qBgjxz4Q=="),
--         ("QBgtS3SlmNz4kJO0", "14|8|1|rURQ+UVwZuoFqM+IuCehoF3sgpA993eV5WgG8xGvF98=|hluvMh8btsoNYWnqdh7GjIRteIqJExPOF6OpVyoJyoc6up7vWike8FdYWSVpdWLKGvy5eW9O5jueXCr7jsZhUw=="),
--         ("P5nNlt5J6V2WzS3B", "14|8|1|GFhLIRlCUHZxJK0cdy0pr5xrxvSdBmYUJIqsZzzS80s=|itQd6fZvOOwQ5IqXIbQFNBTzmq13oCtrXADfliUqAxxdGX/norzgZUlrhD5jtJxi102jWnD3MoxBK+yq12OE2Q=="),
--         ("RLD++VJTTStMm3Id", "14|8|1|UkI0pcb477wFMUA/KmuEQLW1FwhlLmfZvTVinvXDy78=|io/zCQi2lhLxmFE7f/4DJcodQ1VfETg//RUnH/0TlCo5hIv7Os7ST6VIKZR+Hy1J1KBSWaMvYNtMBfqmgfFzNg=="),
--         ("iaGnnjne7XU80g6W", "14|8|1|8hebt5in6IA2EhFskT3z9V8HJCUxdisFxz88CnmHE/k=|QBdlXmhwQ1gQna3dBkxsqG/ymFNVCoBCMJUGpv6sxpNnPzPQGGXhYamX6MZWNXEKDpz1F4s/1OMWeNdXBgOhjw=="),
--         ("EheWchbJc/XyB4OT", "14|8|1|Q8KoXLkAsXj2B4B5woWZYVX++rEiBuvpUxqtB/a2cDQ=|tq7nHbnO0XuoYfvr87Wlw21goi5s3WE8TEnsMpI0nxOGvqtv6ZE1X+kONSM1yRPjY8yiO+Bcz841uiTud5j4Og=="),
--         ("UH1ar3Kf3zDu5o8e", "14|8|1|5xPDQU19zk3GLpIuZFp33KKdUDMKW9oZEitOpWOaRyk=|uhi1yNJzqMjAFo2nsgHM9PWAARSJuk+AcXFyMcidROnBWG4BtNUNQK6QkYgl6t5Y8oX0kRVFl+nizQyFv0kFZQ=="),
--         ("HRX/tRDvcbAUqeWd", "14|8|1|4BpvPBPAeqKI/LRUmS9q+8HVjpgeNVNYsajaI45Ow/Q=|N0BWXr5zaSjp0nqAZq0CYAP+fuP6Oo9kcXQXYIQluGYygIh/6Zm0ibNlY6YmMKEtBDvhS/aND6FXcrN5HAhcrQ=="),
--         ("zBvOb5kb/DK5x+Jj", "14|8|1|VXJ7wrp0JdFY8hZ+WKZVqkeIc2J3/ZeYF7tUgsnPVN4=|l2QK6PcNa66fJAphrYWbH0yYf3za+ICwpjaJKER1FqaAJaUEmoRaiCjhgSaYa4gSqMwmjp1MY1WYynlUvxyWGQ=="),
--         ("zRE+1iIYQzgWxvYO", "14|8|1|BsuIqx6NtVApX9fDXNeJ8KrHlWImyR1wc2GOUqaUoXo=|xtYFwdBgpewh+i5ogNnwQteVuf57JYIj8zwtqsxLidOHIseizLiuSvEICkcZUaw5TIPeIzyx5jCkke4yTM+lDQ=="),
--         ("wto8Huuhsr11cKFQ", "14|8|1|rQvL0c97dj9NJjM+gcIxD4Tohd3pIAsdhUl3QjSg2V4=|jYva5p/Umkg3xjsRhF9/RYhGYYsNaG/GD5/FRY6IdFod+P6ZGTjSAWRCueJrm59arGZbttG71Hc95cr4fG3HFw=="),
--         ("z6AHJrHA7NwBIa/d", "14|8|1|o6PSlMLEpKZRSVLXCkKnY3A3UaArAsCdcwmsO/99JHA=|5zcK6NltxhD9BHe1roRuYWNaI8ucg2LxCIaoOv+E68lVHqWRETsLKpR9bceKg81QPXaw0e+SYbDxIJsaNvVQGQ=="),
--         ("gSbzFCU8XzxgeKQH", "14|8|1|1Oo5or8PQIcN22P9+ypF3uGzy+ZUxUUIHbkjbH5NWpg=|SqG8dIDokgEMcWvrlg/tjWxqF+LfHS0Js0awdj1RzfqtWkqFs63s8XdUse1lsikE+gHTv8xtc1g1h1V+UroAVg=="),
--         ("D53AiX/xp0rgZ7s5", "14|8|1|tM/HHNMhbMQR4ik2MfAmy7THl9dlavxlzaooM07zcIE=|mVyJ8zT8UBhQMe4/bKOGsQRsqZDB8Cj3tZGazrswXfIuntvhXWi4r3eIvczwaKfKTqMVDqsQr6+0/1oIgkUz1w=="),
--         ("iQUqz/WuvPRktiQn", "14|8|1|rxQkEvDvD4SVUlB45yTi9yNnaiaeyUqigZ9n5RsBPYY=|uYA/7pmlolZCufoDsuECsBXjAMJAQ9EySwp/P+nzyZIpQGO0F0zeoE905SFDidSVkqerzehSXqRMyke7tUXhRg=="),
--         ("NiSWNXFJGbaxbY/p", "14|8|1|r+VQ23B6cysmEEifqFqyrGVdnj36iQY4/ozVHwTqsQs=|N7uO9+oyVULCA8g0eYD3AkpBiPVBHdUc5YTPaHcL6WRQsAoMOmcg1hoxrYG1X5t+l1uLm1AMbiEyUpp0Bk8GfA=="),
--         ("Wdwx9+W/nP41bb9R", "14|8|1|NgFyjyFBoZk8gYXsDWIMDEi9va8o1D7djI8IP3Bs1Zg=|Ey2E6r9VARCkagPgZIhFcOmphYZI03Hl1m5rToSxaukHj2cp62IpcxTEslPdom5OUnRF0KnVn7c3dHoXfqG4mw=="),
--         ("EF6DlVdCTmkmEa62", "14|8|1|mziKcxzzEguNbSITnLKLoVHtOlcXoc7wYqylWNYz0Uc=|/l5t2mAykAZQwf5+aIBS0Xr6Csrh3nbJKhlEUh6Bhb1sBlsUWlKHBIUgD0/XyTOiHcCq0sBZ6qqZ1EfcaLS2FA=="),
--         ("DXHoJqjkkfwp4zD6", "14|8|1|S8NC9r637iXcL22gyrmj+wjcpvGHMjrdtVQClItsSb0=|KkyUkasCSB5p0JZ4/ywVIvXT89NMbcjbEmH6TNIQa2IU6kG1q1MMoDs2k8QfSadZ/2fSGqSVG5Nu7w3/BAsmHw=="),
--         ("uPEbfjNwDEsHjq9K", "14|8|1|6YFqf8DZ0FFQRPezrHyRyGj3wDZ8JgLuw9P09HPi6ko=|ascbRpSCVtY3kH5Dh76L9IlJzKvr5eh6h0UaaJRC2Ed5BEAYkqcIOVr8xtwznuE2yfI/2HXEeOicEyeJlAGBpw=="),
--         ("4s+QDZ8Lw1oJGMzm", "14|8|1|RXTIsiCwWL2DGXq6HR1+uAhn9yq5wkBo5geO5bAnlKA=|4tmj6wlVPWAIM0RWwFK+4ApOwzHsmpFe8vl5MQL2TEVr3kdhh6rbvjfs06YPOkPzzFcpejsK1c4iXwcaFNJbaQ=="),
--         ("3mm44+54a5ORbbRd", "14|8|1|T4k07izGiJXfzDP/Eh6/TH7+3Wj5F7JLZlGUdVx5oaA=|kfRi9sdgs0jIeBisK/dqQacEzoSsIDt+cvYmhnXvlP+538FVk4tOXsb+dCPmP2rEaNoati6a5fOrVgYfeT5XEw=="),
--         ("c9j3G7S8CyixZc9N", "14|8|1|ZFGD7nmvK2l0aDaPHzBC9fE1s2HeIuouGR4B6a/g1j4=|XKyfsMYBGyFdkcaDbCJQR9Wn1pl3SRupgJaaFk4tXvkUsGYu5UAwCmjp8pU14j2JhOv3bAMHtP4uerCT5yKUSg=="),
--         ("3gPpfO+oL1N5K3Fu", "14|8|1|KZGoIP9UrCyFofhznMQBIh8WoLa63l1SspIY72NhUHo=|e+ejdjQXiIP7+EAteHkZomUj7C6LvRemmScYunQb8tlKRi5QMpRJb7lwSYx7ojGRzNHqU0sWOfaWTz6oEJ/ICQ=="),
--         ("g7ML3IeP+khs0SSy", "14|8|1|0egQ6KLU+B4GMiKrCZmxTWwevp+U6rCWBf67dhrFafg=|hmeiPHya7EwQeJLgG0f9WtWDRMQctfn8I1h/rBJnaXRYnG8GeRdhhwKrhMcF3eLwQYHnfZUilFUcMaL6rxwf0w=="),
--         ("je2VijxSLoDDrg75", "14|8|1|hv4Xw2n4GboFTA7qv6bplrsOiQSwJE1Yu5EVTJeIrQk=|wzr9LCVkyC7960MlOxuzsSEP6oG8D8fj8wbEiVZOYARB0N7Y31or99FfKnzOqcfD5eDWOu3dV8yYFLUVLxmGZw=="),
--         ("DAYD0lnopxqOYIIs", "14|8|1|tYUrKL15r2lnn4NojhW8Mvrqfulycf104gjQJJGLoz4=|9+EiwHw5GIDF4pqsffVNn4JUQNl6xFw6uYRnnrdrcXisZwrlMStXrt5cLdCA1dOoNGDmYzik4l0GT+94tS35UQ=="),
--         ("ilteQ2iir2/9ejBz", "14|8|1|IwTA+g2qMKIanHyTLYqwa2POCwOXBiEChRJ5WwGwVu0=|n2uEgxIIi9HmUx66Ki/yF9thA3cxtRTiLAHqi+eURqIk8kTuYqTZZYRvYZDgGlfAQJIzXOaro4zfHFj+91A2yw=="),
--         ("HjUtn/E0U0LBz6I/", "14|8|1|IdnjK5GMMteiOjGxg4nxokhs+6QG+AZ5SI4/8B9LJnA=|ItKRVhGxT4ebPI3aE4Hec7zJPuM/Qj9HPCy36ipD2PYl+gUIywAVOkwaUqjlb3MpdwRAmUPPnNsLibMpBNryBA=="),
--         ("bZs290xxS2W1d2pa", "14|8|1|z+6RLzY++RkXa9HvWqRGT7/R+u7a2/dfXDtRv4QYKm8=|W6AY1mem/dlO7/WJJgwKTkRhVJfPETvcnnJ+rYwfBctv4quAbroXWLmc4PPRQnPeoj6b16L0qNCZiuu1eYbgUw=="),
--         ("T/EO2Ucc9f7lh2un", "14|8|1|29/JIxy1wCuhs61WRzbJq/d1BmTGVl0DNA9lx5VpmEc=|h1Kd9OuhaS2fzJj69KDHnpwk6xNFJ6kqnawgPErFD2RsWrukNAt22j86MR/k0CDWd/6ly+r/yg/kcv5g0xIjTA=="),
--         ("SwfYItXPlmnuxHpO", "14|8|1|EKHH4ZPi+5SDovgY5hwziRkzkU1hb4uCqDOmRr0Y1z8=|vAtwCzjfxMBGy4EXDnzNHCOZqBudovIYCDGa1dOVEPxXMoqwSVjp0BRojhUqm8QzzMQmOwC+OsLYtSuu7VwYAg=="),
--         ("Fvy36QBzrhbcHu7U", "14|8|1|legIpQi0gMMNtSL6xhDKuCV4auh2ypEJ0vBMkxesRN4=|rN4hx1ONfA30o0Yu8GJmOxwhpJz2MrjEWhGX8o6RxzUQUceu1HQrL+tXoWn3DFjbag0oYpCykWng9VJH2wB9AQ=="),
--         ("OHgOnfWQ5HFhIg+B", "14|8|1|pUk/CoNmVxePaVeZIm3GnKf2GYLBFEllpDV5epv9mgU=|d3/dOku58zczImyOd4sRMCbtO9XUHgwON2JGWeVQlOQUXRmlsHn0RVTJ24EpXYis49j4gQ/ColHKtIiPQDXHXg=="),
--         ("8HRxPBJERp64oEUx", "14|8|1|22Yg5L1MXgUehccIWI99z2we/GohpVg6wYzSlmgSTwY=|MPTBZUCr7gEKmrYF4KEoHB9yU6KQ+Q9OSe3BUvbhkYs8EndSpSbHz8B/OLzd69zCUjEGWTkIIbnH3pgc6Q7z6w=="),
--         ("vV+gN3i0x+lver4W", "14|8|1|y1n86HVpyXBK6KdFs6R0ZfTuaejuqMmNDh0EH/pqsPs=|w+I/9XbJ4/SYCglz5RiageD1GWPmqUn2vxAORKYJSOjUJDEPSGUIctTLywl4zzmgwkl5MQSaF/Lwg25zg6K4dg=="),
--         ("UlASSqWmwa2J066S", "14|8|1|XN5v3THzBrgO0tHivrny4kIE4wEAH0iqRqOEru9yXCA=|G9NWsw6j7xeFlGtTqZyupjJ6EyWQsCzQIX/y0b6sbaLPUfZQeVMXEETPzqDFyKKlbAX3GXPaNE6oTrYb339aWg=="),
--         ("nBUA1Q3NpY+Ji8FH", "14|8|1|M1e2dPC8b0dsrBA+70CkZUeNel3wEe3GMjo4TozBKzg=|+7PqBEqbTJRewlj8ZVoSn3hyNj4BKPH3mubFHXLIX3vfrxHmBkKt+/o0JmBmXzyu9Ga3V8NyJl38relrBwaK+g=="),
--         ("3LqhSwxW3HCqT43R", "14|8|1|+polYGRtygEbNc+V3D0t+enszWc0jMJ7kz+6Dx0KZJc=|HgawGwpVlZLkAUINO53kDvOJS3E93AhIulRlVzDs97svu/de4mI8lP0PZkaRtKROWMF/G5yjD6oLW/xKQSA4vg=="),
--         ("jB7vLwgLcIaUg3tR", "14|8|1|DBEoiVw7ivQ0TOtZ7ajQh0/EeIoNiXiSEtzHcCgJ+n4=|7ojF1nIAwscviV2LldP0rv0FYo5cWARVBqtWI1kmv7l6//hEibDQBWl9ChXekLqu6MQXi4KOJ048wGGB4Spipg=="),
--         ("aofZYBB/A8DNKRIi", "14|8|1|+jDrvjUSVfTfYz2yi5+Qnc32uUkDDqbXBHOeN32yV48=|Tpx3ukY1lvG7UtcnTcxAtJSdyKByRcwq43cFBTbqtaU3oDQFdTZCdo9Geu6ShJMkRpQw/Vul7goII8359MPA1A=="),
--         ("T68Sh/OP2gYC8gg9", "14|8|1|lhdGMJ5oX/ypDpcNevSaQ6wv6cD5h8H8vxAbzzFKm4s=|ba0vGowPJ6lonjZLhHtj/NTle35dofk2iWSDADG11I3UNVp5PIqo07uyzP2wiJuh5SmGyYPyft9eq6Euin97gA=="),
--         ("+XhH4l+zcwpO4D7G", "14|8|1|M15glPQjLl9inOfjejFTY/MUH/RLXwh/rYWxoNddPXw=|9mzrrAbl/mbHXPzKjEz4Y9jXQw7/tCyWzLWngFTUmerMoYRGPcg1602WzyJmEQeDwysHZ9/IdNng7uQfL9glJw=="),
--         ("9Lug2SABE6TmcOg1", "14|8|1|BqlRgBPWP2yMURM+Hq6jRJ5rwh2Q9RUgXHNtfH4h8z8=|XMaqqoiRctrB3XvU1dA2R5+92iGluja2LT69c+QiBA7GUhwBvgRqFFpCWtTTHdjpB+sBzICqzeJ8t+N3ds23eA=="),
--         ("HJWwRJLNlJk4FX5c", "14|8|1|0SFk2vr1j/j5+mpmbxwdI/SF/rIdUpCVmfBWOKKzX7g=|wixyWp/rQuBJyGacuBHhfWwWt3eGzc4XQ8ksLrF2OygSV/2qNHJWknzB4Bty3c3bb3RZDi8TY+g2N8UyRRzm2A=="),
--         ("dZFj7VL1QDEAEE4d", "14|8|1|DWvGBwfvQHr3Wb0xK7LUvOLSYm4PPiy7hl+YGLNET/4=|pWDVtFAzNQSq8tEudwZzpqFO/VTX37CGyFwc/rF7MM92RHQ+ssPqxdvaUZmDVJyPFPVqAQDQSM/Tbl5jWnOucA=="),
--         ("pQND73HlucUiaKh7", "14|8|1|dir5SJQUEpHQBktYomh31mLsW8RqN1Jjt5nYCXL3Rvw=|T7+uD4Pq1Qtif1rGKzckZYrJKaculFjXzHAAPjYHzQLs+hwuVhZhsPldfvlpfzgTuAX4APYAEuDbKXW7tYpYqg=="),
--         ("g/yKhi7tFljT1UDs", "14|8|1|pS8X/BZT83WS8BJ0upNLtmwdl51B1VR+1q7cEhyza+8=|xiyZ4lpSxL7G/HkfHk4NyBzGm0IFbVj3FFM3HspGMJr7tLy/ksyxyXdhX1abd1GLlyGjnO4j+YBx9DhT+KoklQ=="),
--         ("uc+TvL7iDgYQQXme", "14|8|1|/ZTdJRWmMLLSuV6D15DMN1YkRxb1tGLGUnUKeGHzxZY=|P3BBKm6Ej3d11taWJYQ7NnkVNmuSaBKk2mGMpDmO3jGRFsZZ0fD0zyll8N1yz750N4D4Q54MAThDrbHFME+unA=="),
--         ("ZG3h6jqXG+zgDjnm", "14|8|1|QrLVyrnjOu+/U+9l0URDXNHsn7fzDkls1K3JIru53OM=|HXFPBXthvk1wHMBciLpySbQKbIXX3Q9E4UQ97B7QPXCGqRJUURYHnjCzU1xxal3iPRBPUOuX2Otn7NyggR4ASQ=="),
--         ("KkGIhMVjLDiNknyf", "14|8|1|5GDYEcH8KFr87kIxzIehzag2fs5FcaFs3+OMSemBlLE=|M4KGPlKHJeF6gRMFNzXSH6noDyykoYs2/l/YCjdvGSlZ/o6lP3+x275n8XU3ekJgN//ULtCK3iVxPtIDdGl6ow=="),
--         ("e344H17yQKjTf+H5", "14|8|1|Wz7UQzDAA2seNigPEavFrXvt3LVRRmNnSROlrT5smxM=|kOd786PIbKVKO2aHDSZFxUhJAt5ZanH9ZIrwUxjplo07wbySHfJoobmkR0t/L6aEE2OxbCLm5TNTPtuGqIzgqw=="),
--         ("Khsf0SAun2ow0lF3", "14|8|1|5mDGxaKXNTJP1pJwlzeEhbVZUZLx/mEg51Wstl1pVYo=|1LPzkXrFY3hTxZI6Eh+FggJGudDm+KK9YhpGj63Isu8ADPSPInrZh3yKsa6Epe+KlrGrFjFYHrMBp/xn7kTIPw=="),
--         ("QjFdD3JGEXlzCBoi", "14|8|1|U5CH2QZGjTJ/zsveQ/w8Yf906S7c25nrQfF+Pl9hBPo=|sAnA3VbXF0HzotjHhAS3VUGcYtNBuGdX6OYpbOkbv3m1VXprDTA50Xhgwc1kPLfzyRxFKhaWkhpwNcEUfjjyzQ=="),
--         ("cJOB4LoGbRuUD+06", "14|8|1|/8VCetg2CNMugucetpV5fD0Hf3dpjG0s9771D8CNnRc=|mX/hjI+8KM1Tl9ucGA9c8wQ064wWlapIwb1fdjdYsq2RiSGfwtZQthzlSBPTpakcgQRwIEPw34iyBcbB+ft51w=="),
--         ("ZVn1dMc9mmEA7QDh", "14|8|1|YfEyVGt3gOC1D+9qcJ2RTxPVvkda3WCPocvaOohI8yY=|JI3HQfHiS07DlfNTr8ov1R2IrLgPDoxUzGK4FOyZdrNMT+lVOV5OJ5aM/RelVw9Gcv1ZTOYPp66nq0VBfVq3JA=="),
--         ("zICUabywOzKnnRVu", "14|8|1|KCahTd8AejuL+1apvR3cJMwL4vSNywRswaRnEX9XlTw=|Fdtr4Pp8aTeQzAEb0gfU1wSLDoqQwbvQMbuCxAa63D0vqJHi4k9PRlXW4BjuSWg0D2jWWYTTXkDza9siY5LLbA=="),
--         ("4wHZUm7RduuXqt/9", "14|8|1|sVFY1qWBel83fsJQS6Ih57YAj0FuJ6NcYrZmP9fwd6Y=|Y5jxQ3Ne04X6YQpsQ4OaOZlyA+yGZqAG1QWIAhDCj9n6h/K4Sfe9kvT/5Zdmr4hDpVzlwMh7cJCLc4yChAykFA=="),
--         ("dxngKGz+3lEz0deM", "14|8|1|FNITFtJr2zW2HDAlHxQ4JLEF4rdj9sMAuUIwBedXiY0=|8hy7GA5aIULv7BwJo0JITOc8I4YI2dmUdA7hgvqjHIlGOOI5lQfnGViZkio7Rw7NC80yi3ImePbckU63EBUuNA=="),
--         ("Xv/4kIhIhNXcBPDC", "14|8|1|6zofV+qcaa4BCcF4WaLqpfhP8o2O5ZI07qwVhrW58rU=|8bo30B+jLx/RWseoyJt+FbGaNx6USuUfpmaESnk5nRFl/fv9Auhna12TDYVQ4f7JCzJf3lESTAoEy9SMUFxyiw=="),
--         ("TDRipA0RZWennIOl", "14|8|1|kcEgdnKPplAowdy4xnzKrxxUNwie98JoQJhMKxiMk0g=|6m4itkXYsRQLEXaqRMPWxdkzQPPrR41ggoJLgDbqMTHOo+YjMEo/4ip2/8Q3Y84DAZNNM0axb1SsK2/v8YTnUQ=="),
--         ("DBLkKACZy8QIvJX0", "14|8|1|SQOJ9MwjApV4qn8KfZjeMz2IBMi67pDrlJsJG0z/Xe0=|cdP8fDKhNRTNaQdTNA8RxXY7dhnjTAAGACtqxWBHcfV5xCCqckfsIqWgQShwNu4cFbam9IIVxd4RatmhUU42AQ=="),
--         ("7yz9++7YoAQeRjFN", "14|8|1|llMgxQwexYJeSATHPw2gqkheCGgvqpZH2LQe9dSXYgw=|6u4tUqhfMn6skqKRoq/pY3JOlH4zEPky97pxTpbz8eBJsSqoJs/UJgV919G66hRu9Yc5zizueH/nFmZpvjOf/g==")
--       ]

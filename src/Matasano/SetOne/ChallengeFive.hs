module Matasano.SetOne.ChallengeFive (test) where

import Data.Bits (xor)
import Data.Char (ord)
import Data.Word (Word8)

import Matasano.Util (showHex)

type Key = [Word8]
type Bytes = [Word8]

crypt :: Key -> Bytes -> Bytes
crypt k s = zipWith xor s (cycle k)

stringCrypt :: String -> String -> String
stringCrypt k s = showHex $ crypt (bytes k) (bytes s)
    where
        bytes = map (fromIntegral . ord)

test :: IO ()
test = do
        putStrLn "Challenge Five"
        putStrLn ("Passed? " ++ show (stringCrypt key plaintext == ciphertext))
    where
        key = "ICE"
        plaintext = "Burning 'em, if you ain't quick and nimble\n"
                 ++ "I go crazy when I hear a cymbal"
        ciphertext = "0b3637272a2b2e63622c2e69692a2369"
                  ++ "3a2a3c6324202d623d63343c2a262263"
                  ++ "24272765272a282b2f20430a652e2c65"
                  ++ "2a3124333a653e2b2027630c692b2028"
                  ++ "3165286326302e27282f"

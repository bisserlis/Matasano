module Matasano.SetOne.ChallengeTwo (showHex, xorHex, test) where

import Data.Bits ((.&.), shift, xor)
import Data.Char (intToDigit)
import Matasano.SetOne.ChallengeOne (readHex)
import Data.Word (Word8)

xorHex :: String -> String -> String
xorHex s t = showHex (zipWith xor (readHex s) (readHex t))

showHex :: [Word8] -> String
showHex = concatMap hexPair
    where
        hexPair n = [showDigit (high n), showDigit (low n)]
        
        high n = (n .&. (128 + 64 + 32 + 16                )) `shift` (-4)
        low n  =  n .&. (                     8 + 4 + 2 + 1)
        
        showDigit = intToDigit . fromIntegral

xorHex_test :: Bool
xorHex_test = xorHex a b == c
    where
        a = "1c0111001f010100061a024b53535009181c"
        b = "686974207468652062756c6c277320657965"
        c = "746865206b696420646f6e277420706c6179"

test :: IO ()
test = do
        putStrLn "Challenge Two"
        putStrLn ("Passed? " ++ show xorHex_test)

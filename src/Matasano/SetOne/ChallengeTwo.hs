module Matasano.SetOne.ChallengeTwo (test) where

import Data.Bits (xor)

import Matasano.Util (readHex, showHex)

xorHex :: String -> String -> String
xorHex s t = showHex (zipWith xor (readHex s) (readHex t))

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

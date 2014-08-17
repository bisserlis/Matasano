module Matasano.SetOne.ChallengeThree (test) where

import Data.Bits (xor)
import Data.Char (chr)
import Data.Function (on)
import Data.List (maximumBy)
import Data.Word (Word8)

import Matasano.Util (readHex)

crypt :: Word8 -> [Word8] -> [Word8]
crypt k s = zipWith xor s (repeat k)

score :: String -> Int
score = length . filter isLetter
    where
        isLetter = (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ " ',.")

guess :: String -> String
guess = maximumBy (compare `on` score) . guesses
    where
        guesses s = map (toString . flip crypt (readHex s)) [0..255]
        toString = map (chr . fromIntegral)

test :: IO ()
test = do
        putStrLn "Challenge Three"
        putStrLn ("Best guess: " ++ guess secret)
    where
        secret = "1b37373331363f78151b7f2b78343133"
              ++ "3d78397828372d363c78373e783a393b3736"

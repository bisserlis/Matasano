module Matasano.SetOne.ChallengeFour (test) where

import Data.Bits (xor)
import Data.Char (chr, toUpper)
import Data.Function (on)
import Data.List (maximumBy)
import Data.Map.Strict (Map, fromList, findWithDefault)
import Data.Word (Word8)

import Matasano.SetOne.ChallengeOne (readHex)

-- Shamelessly lifted off en.wikipedia.org/wiki/Letter_frequency
frequencies :: Map Char Double
frequencies = fromList [ ('A', 8.167)
                       , ('B', 1.492)
                       , ('C', 2.782)
                       , ('D', 4.253)
                       , ('E', 13.00)
                       , ('F', 2.228)
                       , ('G', 2.015)
                       , ('H', 6.094)
                       , ('I', 6.966)
                       , ('J', 0.153)
                       , ('K', 0.772)
                       , ('L', 4.025)
                       , ('M', 2.406)
                       , ('N', 6.749)
                       , ('O', 7.507)
                       , ('P', 1.929)
                       , ('Q', 0.095)
                       , ('R', 5.987)
                       , ('S', 6.327)
                       , ('T', 9.056)
                       , ('U', 2.758)
                       , ('V', 0.978)
                       , ('W', 2.360)
                       , ('X', 0.150)
                       , ('Y', 1.974)
                       , ('Z', 0.074)
                       ]

crypt :: Word8 -> [Word8] -> [Word8]
crypt k s = zipWith xor s (repeat k)

score :: String -> Double
score = sum . map ((\k -> findWithDefault 0 k frequencies) . toUpper)

guess :: [Word8] -> [(Word8, String)]
guess s = for [0..255] (\k -> (k, toString $ crypt k s))
    where
        for = flip map
        toString = map (chr . fromIntegral)

bestGuess :: [Word8] -> (Word8, String)
bestGuess = maximumBy (compare `on` score . snd) . guess

locate :: [String] -> (String, (Word8, String))
locate xs = maximumBy (compare `on` score . snd . snd)
                      (zip xs (map (bestGuess . readHex) xs))

test :: IO ()
test = do
        putStrLn "Challenge Four"
        f <- readFile "data/4.txt"
        let cryptograms = lines f
            (original, (key, result)) = locate cryptograms
        putStrLn "Best Guess: "
        putStrLn ("  Original: " ++ original)
        putStrLn ("  Key     : " ++ show key)
        putStrLn ("  Result  : " ++ result)

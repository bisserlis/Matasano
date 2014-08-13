module Matasano.SetOne.ChallengeSix (test) where

import Data.Bits ((.&.), (.|.), popCount, shift, xor)
import Data.Char (chr)
import Data.Function (on)
import Data.List (maximumBy, minimumBy, tails, transpose)
import Data.Map (Map, findWithDefault, fromList, fromListWith)
import Data.Word (Word8)

read64 :: String -> [Word8]
read64 = go
    where
        go                  []  = []
        go (a : b :'=':'=': []) = tri1 (val a) (val b)
                                : []
        go (a : b : c :'=': []) = tri1 (val a) (val b)
                                : tri2 (val b) (val c)
                                : []
        go (a : b : c : d : es) = tri1 (val a) (val b)
                                : tri2 (val b) (val c)
                                : tri3 (val c) (val d)
                                : go es
        
        tri1 h l = shift  h                                  2
               .|. shift (l .&. (32 + 16                )) (-4)
        tri2 h l = shift (h .&. (          8 + 4 + 2 + 1))   4
               .|. shift (l .&. (32 + 16 + 8 + 4        )) (-2)
        tri3 h l = shift (h .&. (                  2 + 1))   6
               .|.        l
        
        val :: Char -> Word8
        val c = findWithDefault err c table
        
        err = error "read64: Invalid base64 character"
        
        table :: Map Char Word8
        table = fromList (zip (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "+/")
                              [0..63])

hamming :: [Word8] -> [Word8] -> Int
hamming a b = sum . map popCount $ zipWith xor a b

keysize :: [Word8] -> Int -> Int
keysize c m = best $ for [1..m] (\n -> (n, normalize n $ distances n))
    where
        for = flip map
        
        distances n = average [hamming s t | (s:ts) <- tails c', t <- ts]
            where
                average l = fromIntegral (sum l) / fromIntegral (length l)
                c' = c `groupsOf` n
        
        normalize n d = d / fromIntegral n :: Double
        
        best = fst . minimumBy (compare `on` snd)

groupsOf :: [a] -> Int -> [[a]]
groupsOf [] _ = []
groupsOf xs n = front : groupsOf remaining n
    where
        (front, remaining) = splitAt n xs

solve :: Map Char Integer -> [Word8] -> (String, String)
solve freqs ciphertext = applyTuple toString (concat . transpose)
                       . unzip
                       . map (guess freqs)
                       . transpose
                       $ ciphertext `groupsOf` klen
    where
        klen = keysize ciphertext 40
        
        toString = map (chr . fromIntegral)
        
        applyTuple f g (a, b) = (f a, g b)

frequency :: [Char] -> Map Char Integer
frequency = fromListWith (+) . flip zip (repeat 1)

guess :: Map Char Integer -> [Word8] -> (Word8, String)
guess freqs s = maximumBy (compare `on` score . snd)
              . for [0..255]
              $ (\k -> (k, toString $ crypt s k))
    where
        for = flip map
        toString = map (chr . fromIntegral)
        
        crypt s k = zipWith xor s (repeat k)
        
        score = sum . map (\c -> findWithDefault 0 c freqs)

test :: IO ()
test = do
        putStrLn "Challenge Six"
        corpus <- readFile "data/plaintext_corpus.txt"
        f <- readFile "data/6.txt"
        let freqs = frequency corpus
            ciphertext = read64 . concat . lines $ f
            (key, plaintext) = solve freqs ciphertext
        putStrLn "Best guess:"
        putStrLn ("  Key      : " ++ show key)
        putStrLn ("  Plaintext: " ++ show (take 50 plaintext) ++ "...")

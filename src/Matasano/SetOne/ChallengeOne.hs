module Matasano.SetOne.ChallengeOne (hexTo64, readHex, show64, test) where

import Data.Bits ((.&.), (.|.), shift)
import Data.Char (digitToInt)
import Data.List (foldl', unfoldr)
import Data.Word (Word8)

hexTo64 :: String -> String
hexTo64 = show64 . readHex

readHex :: String -> [Word8]
readHex = reverse . unfoldr readHex' . reverse
    where
        readHex'      []  = Nothing
        readHex' (l:  []) = Just ((low l), [])
        readHex' (l:h:ds) = Just ((high h .|. low l), ds)
        
        low = fromIntegral . digitToInt
        high = (`shift` 4) . low

show64 :: [Word8] -> String
show64        []  = []
show64 (a:    []) = enc1 a : enc2 a 0 : "=="
show64 (a:b:  []) = enc1 a : enc2 a b : enc3 b 0 : "="
show64 (a:b:c:ds) = enc1 a : enc2 a b : enc3 b c : enc4 c : show64 ds

enc1 x     = encode $ shift (mask x [1,1,1,1,1,1,0,0]) (-2)
enc2 x y   = encode $ shift (mask x [0,0,0,0,0,0,1,1])   4
                  .|. shift (mask y [1,1,1,1,0,0,0,0]) (-4)
enc3   y z = encode $ shift (mask y [0,0,0,0,1,1,1,1])   2
                  .|. shift (mask z [1,1,0,0,0,0,0,0]) (-6)
enc4     z = encode $        mask z [0,0,1,1,1,1,1,1]

mask i m = i .&. foldl' (\acc b -> shift acc 1 .|. b) 0 m

encode e = (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "+/") !! fromIntegral e

hexTo64_test :: Bool
hexTo64_test = hexTo64 hex == sixtyFour
    where
        hex = "49276d206b696c6c696e6720796f7572"
           ++ "20627261696e206c696b65206120706f"
           ++ "69736f6e6f7573206d757368726f6f6d"
        sixtyFour = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBs"
                 ++ "aWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

test :: IO ()
test = do
        putStrLn "Challenge One"
        putStrLn ("Passed? " ++ show hexTo64_test)

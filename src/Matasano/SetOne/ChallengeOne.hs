module Matasano.SetOne.ChallengeOne (test) where

import Matasano.Util (readHex, show64)

hexTo64 :: String -> String
hexTo64 = show64 . readHex

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

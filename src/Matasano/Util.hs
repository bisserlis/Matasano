module Matasano.Util ( readHex, showHex
                     , read64, show64
                     ) where

import Data.Bits ((.&.), (.|.), shift)
import Data.Char (digitToInt, intToDigit)
import Data.Map (Map, findWithDefault, fromList)
import Data.Word (Word8)

readHex :: String -> [Word8]
readHex [] = []
readHex (_:[]) = error "readHex: String must be of even length"
readHex (h:l:cs) = readByte h l : readHex cs
    where
        readByte h' l' = high h' .|. low l'
        
        low = fromIntegral . digitToInt
        high = (`shift` 4) . low

showHex :: [Word8] -> String
showHex = concatMap hexPair
    where
        hexPair n = [showDigit (high n), showDigit (low n)]
        
        high n = (n .&. (128 + 64 + 32 + 16                )) `shift` (-4)
        low n  =  n .&. (                     8 + 4 + 2 + 1)
        
        showDigit = intToDigit . fromIntegral

read64 :: String -> [Word8]
read64 = go
    where
        go                  []  = []
        go (            _ : []) = error "read64: Malformed base64 string"
        go (        _ : _ : []) = error "read64: Malformed base64 string"
        go (    _ : _ : _ : []) = error "read64: Malformed base64 string"
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

show64 :: [Word8] -> String
show64 = show64'
    where
        show64'        []  = []
        show64' (a:    []) = enc1 a : enc2 a 0 : "=="
        show64' (a:b:  []) = enc1 a : enc2 a b : enc3 b 0 : "="
        show64' (a:b:c:ds) = enc1 a : enc2 a b : enc3 b c : enc4 c : show64' ds
        
        enc1 x     = encode $ (x .&. (128+64+32+16+8+4    )) `shift` (-2)
        enc2 x y   = encode $ (x .&. (                 2+1)) `shift`   4
                          .|. (y .&. (128+64+32+16        )) `shift` (-4)
        enc3   y z = encode $ (y .&. (             8+4+2+1)) `shift`   2
                          .|. (z .&. (128+64              )) `shift` (-6)
        enc4     z = encode $ (z .&. (       32+16+8+4+2+1))
        
        encode e = (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "+/")
                !! fromIntegral e


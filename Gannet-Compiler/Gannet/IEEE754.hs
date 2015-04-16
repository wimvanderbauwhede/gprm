-- | Encoding a 32-bit or 64-bit IEEE floating-point numbers to a 32-bit or 64-bit unsigned integer and
module Gannet.IEEE754(
        encodeIEEE754,
--        encodeIEEE754_32,
--        encodeIEEE754_64,
        decodeIEEE754,
--        decodeIEEE754_32,
--        decodeIEEE754_64
) where

import Data.Bits
--import Data.Word
import Data.List

{-
This is a Double-to-bytes conversion
- assume x is Double, i.e. 64-bit IEEE 754
- Forget NaN, infinity and -0
- Haskell's decodeFloat x returns (m,n) where x = m*2**n
- For denormalized doubles, we shift to the right; the exp is zero
- Unfortunately Haskell's shiftL works only on 32-bit Int's
-}

class FloatingPoint a where
        encodeIEEE754 :: a -> Integer
        decodeIEEE754 :: Integer -> a

instance FloatingPoint Float where
        encodeIEEE754 = encodeIEEE754_32
        decodeIEEE754 = decodeIEEE754_32

instance FloatingPoint Double where
        encodeIEEE754 = encodeIEEE754_64
        decodeIEEE754 = decodeIEEE754_64

encodeIEEE754_64 :: Double -> Integer -- [Word8]                
encodeIEEE754_64 x
        | x == 0.0 = 0
        | otherwise =
                let
                        signbit
                                | x<0.0 = shiftL 1 63 
                                | otherwise = 0
                        xn
                                | x<0 = (-x)
                                | otherwise = x
                        (m,n)=decodeFloat xn
                        m1
                                | m==0 = 0 
                                | otherwise = m - pow2(52) -- (shiftL 1 52) 
                        n1 
                                | n>0 = (-n)
                                | otherwise = n
                        e1 = 1075 + n -- 1023 + 52; 1023=2**(11-1)-1 : exp is biased -- FIXME: should this not be n1?
                        fltw
                                | isDenormalized x = (shiftR m (1-e1)) + signbit
                                | otherwise =  m1 + (toInteger e1)*pow2(52) + signbit -- m1 +                        
                in
                        fltw        
                        
encodeIEEE754_32 :: Float -> Integer
encodeIEEE754_32 x
        | x == 0.0 = 0
        | otherwise =
                let
                        signbit
                                | x<0.0 = shiftL 1 31
                                | otherwise = 0
                        xn
                                | x<0 = (-x)
                                | otherwise = x
                        (m,n)=decodeFloat xn
                        m1
                                | m==0 = 0 
                                | otherwise = m - pow2(23) -- (shiftL 1 23) 
                        n1 
                                | n>0 = (-n)
                                | otherwise = n
                        e1 = 150 + n -- 127 + 23; 127=2**(8-1)-1 (exp bias) -- FIXME: should this not be n1?
                        fltw
                                | isDenormalized x =         (shiftR m (1-e1)) + signbit
                                | otherwise =  m1 + (toInteger e1)*pow2(23) + signbit -- m1 +                        
                in
                        fltw                        

pow2 :: Integer -> Integer
pow2 0 = 1
pow2 n = 2*(pow2 (n-1))

decodeIEEE754_32 :: Integer -> Float
decodeIEEE754_32 word 
        | word == 0 = 0.0
        | word == 0x80000000 = -0.0
        | otherwise =
        let
                exponent =  shiftR (word .&. 0x7F800000 ) 23 
                signbit = shiftR (word .&. 0x80000000 ) 31
                sign = 1 - 2*signbit
                significand = word .&. 0x007FFFFF
        in
                (fromInteger sign)*(mantissa_32 significand)* (2 ^^ (exponent - 127)) -- FIXME: only OK for pos

decodeIEEE754_64 :: Integer -> Double
decodeIEEE754_64 word
        | word == 0 = 0.0
        | word == 0x8000000000000000 = -0.0
        | otherwise =
        let
                exponent =  shiftR (word .&. 0x7FF0000000000000 ) 52
                signbit = shiftR (word .&. 0x8000000000000000 ) 63
                sign = 1 - 2*signbit
                significand = word .&. 0x000FFFFFFFFFFFFF
        in
                (fromInteger sign)*(mantissa_64 significand)*(2 ^^ (exponent - 1023)) -- FIXME: only OK for pos


mantissa_32 :: Integer -> Float
mantissa_32 s = 
        let
                bitpos = take 23 (iterate (+1) 0)
                bitvals = map (\x->(fp_bitval_32 s x)) bitpos
        in
                foldl' (+) 1 bitvals

fp_bitval_32 :: Integer -> Integer -> Float
fp_bitval_32 word pos = 
        let
                bitval =shiftR (word .&. (pow2 pos) ) (fromInteger pos)
                divby = pow2 (23 - pos)
        in
                fromInteger bitval / fromInteger divby


mantissa_64 :: Integer -> Double
mantissa_64 s = 
        let
                bitpos = take 52 (iterate (+1) 0)
                bitvals = map (\x->(fp_bitval_64 s x)) bitpos
        in
                foldl' (+) 1 bitvals

fp_bitval_64 :: Integer -> Integer -> Double
fp_bitval_64 word pos = 
        let
                bitval =shiftR (word .&. (pow2 pos) ) (fromInteger pos)
                divby = pow2 (52 - pos)
        in
                fromInteger bitval / fromInteger divby                

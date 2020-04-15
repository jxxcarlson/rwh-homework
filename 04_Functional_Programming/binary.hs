-- {-# LANGUAGE BinaryLiterals #-}

import  Data.Char (ord)
import Data.Bits (shiftL, (.&.), (.|.))

a = 0b0101
b = 0b1011

c = 0b1111
d = 0b0001
e = a `shiftL` b

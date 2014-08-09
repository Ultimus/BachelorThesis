module Token where

import qualified Data.Vector.Unboxed as V

data Token =
	S String    |
    Atom String |
	Int Int     |
	N Int       |
	Bv [Int]    |
    B Bool      |
    Sep Char    |
    FDInt String|
    Bin Int
	deriving (Eq,Show,Ord)



instance Num Token where
    (Int i1) + (Int i2) = Int (i1 + i2)
    (Int i) - (Int j)  = Int (i -j)
    (Int i) * (Int j) = Int (i * j)
    --fromIntegral i =Int i

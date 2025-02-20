module DataProcessing (
    notFunc, isZeroFunc, evalFunc,
    isEmptyFunc, sumOfTwoInThree, isOneOrTwoZeroes,
    atLeastTwo, headOrDefaultV2, dropHead, isSecondZero,
) where

{-
patten matching
1.
f :: type
f pattern1 = result1
f pattern2 = result2
...
f patternN = resultN

2.
f :: type
f args = case arg of
    pattern1 -> result1
    pattern2 -> result2
    ...
    patternN -> resultN
-}

notFunc :: Bool -> Bool
notFunc True    = False
notFunc False   = True

isZeroFunc :: Integer -> Bool
isZeroFunc 0    = True
isZeroFunc _    = False

evalFunc :: Char -> Integer -> Integer -> Integer
evalFunc op x y = case op of
    '+' -> x + y
    '-' -> x - y
    '*' -> x * y
    '/' -> div x y
    '%' -> mod x y
    _ -> 0

isEmptyFunc :: [a] -> Bool
isEmptyFunc []  = True
isEmptyFunc _   = False

{-
f(x) = (x.length === 3)
        ? x[0] + x[2]
        : 0
-}
sumOfTwoInThree :: Integral a => [a] -> a
sumOfTwoInThree [first, _, third]   = first + third
sumOfTwoInThree _                   = 0

{-
f(x) = (x.length === 1 && x[0] === 0)
        ? True
        : (x.length === 2 && x[0] === 0 && x[1] === 0)
        ? True
        : False
-}
isOneOrTwoZeroes :: (Eq a, Num a) => [a] -> Bool
isOneOrTwoZeroes xs = case xs of
    [0]     -> True
    [0, 0]  -> True
    _       -> False

atLeastTwo :: [a] -> Bool
atLeastTwo []   = False
atLeastTwo [_]  = False
atLeastTwo _    = True

-- [3, 1, 2]
-- 3 : 1 : 2 : []
-- 3 : (1 : (2 : []))
headOrDefaultV2 :: a -> [a] -> a
headOrDefaultV2 defVal []   = defVal
headOrDefaultV2 _ (x:_)     = x

dropHead :: [a] -> [a]
dropHead []     = []
dropHead (_:xs) = xs

isSecondZero :: (Eq a, Num a) => [a] -> Bool
isSecondZero (_:0:_)    = True
isSecondZero _          = False

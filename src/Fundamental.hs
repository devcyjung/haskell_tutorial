module Fundamental (
    increase, headOrDefault,
    signString, sameThree, sameThreeV2,
    appendLastTwos, countValue, countValueV2,
    applyToSame, satisfies, applyTwice,
    doubleList, evenFilter, hasLargerThan10,
    twoSumNaive, firstNExponents,
) where

import Data.List (genericTake)
import Numeric.Natural (Natural)

-- f(x) = x + 1
increase :: Integer -> Integer
increase arg = arg + 1

-- if then else
-- f(d, x) = x[0] || d
headOrDefault :: a -> [a] -> a
headOrDefault defVal list =
    if null list 
    then defVal
    else list !! 0

-- guard |
-- It's a if-else syntax sugar
{- 
f(x) = (x == 0)
    ? "Zero"
    : (x > 0)
    ? "Positive"
    : "Negative"
-}
signString :: (Num a, Ord a) => a -> String
signString n
    | n < 0     = "Negative"
    | n == 0    = "Zero"
    | otherwise = "Positive"

-- variable let .. in 
-- f(x) = x[:3] == x[-3:]
{-
sameThree list =
    (take 3 (reverse list)) == (reverse (take 3 list))
-}
sameThree :: Eq a => [a] -> Bool
sameThree list =
    let h = take 3 list
        t = reverse(take 3 (reverse list))
    in h == t

sameThreeV2 :: Eq a => [a] -> Bool
sameThreeV2 list = h == t
    where
        h = take 3 list
        t = reverse (take 3 (reverse list))

-- local function where ..
-- f(x, y) = [...x[-2:], ...y[-2:]]
appendLastTwos :: [a] -> [a] -> [a]
appendLastTwos l1 l2 =
    lastTwo l1 ++ lastTwo l2
        where
            lastTwo :: [a] -> [a]
            lastTwo lst = reverse(take 2 (reverse(lst)))

-- recursion & pattern matching
-- f(v, x) = x.filter(e => e == v).length
countValue :: Eq a => a -> [a] -> Integer
countValue x xs = go 0 xs
    where
        go acc [] = acc
        go acc (y:ys)
            | x == y    = go (acc + 1) ys
            | otherwise = go acc ys

countValueV2 :: Eq a => a -> [a] -> Integer
countValueV2 x xs = go 0 xs
    where
        go acc y =
            if null y
            then acc
            else
                if y !! 0 == x
                then go (acc + 1) (drop 1 y)
                else go acc (drop 1 y)

-- higher order function
-- f(func, x) = func(x, x)
applyToSame :: (a -> a -> b) -> a -> b  
applyToSame f x = f x x

-- lambda expression
-- \arg1 arg2 ... -> expression
-- satisfies (\x -> mod x 3 == 0) 33
-- f(p, x) = (p(x))
--              ? `Value ${x} satisfies it.`
--              : `Value ${x} doesn't satisfy it.`
satisfies :: (Show a) => (a -> Bool) -> a -> String
satisfies predicate x
    | predicate x   = "Value " ++ (show x) ++ " satisfies it."
    | otherwise     = "Value " ++ (show x) ++ " doesn't satisfy it."

-- partial application
-- (* 3), div 12, True ||, comp2' f = comp2 f add ...
-- applyTwice (+ 20) 12
-- applyTwice (* 3) 4
-- f(func, x) = func(func(x))
applyTwice :: (a -> a) -> a -> a
-- applyTwice f x = f (f x)
applyTwice f = f . f

-- standard hof
-- map, filter, any, concatMap, iterate
doubleList :: Num a => [a] -> [a]
doubleList xs = map (* 2) xs

evenFilter :: Integral a => [a] -> [a]
evenFilter xs = filter even xs

hasLargerThan10 :: (Ord a, Num a) => [a] -> Bool
hasLargerThan10 xs = any (> 10) xs

twoSumNaive :: (Num a, Eq a) => a -> [a] -> Integer
twoSumNaive tgt xs = go 0 (concatMap (\x -> map (x +) xs) xs)
    where
        go acc [] = acc
        go acc (y:ys)
            | tgt == y  = go (acc + 1) ys
            | otherwise = go acc ys

firstNExponents :: Natural -> [Natural]
firstNExponents n = genericTake n (iterate (* 2) 1)

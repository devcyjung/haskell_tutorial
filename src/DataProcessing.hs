module DataProcessing (
    notFunc, isZeroFunc, evalFunc,
    isEmptyFunc, sumOfTwoInThree, isOneOrTwoZeroes,
    atLeastTwo, headOrDefaultV2, dropHead, isSecondZero,
    isOddNaive, sumList, startsWithNaive,
    splitAtPos3, showTriple,
    UserNaive (..), getUserNameNaive, getUserAgeNaive, setUserNameNaive,
    User (..), RGB (..), showColor,
    MyResult (..), divide, showResult,
    IntList (..), listLength, nzeroes,
    damage, checkNum,
    ATK (..), DEF (..), HP(..), dmg,
    dupTuple,
    Chest (..), Armor, Sword, Artifact, Gemstone, BronzeChest, SilverChest, GoldenChest,
    RewardChest (..), Dragon (..), reward,
    fromMaybeInt, findFirstFit, myPartitionEithers,
    myShowInt, onlyEven, prodZipList,
    showLength, wordCountList, wordCountEvennessList, takeEven5,
) where

import Numeric.Natural (Natural)

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

isOddNaive :: Natural -> Bool
isOddNaive 0 = False
isOddNaive n = case isOddNaive (n - 1) of
    True    -> False
    False   -> True

sumList :: Num a => [a] -> a
sumList []      = 0
sumList (x:xs)  = x + sumList xs

startsWithNaive :: (Eq a) => [a] -> [a] -> Bool
startsWithNaive [] _    = True
startsWithNaive _ []    = False
startsWithNaive (x:xs) (y:ys)
    | x == y    = startsWithNaive xs ys
    | otherwise = False

{-
tuples can have different type
for example ([True, False], "abc", 'F')
fst returns first, snd returns second
-}
splitAtPos3 :: [a] -> ([a], [a])
splitAtPos3 xs = (take 3 xs, drop 3 xs)

showTriple :: (Show a) => (Bool, a, String) -> String
showTriple (x, y, z)
    | x         = "The second argument is: " ++ show y
    | otherwise = "The third argument is: " ++ z


-- algebraic data type: sum type, product type

{-
product types: all possible combinations incl. values of all types
ex. tuple, data type, data record
-}

-- Data type
-- MkUser is Constructor, the 3 types are Field types
data UserNaive = MkUserNaive String Int Bool
    deriving (Show) -- to display our type in GHCi

-- Getters
getUserNameNaive :: UserNaive -> String
getUserNameNaive (MkUserNaive name _ _) = name

getUserAgeNaive :: UserNaive -> Int
getUserAgeNaive (MkUserNaive _ age _) = age

-- Setters
setUserNameNaive :: String -> UserNaive -> UserNaive
setUserNameNaive name (MkUserNaive _ age isTired) = MkUserNaive name age isTired

-- Data record
{-
Generates following top-level functions
userName    :: User -> String
userAge     :: User -> Int
userIsTired :: User -> Bool

Initializing using field names is possible
john = MkUser {userAge = 29, userIsTired = True, userName = "John"}

Record update syntax is simpler
ivan = john {userName = "Ivan", userIsTired = False}
-}

data User = MkUser
    { userName      :: String
    , userAge       :: Int
    , userIsTired   :: Bool
    }
    deriving (Show)
    
{-
sum types: choice of zero or more types
any possible option of a value from each type
ex. data enumeration, multiple constructors
-}

data RGB
    = Red
    | Green
    | Blue
    deriving (Show)

-- showColor Blue
-- map showColor [Red, Green, Blue, Green]

showColor :: RGB -> String
showColor rgb = case rgb of
    Red     -> "red"
    Green   -> "green"
    Blue    -> "blue"

-- multiple constructors

data MyResult a
    = MyError String
    | MyOk a
    deriving (Show)

divide :: (Fractional a, Eq a) => a -> a -> MyResult a
divide _ 0 = MyError "Division by zero!"
divide x y = MyOk (x / y)

showResult :: (Show a) => MyResult a -> String
showResult (MyError msg) = "Error: " ++ msg
showResult (MyOk result) = "Ok: " ++ show result

-- Recursive data type

data IntList
    = Empty
    | Cons Int IntList
    deriving (Show)

listLength :: IntList -> Int
listLength Empty        = 0
listLength (Cons _ xs)  = 1 + listLength xs

nzeroes :: Int -> IntList
nzeroes 0 = Empty
nzeroes n = Cons 0 (nzeroes (n - 1))

-- Type alias: type
-- type String = [Char]
-- type FilePath = String

type MyQuadruples      = [(Int, Bool, String, String)]
type IntPredicate   = Int -> Bool

type Attack     = Int
type Defense    = Int
type Health     = Int

damage :: Attack -> Defense -> Health -> Health
damage atk def hp = hp + def - atk

checkNum :: MyQuadruples -> IntPredicate -> [String]
checkNum xs f = map go xs
    where
        go (i,b,s1,s2)
            | b == f i  = s1
            | otherwise = s2

-- Wrapper for existing type: newtype
-- Can only have 1 constructor, and 1 field

newtype ATK = MkATK Int
    deriving (Show)
newtype DEF = MkDEF Int
    deriving (Show)
newtype HP  = MkHP  Int
    deriving (Show)

dmg :: ATK -> DEF -> HP -> HP
dmg (MkATK atk) (MkDEF def) (MkHP hp) =
    MkHP (hp + def - atk)

-- Parametric polymorphism
-- Type variables start with lowercase, and specific types start with uppercase
dupTuple :: a -> (a, a)
dupTuple x = (x, x)

-- Polymorphic types
data Chest a = MkChest
    { chestGold     :: Int
    , chestTreasure :: a
    }
    deriving (Show)

newtype Armor       = MkArmor Int
    deriving (Show)
newtype Sword       = MkSword Int
    deriving (Show)
newtype Artifact    = MkArtifact Int
    deriving (Show)
newtype Gemstone    = MkGemstone Int
    deriving (Show)

type BronzeChest = Chest Armor
type SilverChest = Chest (Sword, Armor)
type GoldenChest = Chest (Artifact, Sword, [Gemstone])

data RewardChest
    = Bronze BronzeChest
    | Silver SilverChest
    | Golden GoldenChest
    deriving (Show)

data Dragon = MkDragon
    { dragonDifficulty  :: Int
    }
    deriving (Show)

reward :: Dragon -> RewardChest
reward dragon 
    | d < 10    = Bronze (MkChest 5 (MkArmor 5))
    | d < 100   = Silver (MkChest 10 (MkSword 3, MkArmor 5))
    | otherwise = Golden (MkChest 100 (MkArtifact 10, MkSword 20, [MkGemstone 1, MkGemstone 4]))
    where
        d = dragonDifficulty dragon

-- common builtin data types
{-
data Maybe a
    = Nothing
    | Just a

data Either a b
    = Left a
    | Right b

-- Usually Left is Error

-- List
data [] a
    = []
    | a : [a]

-- Your List
data List a
    = Empty
    | Cons a (List a)
-}

fromMaybeInt :: Maybe Int -> Int
fromMaybeInt Nothing    = 0
fromMaybeInt (Just n)   = n

findFirstFit :: (a -> Bool) -> [a] -> Maybe a
findFirstFit predicate xs = case filter predicate xs of
    []  -> Nothing
    r:_ -> Just r

-- partitionEithers is in Data.Either
myPartitionEithers :: [Either a b] -> ([a], [b])
myPartitionEithers []       = ([], []) 
myPartitionEithers (x:xs)   = case x of
    Left l  -> (l : (fst (myPartitionEithers xs)), snd (myPartitionEithers xs))
    Right r -> (fst (myPartitionEithers xs), r : (snd (myPartitionEithers xs)))

-- eta reduction
-- if f(x) == g(x) for all x, then f == g

myShowInt :: Int -> String
myShowInt = show -- same as, myShowInt n = show n

onlyEven :: [Integer] -> [Integer]
onlyEven = filter even -- same as, onlyEven xs = filter even xs

-- zipWith (*) [3, 1, 2] [10, 20, 30] -> [30, 20, 60]
prodZipList :: (Num a) => [a] -> [a] -> [a]
prodZipList = zipWith (*) -- same as, prodZipList xs ys = zipWith (*) xs ys

-- function composition (.)
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- (.) f g = \x -> f (g x)

showLength :: Show a => a -> Int
showLength = length . show

wordCountList :: [String] -> [Int]
wordCountList = map (length . words)

wordCountEvennessList :: [String] -> [Bool]
wordCountEvennessList = map (even . length . words)

-- list of first 5 lists that have even length
takeEven5 :: [[a]] -> [[a]]
-- takeEven5 list = take 5 (filter (\l -> even (length l)) list)
takeEven5 = take 5 . filter (even . length)
-- undo eta reduction
-- takeEven5 list = (take 5 . filter (even . length)) list
-- undo function composition
-- takeEven5 list = take 5 (filter (even . length) list)
-- undo eta reduction
-- takeEven5 list = take 5 (filter (\l -> (even . length) l) list)
-- undo function composition
-- takeEven5 list = take 5 (filter (\l -> even (length l)) list)

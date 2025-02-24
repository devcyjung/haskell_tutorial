{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}    -- enable extensions
{-# LANGUAGE GeneralisedNewtypeDeriving  #-}
module TypeClassConcept (
    Display (..), greet, displayBoth,
    Whatever (..),
    checkAllBool, checkAnyBool, getTotalSum, getTotalProduct, getFirstNonNothing, getLastNonNothing,
    MySingleton (..), MyFunctor (..),
    foldlExample, foldrExample,
    mySum, myLen, myLenSum, myHead, myFind, myFindIndexV1, myFindIndexV2, myFindIndexV3, myFindIndexV4,
    myListAll, myEverySecondElement,
    MyFoldable (..),
    myLazySum, myStrictSum, myStrictSumV2,
) where

import Data.Bits        (Bits, FiniteBits)
import Data.Ix          (Ix)
import Data.List        (intercalate)
import Data.Monoid      (First (..), Last (..))
import Data.Semigroup   (All (..), Any (..), Sum (..), Product (..)) 

-- Parametric Polymorphism vs Ad-hoc Polymorphism
-- Same behavior for different types vs Different behavior for different types
-- Parametric : The first element of a pair,
-- Parametric : Reversing a list,
-- Parametric : List length,
-- Parametric : Taking 5 elements from a list,
-- Parametric : Function composition,
-- Ad-hoc : Number addition,
-- Ad-hoc : Equality,
-- Ad-hoc : Comparison,
-- Ad-hoc : Conversion to string,
-- Ad-hoc : Parsing from a string,

class Display a where       -- Typeclass name: Display, Type variable: a
    {-# MINIMAL display #-} -- At minimum, display needs to be implemented
    display :: a -> String  -- Method name: display, Method type signature: a -> String
    displayList :: [a] -> String
    displayList l =
        "[" ++ intercalate "," (map display l) ++ "]"   -- default method implementation

instance Display Bool where -- 1 instance for 1 type
    display False   = "false"
    display True    = "true"

instance Display Char where
    display :: Char -> String   -- enabled by InstanceSigs
    display c       = [c]       -- [c] is turning Char to [Char] which is String

instance Display a => Display [a] where -- [Bool] and [Char] are now instances of Display
    display []      = ""
    display (x:xs)  = display x ++ display xs

greet :: Display a => a -> String
greet val = "Hello, " ++ display val

displayBoth :: (Display a, Display b) => a -> b -> String
displayBoth a b = display a ++ " and " ++ display b

-- data : What is stored inside, no method
-- class : Only method, no data. Default implementation
-- instance : Bridge of data and method. Implementation

-- standard typeclasses
-- Eq : check for equality
-- Ord : compare
-- Show : convert to String
-- Read : parse from String
-- Bounded : has minimal and maximal value
-- Enum : enumeration
-- Num : a number (addition, multiplication, subtraction, etc)

-- check typeclass in ghci
-- :info Bounded
-- :i Bounded

-- Haskell equality table
-- https://htmlpreview.github.io/?https://github.com/quchen/articles/blob/master/haskell-equality-table.html
-- [] == "" because String is [Char]

-- stock derivable classes
data Whatever
    = What
    | Ever
    deriving (Eq, Ord, Show, Read, Enum, Bounded, Ix)

-- with GeneralisedNewtypeDeriving enabled, newtype can derive any typeclass of original type
newtype IntSize = IntSize
    { unSize :: Int
    } deriving ( Show
               , Read
               , Eq
               , Ord
               , Enum
               , Bounded
               , Ix
               , Num
               , Integral
               , Real
               , Bits
               , FiniteBits
               )

-- Monoid
-- type a is Monoid when it has associative function (<>) that lets you combine any two value of type a
-- into one, and a neutral element (mempty) such that
-- a <> mempty == mempty <> a == a
-- A Monoid is a Semigroup with the added requirement of a neutral element.
-- A Monoid is a Semigroup, but a Semigroup is not necessarily a Monoid.
-- Semigroup : typeclass for combining instances <> (a <> b) <> c == a <> (b <> c)
-- Appendable : Equivalent but has append instead of <>
-- instance Semigroup [a] where
--     (<>) = (++)
-- instance Semigroup Bool -> should we use || or && .. so Bool's not an instance of Semigroup 
-- Standard library has 2 newtypes based on Bool, Any and All
-- instance Semigroup Any where
--     Any x <> Any y = Any (x || y)
-- instance Semigroup All where
--     All x <> All y = All (x && y)
-- Num has Sum and Product for Semigroup newtype
-- newtype Sum a =
--     Sum { getSum :: a }
-- newtype Product a =
--     Product { getProduct :: a }
-- First and Last Semigroups are wrappers for Maybe type. Taking first or last non-Nothing value
-- newtype First a =
--     First { getFirst :: a }
-- newtype Last a = 
--     Last { getLast :: a }

checkAllBool :: (a -> Bool) -> [a] -> All
checkAllBool f xs =
    foldMap All (map f xs)

checkAnyBool :: (a -> Bool) -> [a] -> Any
checkAnyBool f xs =
    foldMap Any (map f xs)

getTotalSum :: Num a => [a] -> Sum a
getTotalSum =
    foldMap Sum

getTotalProduct :: Num a => [a] -> Product a
getTotalProduct =
    foldMap Product

getFirstNonNothing :: [Maybe a] -> Data.Monoid.First a
getFirstNonNothing =
    foldMap Data.Monoid.First

getLastNonNothing :: [Maybe a] -> Data.Monoid.Last a
getLastNonNothing =
    foldMap Data.Monoid.Last

-- monoid is a semigroup with a neutral element
-- class Semigroup a => Monoid a where
--     mempty :: a
-- x <> mempty == x
-- mempty <> x == x

-- standard instances of monoid
-- instance Monoid [a] where mempty = []
-- instance Monoid Any where mempty = Any False
-- instance Monoid All where mempty = All True
-- instance Num a => Monoid (Sum a) where mempty = Sum 0
-- instance Num a => Monoid (Product a) where mempty = Product 1

-- First, Last don't have mempty
-- instance Semigroup (First a) wherer a <> _ = a
-- instanace Monoid (First a) where mempty = ??
-- Modules: base has two First data types
-- Data.Semigroup
-- newtype First a = First { getFirst :: a }
-- Data.Monoid
-- newtype First a = First { getFirst :: Maybe a }
-- Data.Monoid First & Last has mempty = Nothing

-- Kind : type of types
-- :k Int, :k String in ghci
-- * : Int, String, .. -- complete type
-- * -> * : Maybe, [], .. -- type constructor
-- * -> * -> * : Either, (->), .. -- type constructor that takes two types as input
-- * -> Constraint: Num, Eq, .. -- constraint constructor

-- kind : polymorphism over type constructors
-- A typeclass for creating singleton containers from values
class MySingleton f where
    mySingleton :: a -> f a -- :t MySingleton f => a -> f a
-- This class in for type constructors like Maybe, [], not Int
-- Value types inside the type constructor should be the same

-- :k MySingleton
-- (* -> *) -> Constraint

-- mySingleton 3 :: Maybe Int
-- Just 3
instance MySingleton Maybe where
    mySingleton :: a -> Maybe a
    mySingleton = Just

-- mySingleton 3 :: [Int]
-- [3]
instance MySingleton [] where
    mySingleton :: a -> [a]
    mySingleton x = [x]

-- Functor : Maps values inside context f
class MyFunctor f where
    myFmap :: (a -> b) -> f a -> f b

-- :k MyFunctor
-- (* -> *) -> Constraint

-- myFmap (*3) (Just 5)
-- Just 15
instance MyFunctor Maybe where
    myFmap :: (a -> b) -> Maybe a -> Maybe b
    myFmap _ Nothing    = Nothing
    myFmap f (Just x)   = Just (f x)

-- myFmap (*3) [5]
-- [15]
instance MyFunctor [] where
    myFmap :: (a -> b) -> [a] -> [b]
    -- myFmap   = map
    myFmap _ []     = []
    myFmap f (x:xs) = f x : myFmap f xs

-- myFmap (*3) (Right 5)
-- Right 15
instance MyFunctor (Either e) where
    myFmap :: (a -> b) -> (Either e) a -> (Either e) b
    myFmap _ (Left e)   = Left e
    myFmap f (Right x)  = Right (f x)

-- identity function
-- id :: a -> a
-- id x = x

-- functor law 1 : identity
-- fmap id == id

-- functor law 2 : composition
-- fmap (f . g) == fmap f . fmap g

-- Folds (reduce in JS)
-- ex. Folds for list
-- foldl :: (a -> b -> a) -> a -> [b] -> a  
-- (a -> b -> b)    : "step" function
-- b                : initial value
-- [a]              : list of values
-- b                : final result
-- foldl : left - to - right
-- foldr : right - to - left

-- foldlExample (\acc -> \cur -> acc ++ show cur) "hi" [1..10]
-- "hi12345678910"
foldlExample :: (a -> b -> a) -> a -> [b] -> a
foldlExample = foldl

-- foldrExample (\cur -> \acc -> acc ++ show cur) "hi" [1..10] 
-- "hi10987654321"
foldrExample :: (b -> a -> a) -> a -> [b] -> a
foldrExample = foldr

-- foldl always leaks memory due to its nature
-- use foldl' instead of foldl. foldl' is "strict evaluation" version of foldl
-- foldl f z [1,2,3] == f (f (f z 1) 2) 3
-- foldr f z [1,2,3] == f 1 (f 2 (f 3 z))

-- fold is very powerful
mySum :: Num a => [a] -> a
mySum = foldl (+) 0

myLen :: [a] -> Integer
myLen = foldl (\x _ -> x + 1) 0

myLenSum :: Num a => [a] -> (Integer, a)
myLenSum xs = (foldl (\x _ -> x + 1) 0 xs, foldl (+) 0 xs)

myHead :: [a] -> Maybe a
myHead = foldr (\x _ -> Just x) Nothing

myFind :: Eq a => a -> [a] -> Maybe a
myFind x = foldl (\acc cur ->
    if cur == x && acc == Nothing
    then Just cur
    else acc) Nothing

myFindIndexV1 :: Eq a => a -> [a] -> Maybe Integer
myFindIndexV1 x xs =
    let 
        res = foldl (\acc cur -> case acc of
            Left idx    ->
                if x /= cur
                then Left (idx + 1)
                else Right idx
            Right _     -> acc) (Left 0) xs
    in
        case res of
            Left _  -> Nothing
            Right r -> Just r

myFindIndexV2 :: Eq a => a -> [a] -> Maybe Integer
myFindIndexV2 x xs = foldl (\acc cur -> case acc of
    Nothing ->
        if snd cur == x
        then Just (fst cur)
        else acc
    Just _  -> acc) Nothing (zip [0..] xs)

-- even though foldl' is more optimized than foldl, it cannot compute on infinite list.
myFindIndexV3 :: Eq a => a -> [a] -> Maybe Integer
myFindIndexV3 x xs = foldl' (\acc cur -> case acc of
    Nothing ->
        if snd cur == x
        then Just (fst cur)
        else acc
    Just _  -> acc) Nothing (zip [0..] xs)

-- due to tail call optimization, unlike V1, V2 or V3, myFindIndexV4 can compute on infinite lists
-- myFindIndexV4 5 [1..]
-- In Haskell, always prefer foldr to foldl,
-- and always prefer prepending to list, rather than apppending to the last.
myFindIndexV4 :: Eq a => a -> [a] -> Maybe Integer
myFindIndexV4 x xs = foldr (\cur acc ->
    if x == snd cur
    then Just (fst cur)
    else acc) Nothing (zip [0..] xs)

myListAll :: (a -> Bool) -> [a] -> [a]
myListAll f = foldr (\cur acc -> case f cur of
    True    -> cur : acc
    False   -> acc) []

myEverySecondElement :: [[a]] -> [a]
myEverySecondElement = foldr (\cur acc -> case cur of
    (_:x:_) -> x : acc
    _       -> acc) []

-- Foldable : fold works on all Foldable types
class MyFoldable t where    -- t for 'transform' because it transforms type. It's a type constructor
    myFoldr :: (cur -> acc -> acc) -> acc -> t cur -> acc
    myFoldMap :: Monoid m => (cur -> m) -> t cur -> m 

-- :t foldr
-- Foldable t => (a -> b -> b) -> b -> t a -> b
-- :t sum
-- (Foldable t, Num a) => t a -> a
-- :t concat
-- Foldable t => t [a] -> [a]

-- foldr (-) 10 (Just 3)
-- -7
-- foldr (-) 10 (Nothing)
-- 10
-- it's discouraged to use fold on Maybe types

-- Lazy evaluation vs Strict evaluation (BangPatterns extension needed to be enabled)
-- Lazy evaluation may lead to memory leak
myLazySum :: [Int] -> Int
myLazySum = go 0
    where
        go acc []       = acc
        go acc (x:xs)   = go (acc + x) xs

-- debugging in Haskell : Equational Reasoning
-- sum [1, 2, 3]
-- go 0 [1, 2, 3]
-- go (0 + 1) [2, 3]
-- go ((0 + 1) + 2) [3]
-- go (((0 + 1) + 2) + 3) []
-- (((0 + 1) + 2) + 3)
-- ((1 + 2) + 3)
-- (3 + 3)
-- 6

-- Bang (!) forces evaluation of lazy computation to some degree
myStrictSum :: [Int] -> Int
myStrictSum = go 0
    where
        go !acc []      = acc
        go !acc (x:xs)  = go (acc + x) xs

-- no memory leak
-- sum [1, 2, 3]
-- go 0, [1, 2, 3]
-- go 1, [2, 3]
-- go 3, [3]
-- go 6, []
-- 6

myStrictSumV2 :: [Int] -> Int
myStrictSumV2 = foldl' (+) 0

{-# LANGUAGE InstanceSigs #-}
module MonadConcept (
    maybePlusNaive, maybeAndThen, maybePlusBetter,
    eitherPlusNaive, eitherAndThen, eitherPlusBetter,
    listPlusNaive, listAndThen, listPlusBetter,
    MyMonad (..), maybePlusMonad, eitherPlusMonad, listPlusMonad, plusMonad,
) where

import Prelude hiding ((>>=), return) -- hides the default monad methods for exercise

-- Problem : Awfully nested pattern matching for Maybe types (equivalent to Nullable, Optional types)
maybePlusNaive :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
maybePlusNaive ma mb mc md = case ma of
    Nothing -> Nothing
    Just a  -> case mb of
        Nothing -> Nothing
        Just b  -> case mc of
            Nothing -> Nothing
            Just c  -> case md of
                Nothing -> Nothing
                Just d  -> Just (a + b + c + d)

-- Solution : Ordinary helper function
maybeAndThen :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeAndThen ma f = case ma of
    Nothing -> Nothing
    Just x  -> f x

maybePlusBetter :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
maybePlusBetter ma mb mc md = 
    maybeAndThen ma (\a -> 
        maybeAndThen mb (\b ->
            maybeAndThen mc (\c ->
                maybeAndThen md (\d ->
                    Just (a + b + c + d)))))

-- Problem again : For Either type
eitherPlusNaive :: Either String Int -> Either String Int -> Either String Int -> Either String Int -> Either String Int
eitherPlusNaive ea eb ec ed = case ea of
    Left e  -> Left e
    Right a -> case eb of
        Left e  -> Left e
        Right b -> case ec of
            Left e  -> Left e
            Right c -> case ed of
                Left e  -> Left e
                Right d -> Right (a + b + c + d)

-- Solution : Another similar helper function
eitherAndThen :: Either err res -> (res -> Either err res) -> Either err res
eitherAndThen ea f = case ea of
    Left err    -> Left err
    Right x     -> f x

eitherPlusBetter :: Either String Int -> Either String Int -> Either String Int -> Either String Int -> Either String Int
eitherPlusBetter ea eb ec ed =
    eitherAndThen ea (\a ->
        eitherAndThen eb (\b ->
            eitherAndThen ec (\c ->
                eitherAndThen ed (\d ->
                    Right (a + b + c + d)))))

-- Problem again : For List type
listPlusNaive :: [Int] -> [Int] -> [Int] -> [Int] -> [Int]
listPlusNaive la lb lc ld = case la of
    []  -> []
    _   -> case lb of
        []  -> []
        _   -> case lc of
            []  -> []
            _   -> case ld of
                []  -> []
                _   -> 
                    concatMap (\a ->
                        concatMap (\b ->
                            concatMap (\c ->
                                concatMap (\d ->
                                    [a + b + c + d])
                                ld)
                            lc)
                        lb)
                    la
-- debugging
-- listPlusNaive [a1, a2] [b1, b2] [c1, c2] [d1, d2]
-- D = concatMap (\d -> [a + b + c + d]) ld
-- D = [a + b + c + d1, a + b + c + d2]
-- C = concatMap (\c -> D) lc
-- C = [a + b + c1 + d1, a + b + c1 + d2, a + b + c2 + d1, a + b + c2 + d2]
-- B = concatMap (\b -> C) lb
-- B = [a + b1 + c1 + d1, a + b1 + c1 + d2, a + b1 + c2 + d1, a + b1 + c2 + d2, a + b2 + c1 + d1, a + b2 + c1 + d2, a + b2 + c2 + d1, a + b2 + c2 + d2]
-- A = concatMap (\a -> B) la
-- A = [a1 + b1 + c1 + d1, a1 + b1 + c1 + d2, a1 + b1 + c2 + d1, a1 + b1 + c2 + d2, a1 + b2 + c1 + d1, a1 + b2 + c1 + d2, a1 + b2 + c2 + d1, a1 + b2 + c2 + d2, a2 + b1 + c1 + d1, a2 + b1 + c1 + d2, a2 + b1 + c2 + d1, a2 + b1 + c2 + d2, a2 + b2 + c1 + d1, a2 + b2 + c1 + d2, a2 + b2 + c2 + d1, a2 + b2 + c2 + d2]

-- Solution : Another helper
listAndThen :: [a] -> (a -> [b]) -> [b]
listAndThen la f = case la of
    []      -> []
    x:xs    -> f x ++ listAndThen xs f

listPlusBetter :: [Int] -> [Int] -> [Int] -> [Int] -> [Int]
listPlusBetter la lb lc ld =
    listAndThen la (\a ->
        listAndThen lb (\b ->
            listAndThen lc (\c ->
                listAndThen ld (\d ->
                    [a + b + c + d]))))

-- Monad : generalization of andThen pattern
-- 1. polymorphic andThen
-- 2. polymorphic constructor

class MyMonad m where
    return  :: a -> m a
    (>>=)   :: m a -> (a -> m b) -> m b
    -- >>= is called 'bind'\
    -- Monad is a typeclass for type constructors like Maybe, but not Int

instance MyMonad Maybe where
    return  :: a -> Maybe a
    return x = Just x   -- incorrect implementation: return x = Nothing

    (>>=)   :: Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing >>= _ = Nothing
    Just x  >>= f = f x

instance MyMonad (Either e) where
    return  :: a -> Either e a
    return x = Right x

    (>>=)   :: Either e a -> (a -> Either e b) -> Either e b
    Left e  >>= _ = Left e
    Right x >>= f = f x

instance MyMonad [] where
    return  :: a -> [a]
    return x = [x]

    (>>=)   :: [a] -> (a -> [b]) -> [b]
    []      >>= _ = []
    xs      >>= f = concatMap f xs

-- laws of monad
-- 1. Left identity:    return a >>= f == f a
-- 2. Right identity:   m >>= return == m
-- 3. Associativity:    (m >>= f) >>= g == m >>= (\x -> f x >>= g)

-- Generalized solution
-- andThen ::        Maybe a  -> (a ->    Maybe b)  -> Maybe    b
-- andThen ::     Either e a  -> (a -> Either e b)  -> Either e b
-- andThen ::             [a] -> (a ->         [b]) ->         [b]
-- (>>=)   :: Monad m => m a  -> (a ->        m b)  ->        m b

-- Similar constructors
-- Just    ::              a  ->          Maybe a
-- Right   ::              a  ->       Either e a
-- (:[])   ::              a  ->               [a]
-- return  :: Monad m => m a  ->              m a

-- Refactoring
-- prefix form: f a b
-- infix form:  a `f` b
-- (>>=) is same as `andThen` (infix form of andThen)
-- return is generalized version of Just, Right, :[], the constructor

maybePlusMonad :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
maybePlusMonad ma mb mc md = ma >>= \a -> mb >>= \b -> mc >>= \c -> md >>= \d -> return (a + b + c + d)

eitherPlusMonad :: Either String Int -> Either String Int -> Either String Int -> Either String Int -> Either String Int
eitherPlusMonad ea eb ec ed = ea >>= \a -> eb >>= \b -> ec >>= \c -> ed >>= \d -> return (a + b + c + d)

listPlusMonad :: [Int] -> [Int] -> [Int] -> [Int] -> [Int]
listPlusMonad la lb lc ld = la >>= \a -> lb >>= \b -> lc >>= \c -> ld >>= \d -> return (a + b + c + d)

plusMonad :: (MyMonad m, Num a) => m a -> m a -> m a -> m a -> m a
plusMonad ma mb mc md = ma >>= \a -> mb >>= \b -> mc >>= \c -> md >>= \d -> return (a + b + c + d)

-- monad < applicative < functor
-- class Functor f where
--     fmap     :: (a -> b) -> f a -> f b
--
-- class Functor f => Applicative f where
--     pure     :: a -> f a
--     (<*>)    :: f (a -> b) -> f a -> f b
--
-- class Applicative m => Monad m where
--     return   :: a -> m a
--     return   = pure
--     (>>=)    :: m a -> (a -> m b) -> m b
--
-- most people use pure instead of return. they are identical.

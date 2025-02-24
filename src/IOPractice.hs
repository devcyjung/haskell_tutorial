module IOPractice (
    myGetLine, myGetTwoLines, myPutStrLn, myPutTwoStrLns, lengthOfUserInput,
    myPutTwoStrLnsV2, readAndWriteInReverse, readAndWriteInReverseV2,
) where

-- pure functions   : no side-effect. only depend on explicit input argument
-- map :: (a -> b) -> [a] -> [b]
-- benefits of pure functions: 
-- 1. determinism   2. easier reasoning     3. simple testing
-- 4. composable    5. optimization         6. parallelism

-- initially haskell didn't have any means to introduce side effect

-- purity + laziness
-- how would a function that reads a line would look like?
-- getLine :: String
-- or how about reading two lines?
-- getTwoLines :: [String]
-- getTwoLines = [getLine, getLine]

-- getTwoLines !! 1
-- since haskell is lazy evaluated, so the 0th index won't be evaluated.
-- does that mean getLine is called once?
-- and what about when getLine is called again

-- does it mean the result will depend on whether index 0 is accessed first or index 1 is accessed first

-- something needs to be changed for Haskell to have a side effect

-- :k IO
-- * -> *

myGetLine :: IO String  -- myGetLine returns a String while also performing side-effect
-- opaque data type IO without any constructor, cannot pattern match
-- data IO a = ..
-- thankfully io has monad instance
-- instance Monad IO where
--     return   :: a -> IO a
--     (>>=)    :: IO a -> (a -> IO b) -> IO b
myGetLine = getLine

myGetTwoLines :: IO [String]
myGetTwoLines =
    myGetLine >>= \line1 -> myGetLine >>= \line2 -> pure [line1, line2]

myPutStrLn :: String -> IO () -- () is a unit type, similar to None in Python. try :t ()
myPutStrLn = putStrLn

myPutTwoStrLns :: (String, String) -> IO ()
myPutTwoStrLns (str1, str2) =
    myPutStrLn str1 >>= \_ -> myPutStrLn str2 >>= \_ -> pure ()

-- but without pattern matching, how do we extract value from IO types
-- use any methods Functor, Applicative, or Monad provide
lengthOfUserInput :: IO Integer
lengthOfUserInput = fmap computeLen myGetLine
    where
        computeLen []       = 0
        computeLen (_:xs)   = 1 + computeLen xs

-- haskell program entry point:
-- module Main's main :: IO () function
-- ghc Main.hs -o helloworld
-- ./helloworld

-- (>>) operator
-- (>>) :: Monad m => m a -> m b -> m b
-- action1 >> action2 = action1 >>= \_ -> action2
-- used for IO pipeline

myPutTwoStrLnsV2 :: (String, String) -> IO ()
myPutTwoStrLnsV2 (str1, str2) =
    myPutStrLn str1 >> myPutStrLn str2

-- do notation : syntax sugar for >>=, >>, and let-in
-- These three are identical
-- ex1 = fun1 >>= \res -> fun2 res

-- do notation version
-- ex1 = do
--     res <- fun1
--     fun2 res

-- eta-reduction version
-- ex1 = fun1 >>= fun2

-- These two are identical
-- ex2 = fun1 >> fun2

-- ex2 = do
--     fun1
--     fun2

-- These two are identical
-- ex3 = let x = f y in
--       fun x

-- ex3 = do
--     let x = f y
--     fun x

readAndWriteInReverse :: IO ()
readAndWriteInReverse =
    getLine >>= \line ->
    let rev = reverse line in
    putStrLn rev

readAndWriteInReverseV2 :: IO ()
readAndWriteInReverseV2 = do
    l <- getLine            -- common mistake : let line = getLine  -- does not execute the function
    let rev = reverse l     -- common mistake : rev <- reverse l    -- do not bind a pure function
    putStrLn rev

-- Cabal (ambiguous)
-- 1. format for describing structure of Haskell packages
-- 2. files with .cabal file extension that describes the package
-- 3. cabal-install : a Haskell build tool that works with Cabal packages

-- Design paradigm: Functional kernel, imperative shell
-- Separate pure logic from effectful computations and require IO only when needed.

-- example: a program that finds the most frequent word in a file
-- imperative version
-- readFile     :: FilePath -> IO Text
-- countWords   :: Text -> IO (Map Text Int)
-- printWord    :: Map Text Int -> IO ()

-- functional version
-- readFile     :: FilePath -> IO Text
-- countWords   :: Text -> Map Text Int
-- findFrequent :: Map Text Int -> Maybe (Text, Int)
-- displayReult :: Maybe (Text, Int) -> Text
-- putStrLn     :: Text -> IO ()

-- more resources
-- CIS 194 : IO and Monads (with cakes)
-- Lined-up Haskell types
-- Aelve Guide : Do notation
-- GitHub : kowainik/awesome-cabal

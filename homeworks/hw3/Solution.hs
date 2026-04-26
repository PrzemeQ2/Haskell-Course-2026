{-# LANGUAGE DeriveFunctor #-}
module Hw3.Solution where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (delete)
import Control.Monad (guard)

-- 1. Maze Navigation
type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)

-- a)

move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do 
    neighbours <- Map.lookup pos maze
    position <- Map.lookup dir neighbours
    return position

-- b) 

safeHead :: [Dir] -> Maybe Dir 
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [Dir] -> Maybe [Dir] 
safeTail [] = Nothing
safeTail (_:xs) = Just xs

followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath _ pos [] = Just pos 
followPath maze pos dirs = do 
    step <- safeHead dirs
    rest <- safeTail dirs
    neighbour <- move maze pos step
    followPath maze neighbour rest    

-- c) 

safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath _ pos [] = Just [pos]
safePath maze pos dirs = do 
    step <- safeHead dirs
    tail' <- safeTail dirs
    next <- move maze pos step
    rest <- safePath maze next tail'
    return (pos:rest)

-- 2. Decoding a message 
type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt keys string = traverse (\c -> Map.lookup c keys) string

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords _ [] = Just []
decryptWords keys (word:restOfWords) = do 
    decryptedWord <- decrypt keys word 
    undecryptedWords <- decryptWords keys restOfWords
    return (decryptedWord : undecryptedWords)

-- 3. Seating arrangements
type Guest = String
type Conflict = (Guest, Guest)

permutations :: [Guest] -> [[Guest]]
permutations [] = [[]]
permutations guests = do 
    guest <- guests
    rest <- permutations (delete guest guests)
    return (guest : rest)

isValidSeating :: [Guest] -> [Conflict] -> Bool 
isValidSeating [] _ = True
isValidSeating seating conflicts = 
    let 
        pairs = zip seating (tail seating ++ [head seating])
        isConflict (g1, g2) = (g1, g2) `elem` conflicts || (g2, g1) `elem` conflicts
    in 
        not (any isConflict pairs)
  
seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do 
    seatings1 <- permutations guests
    guard (isValidSeating seatings1 conflicts)
    return seatings1

-- 4. Result monad with warnings
data Result a = Failure String | Success a [String]

-- a) 
instance Functor Result where
    fmap _ (Failure a) = Failure a 
    fmap f (Success a str) = Success (f a) str

instance Applicative Result where
    pure x = Success x []
    (Failure str) <*> _ = Failure str 
    _ <*> (Failure str) = Failure str
    (Success f str1) <*> (Success x str2) = Success (f x) (str1 ++ str2) 

instance Monad Result where
    Failure str >>= _ = Failure str
    (Success x str) >>= f = 
        case f x of 
            Failure str1 -> Failure str1
            Success y str2 -> Success y (str ++ str2)


-- b) 
warn    :: String -> Result ()
warn str = Success () [str]

failure :: String -> Result a
failure str = Failure str

-- c)

validateAge :: Int -> Result Int
validateAge age = do 
    if age < 0 
        then 
            failure "The age cannot be negative"
    else if age > 150 
        then do 
            warn "The age shouldn't be above 150"
            return age
    else 
            return age

validateAges :: [Int] -> Result [Int]
validateAges ages = mapM validateAge ages

-- 5. Evaluator with simplification log

newtype Writer m a = Writer {runWriter :: (a,m)} deriving (Show, Functor)

instance (Monoid m) => Applicative (Writer m) where
    pure x = Writer (x, mempty)  
    Writer (f, logf) <*> Writer (x, logx) = Writer (f x, logf <> logx) 

instance (Monoid m) => Monad (Writer m) where
    Writer (x, logx) >>= f = 
        let Writer (y, logy) = f x 
        in Writer (y, logx <> logy)  
 
tell :: m -> Writer m ()
tell message = Writer { runWriter = ((), message) }

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr deriving (Show, Eq)

simplify :: Expr -> Writer [String] Expr
simplify (Lit n) = return (Lit n)

simplify (Neg expr) = do 
    e <- simplify expr
    simplifyStep (Neg e)

simplify (Add expr1 expr2) = do
    e1 <- simplify expr1
    e2 <- simplify expr2
    simplifyStep (Add e1 e2)

simplify (Mul expr1 expr2) = do
    e1 <- simplify expr1
    e2 <- simplify expr2
    simplifyStep (Mul e1 e2)

simplifyStep :: Expr -> Writer [String] Expr

simplifyStep (Add (Lit 0) e) = do
    tell ["Additive identity: 0+e -> e"]
    return e

simplifyStep (Add e (Lit 0)) = do
    tell ["Additive identity: e+0 -> e"]
    return e

simplifyStep (Mul (Lit 1) e) = do
    tell ["Multiplicative identity: 1*e -> e"]
    return e

simplifyStep (Mul e (Lit 1)) = do
    tell ["Multiplicative identity e*1 -> e"]
    return e

simplifyStep (Mul (Lit 0) _) = do
    tell ["Zero absorption: 0*e -> 0"]
    return (Lit 0)

simplifyStep (Mul _ (Lit 0)) = do
    tell ["Zero absorption: e*0 -> 0"]
    return (Lit 0)

simplifyStep (Neg (Neg e)) = do
    tell ["Double Negation Neg (Neg e) -> e"] 
    return e

simplifyStep (Add (Lit a) (Lit b)) = do
    tell ["Constant folding: " ++ show a ++ "+" ++ show b ++ " -> " ++ show (a+b)]
    return (Lit (a + b))

simplifyStep (Mul (Lit a) (Lit b)) = do
    tell ["Constant folding: " ++ show a ++ "*" ++ show b ++ " -> " ++ show (a*b)]
    return (Lit (a * b))

simplifyStep e = return e
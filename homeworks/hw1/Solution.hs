{-# LANGUAGE BangPatterns #-}
-- Exercise 1 (Goldenbach Pairs)

goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n 
    | n < 4 = []
    | otherwise = [(p, q) | p<-[1..n], q<-[p..n],p + q == n, isPrime p, isPrime q]  

-- Exercise 2 (Coprime Pairs)

unique :: [Int] -> [Int]
unique [] = []
unique (x:xs) = x : unique [y | y<-xs, y /= x]

positive :: [Int] -> [Int]
positive xs = [y | y<-xs, y > 0]

coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs list = [(x,y) | x<-processedList, y<-processedList, gcd x y == 1, x < y]
    where processedList = unique (positive list) 

-- Exercise 3 (Sieve of Eratosthenes)

primesTo :: Int -> [Int]
primesTo n = sieve [2..n]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve [y | y<-xs, y `mod` x /= 0] 

isPrime :: Int -> Bool
isPrime n 
    | n < 2 = False
    | n == 2 = True
    | even n = False
    | otherwise = n == last primes
    where primes = primesTo n

-- Exercise 4 (Matrix Multiplication) 

matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul [] _ = [] 
matMul _ []  = []
matMul a b = [ [ sum [ a !! i !! k * b !! k !! j | k <- [0..p-1] ] | j <- [0..n-1] ] | i <- [0..m-1] ]
        where 
            m = length a
            p = length (head a)
            n = length (head b)

-- Exercise 5 (Permutations)

permutations :: Int -> [a] -> [[a]]
permutations 0 _ = [[]]
permutations _ [] = []
permutations k xs = [ x:rest | (x, remaining)<-getPair xs, rest<-permutations (k-1) remaining]
    where
        getPair :: [a] -> [(a, [a])]
        getPair [] = []
        getPair (y:ys) = (y, ys) : [ (z, y:zs) | (z,zs)<-getPair ys]

-- Exercise 6 (Hamming Numbers)

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys 
merge (x:xs) (y:ys) 
    | x < y = x : merge xs (y:ys)
    | x == y = x : merge xs ys
    | otherwise = y : merge (x:xs) ys 

hamming :: [Integer]
hamming = 1 : merge (map (*2) hamming) (merge (map (*3) hamming) (map (*5) hamming))

-- Exercise 7 (Hamming Numbers)

power :: Int -> Int -> Int
power b e = go e 1 
    where
        go 0 !acc = acc
        go e !acc = go (e-1) (acc*b)

    
-- Exercise 8 (Running Maximum: seq vs. Bang Patterns)

listMaxSeq :: [Int] -> Int 
listMaxSeq (head:list) = go list head
    where 
        go [] acc = acc
        go (x:xs) acc = 
            let newAcc = max x acc 
            in newAcc `seq` go xs newAcc

listMaxBang :: [Int] -> Int 
listMaxBang (head:list) = go list head
    where 
        go [] !acc = acc
        go (x:xs) !acc = go xs (max x acc)

-- Exercise 9 (infinite Prime Stream)

primes :: [Int]
primes = sieve [2..]  

isPrimeInfinite :: Int -> Bool 
isPrimeInfinite n = go primes
    where 
        go (x:xs) 
            | x == n = True
            | x < n = go xs
            | otherwise = False

-- Exercise 10 (Strict Accumulation and Space Leaks) 

-- (a)
badMean :: [Double] -> Double
badMean list = go list (0, 0)
    where 
        go [] (sum, len) = sum / len
        go (x:xs) (sum, len) = go xs (sum+x, len+1)

-- (b)
mean :: [Double] -> Double
mean list = go list (0, 0)
    where 
        go [] (!sum, !len) = sum / len
        go (x:xs) (!sum, !len) = go xs (sum+x, len+1)

-- It is necessary to use bang patterns on the components of the pair (!sum, !len)
-- because writing !(sum, len) instead would only evaluate the pair to WHNF,
-- leaving the components as unevaluated thunks (sum+x, len+1).
-- Hence, we shall force computations to each element by using bang patterns.

-- (c)
meanVariance :: [Double] -> (Double, Double)
meanVariance xs = go xs (0, 0, 0)
    where
        go [] (!sum, !sumsq, !len) = (sum / len, (sumsq/len) - (sum / len)^2)
        go (x:xs) (!sum, !sumsq, !len) = go xs (sum+x, sumsq+(x^2), len+1)
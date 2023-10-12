module Sequence where

import Control.Applicative
import Data.List (find)
import Data.Maybe (fromJust)

-- * Sequences

-- | Metallonacci numbers
-- a_n = a_(n-1) * m + a_(n - 2)
-- offset 0
--
-- >>> take 10 $ mfib 2
-- [0,1,2,5,12,29,70,169,408,985]
mfib :: Int -> [Int]
mfib m = 0 : 1 : zipWith (+) (fmap (* m) $ drop 1 $ mfib m) (mfib m)

-- | Fibonacci numbers
-- a_n = a_ (n - 1) + a_ (n - 2)
-- Metallonacci numbers where m = 1
-- offset 0
--
-- >>> take 10 $ fib
-- [0,1,1,2,3,5,8,13,21,34]
fib :: [Int]
fib = mfib 1

-- | N-bonacci numbers
-- starts with n - 1 zeros followed by a one
-- each term is the sum of the previous n terms
-- https://oeis.org/wiki/N-bonacci_numbers
--
-- >>> take 10 $ nfib 3
-- [0,0,1,1,2,4,7,13,24,44]
nfib :: Int -> [Int]
nfib 0 = repeat 0
nfib n = replicate (n - 1) 0 ++ [1] ++ foldr1 (zipWith (+)) (take n $ iterate (drop 1) (nfib n))

-- | Prime numbers
-- offset 1
--
-- >>> take 10 $ prime
-- [2,3,5,7,11,13,17,19,23,29]
prime :: [Int]
prime = 2 : 3 : 5 : iterate prime' 7
  where
    -- Given the largest prime found (and the list of primes), find the next prime
    prime' :: Int -> Int
    prime' p = fromJust $ find (\n -> all (\a -> mod n a /= 0) (takeWhile (<= (floor . sqrt . fromIntegral) n) prime)) [p + 2, p + 4 ..]

-- | Catalan numbers
-- offset 0
-- a(n) = 2*(2*n-1)*a(n-1)/(n+1) with a(0) = 1 - OEIS A000108
--
-- >>> take 10 $ catalan
-- [1,1,2,5,14,42,132,429,1430,4862]
catalan :: [Int]
catalan = 1 : map (\n -> (4 * n - 2) * (catalan !! (n - 1)) `div` (n + 1)) [1 ..]

-- * Functions

-- | Factor a number
--
-- >>> factor 90
-- [(2,1),(3,2),(5,1)]
factor :: Int -> [(Int, Int)]
factor n = factor' n prime
  where
    factor' :: Int -> [Int] -> [(Int, Int)]
    factor' 1 _ = []
    factor' n (p : ps)
      | n < p * p = [(n, 1)]
      | mod n p == 0 = let (q, r) = logRem n p 0 in (p, q) : factor' r ps
      | otherwise = factor' n ps
    logRem a b n = if mod a b == 0 then logRem (div a b) b (n + 1) else (n, a)

-- | Euler's totient function
--
-- >>> map totient [1..10]
-- [1,1,2,2,4,2,6,4,6,4]
totient :: Int -> Int
totient n = product $ map g $ factor n
  where
    g (p, n) = (p - 1) * p ^ (n - 1)

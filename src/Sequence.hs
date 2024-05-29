module Sequence where

import Control.Applicative
import Control.Monad.ST (ST, runST)
import Data.Array.ST
import Data.Function (fix)
import qualified Data.IntMap.Lazy as IM
import Data.List (find, group)
import Data.Maybe (fromJust)
import Data.Word (Word64)
import System.Random

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

-- | Sum of squares
-- offset 1
-- Numbers that can be written as the form i^2 + j^2
-- i and j can be 0
--
-- >>> take 10 $ sumOfSquares
-- [0,1,2,4,5,8,9,10,13,16]
sumOfSquares :: [Int]
sumOfSquares = map head $ group $ sort2D listOfLists
  where
    listOfLists = [[i * i + j * j | j <- [i ..]] | i <- [0 ..]]

-- | Golomb's sequence
-- smallest nondecreasing sequence where
-- `golomb == map length (group golomb)`
-- starting with 1
--
-- >>> take 20 golomb
-- [1,2,2,3,3,4,4,4,5,5,5,6,6,6,6,7,7,7,7,8]
golomb :: [Int]
golomb = 1 : 2 : 2 : concat (zipWith replicate (drop 2 golomb) [3 ..])

-- | Terms of smallest positive integers so that
-- no three terms a(n) a(n - d) a(n - 2d) form a arithmetic sequence
--
-- >>> take 20 forestFire
-- [1,1,2,1,1,2,2,4,4,1,1,2,1,1,2,2,4,4,2,4]
forestFire :: [Int]
forestFire = map fire [0 ..]
  where
    fire n = head [c | c <- [1 ..], not $ any (isArithmetic c n) [1 .. div n 2]]
    isArithmetic c n d = b - a == c - b
      where
        b = forestFire !! (n - d)
        a = forestFire !! (n - 2 * d)

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

-- Miller-Rabin primality test
-- https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
-- for values under 64-bits, result is deterministic and seed is not used
--
-- >>> [n | n <- [1..20], millerRabin undefined n]
-- [2,3,5,7,11,13,17,19]
-- >>> millerRabin 1 (14710351489 * 49021259447)
-- False
-- >>> millerRabin 1 5376136917648529903181
-- True
millerRabin :: Int -> Integer -> Bool
millerRabin seed n
  | n <= 1 = False
  | nIsSmall && n `elem` bases = True
  | otherwise = all round bases
  where
    nIsSmall = fromIntegral n <= (maxBound :: Word64)
    (a, b) = fix (\v (n, r) -> if odd r then (n, r) else v (n + 1, div r 2)) (0, n - 1) -- n-1 = 2^a * b
    bases
      | nIsSmall = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37] -- deterministic
      | otherwise = take 20 $ randomRs (2, n - 1) (mkStdGen seed) -- probabilistic
    round base = start == 1 || elem (n - 1) (take a $ iterate (`modExp` 2) start)
      where
        start = base `modExp` b
    modExp x y
      | y == 0 = 1
      | odd y = (x * modExp x (y - 1)) `mod` n
      | otherwise = modExp (x ^ 2 `mod` n) (div y 2)

-- | Partition numbers
--
-- >>> map partitionNumbers [0..10]
-- [1,1,2,3,5,7,11,15,22,30,42]
partitionNumbers :: Int -> Int
partitionNumbers n = runST $ do
  arr <- newListArray ((1, 1), (n, n)) (replicate (n + 1) 1 ++ replicate (n * (n + 1)) (-1))
  part arr (n, n)
  where
    -- ways of partitioning target with numbers 1..maxVal
    part :: STUArray s (Int, Int) Int -> (Int, Int) -> ST s Int
    part _ (0, _) = return 1
    part arr idx@(target, maxVal) = do
      curr <- readArray arr idx
      if curr == -1
        then do
          -- calculate
          val <- sum <$> mapM (\a -> part arr (target - a, a)) [1 .. min target maxVal]
          -- memo
          writeArray arr idx val
          return val
        else return curr

-- Ways to put n queens on an nxn board without attacking each other
--
-- >>> map nonattackingQueens [0..10]
-- [1,1,0,0,2,10,4,40,92,352,724]
nonattackingQueens :: Int -> Int
nonattackingQueens n = length $ foldr addCol [[]] [1 .. n]
  where
    addCol j xss = [(i, j) : xs | xs <- xss, i <- [1 .. n], safe (i, j) xs]
    safe (i, j) = all safe'
      where
        -- no need to check vertical position
        safe' (ii, jj) = i /= ii && jj - j /= ii - i && i + j /= ii + jj

-- * Helper functions

-- | Sort a 2D list that are nondecreasing in both directions
sort2D :: [[Int]] -> [Int]
sort2D (xs : xss) = sort2D' [xs] xss
  where
    -- head of all lists in pool and first element in leftover are candidates
    sort2D' pool leftover@((leftoverMin : ls) : lss)
      | poolMin <= leftoverMin = poolMin : sort2D' (newPool pool) leftover
      | otherwise = leftoverMin : sort2D' (ls : pool) lss
      where
        poolMin = minimum $ map head pool
        newPool (xx@(x : xs) : xss)
          | x == poolMin = xs : xss
          | otherwise = xx : newPool xss

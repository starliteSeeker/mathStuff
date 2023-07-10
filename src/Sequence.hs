module Sequence where

import Control.Applicative
import Data.List (find)
import Data.Maybe (fromJust)

cool = 3

-- | Fibonacci numbers
--
-- >>> take 10 $ fib
-- [0,1,1,2,3,5,8,13,21,34]
fib :: [Int]
fib = 0 : 1 : getZipList ((+) <$> ZipList (drop 1 fib) <*> ZipList fib)

-- | Prime numbers
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
-- a(n) = 2*(2*n-1)*a(n-1)/(n+1) with a(0) = 1 - OEIS A000108
--
-- >>> take 10 $ catalan
-- [1,1,2,5,14,42,132,429,1430,4862]
catalan :: [Int]
catalan = 1 : map (\n -> (4 * n - 2) * (catalan !! (n - 1)) `div` (n + 1)) [1 ..]

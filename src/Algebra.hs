module Algebra where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer

-- | Differential list to use with Writer
newtype DiffList a = DiffList {getList :: [a] -> [a]}

instance (Show a) => Show (DiffList a) where
  show (DiffList f) = show $ f []

instance Semigroup (DiffList a) where
  DiffList af <> DiffList bf = DiffList (af . bf)

instance Monoid (DiffList a) where
  mempty = DiffList ([] ++)

-- | Convert from DiffList to list
fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

-- | Extended euclidean algorithm
--
-- >>> putStr $ unlines $ fromDiffList $ execWriter $ exEuclid 5 8
-- 8 mod 5 = 3
-- 5 mod 3 = 2
-- 3 mod 2 = 1
-- 2 mod 1 = 0
-- gcd = 1
-- 1 = (0)*2 + (1)*1
-- 1 = (1)*3 + (-1)*2
-- 1 = (-1)*5 + (2)*3
-- 1 = (2)*8 + (-3)*5
exEuclid :: Int -> Int -> Writer (DiffList String) (Int, Int, Int)
exEuclid a b
  | a < 0 = do
      (gcd, x, y) <- exEuclid (-a) b
      return (gcd, -x, y)
  | b < 0 = do
      (gcd, x, y) <- exEuclid a (-b)
      return (gcd, x, -y)
  | a < b = do
      (gcd, x, y) <- exEuclid b a
      return (gcd, y, x)
  | b == 0 = do
      tell $ DiffList (["gcd = " ++ show a] ++)
      return (a, 1, 0)
  | otherwise = do
      -- gcd = s*a + t*b
      -- mod a b = a - b * (div a b)
      tell $ DiffList ([show a ++ " mod " ++ show b ++ " = " ++ show (mod a b)] ++)
      (gcd, x, y) <- exEuclid b $ mod a b
      let m = negate $ div a b
      let s = y
      let t = x + y * m
      tell $ DiffList ([show gcd ++ " = (" ++ show s ++ ")*" ++ show a ++ " + (" ++ show t ++ ")*" ++ show b] ++)
      return (gcd, s, t)

-- | @inverse a m@ calculates the inverse of a in mod m
--
-- >>> fst $ runWriter $ runMaybeT $ inverse 2 5
-- Just 3
--
-- >>> fst $ runWriter $ runMaybeT $ inverse 2 4
-- Nothing
inverse :: Int -> Int -> MaybeT (Writer (DiffList String)) Int
inverse a m = do
  (gcd, x, _) <- lift $ exEuclid a m
  if gcd == 1 then return $ mod x m else mzero

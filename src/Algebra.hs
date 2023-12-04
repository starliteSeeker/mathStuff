module Algebra where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer
  ( Writer,
    runWriter,
    tell,
    writer,
  )
import Data.Bifunctor (second)
import Data.List
import Data.Ratio
import qualified Data.Set as S

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
-- >>> fst $ runWriter $ runMaybeT $ inverse 2 4
-- Nothing
inverse :: Int -> Int -> MaybeT (Writer (DiffList String)) Int
inverse a m = do
  (gcd, x, _) <- lift $ exEuclid a m
  if gcd == 1 then return $ mod x m else mzero

type Val = Writer Expr Rational

data Expr
  = AddSub (S.Set Expr) (S.Set Expr)
  | MulDiv (S.Set Expr) (S.Set Expr)
  | R Integer
  | Empty
  deriving (Eq, Ord)

instance Show Expr where
  show Empty = "EMPTY"
  show (R r) = show r
  show (AddSub a b)
    | S.size a == 1 && S.null b = show a
    | otherwise = "(" ++ a' ++ if S.null b then [] else b' ++ ")"
    where
      a' = intercalate "+" $ map show $ S.elems a
      b' = '-' : intercalate "-" (map show $ S.elems b)
  show (MulDiv a b)
    | S.size a == 1 && S.null b = show a
    | otherwise = "(" ++ a' ++ if S.null b then [] else b' ++ ")"
    where
      a' = intercalate "*" $ map show $ S.elems a
      b' = '-' : intercalate "/" (map show $ S.elems b)

instance Semigroup Expr where
  Empty <> x = x
  x <> Empty = x
  AddSub a b <> AddSub c d = AddSub (S.union a c) (S.union b d)
  MulDiv a b <> MulDiv c d = MulDiv (S.union a c) (S.union b d)
  x <> MulDiv c d = MulDiv (S.insert x c) d
  x <> AddSub c d = AddSub (S.insert x c) d
  _ <> R _ = undefined

instance Monoid Expr where
  mempty = Empty

-- | Make the target number using all given numbers
countdown :: [Integer] -> Integer -> [String]
countdown given target =
  let seed = map (\v -> tell (R v) >> return (v % 1)) given
   in map show $ filter (\a -> fst (head a) == fromIntegral target) $ map (map runWriter) $ iterate (>>= (concatMap tt . takePairs)) [seed] !! (length given - 1)
  where
    takeOne :: [Val] -> [(Val, [Val])]
    takeOne (l : ls) = (l, ls) : map (second (l :)) (takeOne ls)
    takeOne [] = []
    takePairs :: [Val] -> [(Val, Val, [Val])]
    takePairs (l : ls) = map (\(a, s) -> (l, a, s)) (takeOne ls) ++ map (\(a, b, s) -> (a, b, l : s)) (takePairs ls)
    takePairs [] = []
    tt :: (Val, Val, [Val]) -> [[Val]]
    tt (a, b, ls) =
      [ a `addW` b : ls,
        a `subW` b : ls,
        b `subW` a : ls,
        a `mulW` b : ls
      ]
        ++ [a `divW` b : ls | (fst . runWriter) b /= 0]
        ++ [b `divW` a : ls | (fst . runWriter) a /= 0]
    binOp :: Char -> (Rational -> Rational -> Rational) -> (Expr -> Expr) -> Val -> Val -> Val
    binOp c op f a b =
      let (aa, aw) = runWriter a
          (ba, bw) = runWriter b
       in writer (aa `op` ba, aw <> f bw)
    addW :: Val -> Val -> Val
    addW = binOp '+' (+) f
      where
        f x@(AddSub _ _) = x
        f x = AddSub (S.singleton x) S.empty
    subW :: Val -> Val -> Val
    subW = binOp '-' (-) f
      where
        f (AddSub a b) = AddSub b a
        f x = AddSub S.empty (S.singleton x)
    mulW :: Val -> Val -> Val
    mulW = binOp '*' (*) f
      where
        f x@(MulDiv _ _) = x
        f x = MulDiv (S.singleton x) S.empty
    divW :: Val -> Val -> Val
    divW = binOp '/' (/) f
      where
        f (MulDiv a b) = MulDiv b a
        f x = MulDiv S.empty (S.singleton x)

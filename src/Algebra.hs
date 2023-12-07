module Algebra where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer
  ( Writer,
    mapWriter,
    runWriter,
    tell,
    writer,
  )
import Data.Bifunctor (second)
import Data.List
import qualified Data.Map as M
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

-- * Countdown number game

type Val = Writer Expr Rational

data Expr
  = AddSub (M.Map Expr Int) (M.Map Expr Int)
  | MulDiv (M.Map Expr Int) (M.Map Expr Int)
  | R Integer
  | Empty
  deriving (Eq, Ord)

fromFreq :: (Foldable t) => t (b, Int) -> [b]
fromFreq = concatMap (uncurry (flip replicate))

instance Show Expr where
  show Empty = "EMPTY"
  show (R r) = show r
  show (AddSub a b) =
    a' ++ (if M.null b then [] else b')
    where
      a' = intercalate "+" $ map show $ fromFreq $ M.toList a
      b' = '-' : intercalate "-" (map show $ fromFreq $ M.toList b)
  show (MulDiv a b) =
    a' ++ (if M.null b then [] else b')
    where
      a' = intercalate "*" $ map show' $ fromFreq $ M.toList a
      b' = '/' : intercalate "/" (map show' $ fromFreq $ M.toList b)
      show' a@(AddSub _ _) = "(" ++ show a ++ ")" -- add parenthesis if expression is +/-
      show' a = show a

instance Semigroup Expr where
  Empty <> x = x
  x <> Empty = x
  AddSub a b <> AddSub c d = AddSub (M.unionWith (+) a c) (M.unionWith (+) b d)
  MulDiv a b <> MulDiv c d = MulDiv (M.unionWith (+) a c) (M.unionWith (+) b d)
  x <> MulDiv c d = MulDiv (M.insertWith (+) x 1 c) d
  x <> AddSub c d = AddSub (M.insertWith (+) x 1 c) d
  _ <> R _ = undefined

instance Monoid Expr where
  mempty = Empty

-- | Make the target number using all given numbers
-- returns all possible solutions
--
-- >>> countdown' 4 [2,5,10,23]
-- ["4 = (23-5-10)/2","4 = (5+23)/2-10","4 = 5*10-2*23"]
countdown' :: Integer -> [Integer] -> [String]
countdown' _ [] = []
countdown' target given =
  let seed = S.singleton $ map (\v -> writer (v % 1, R v)) given
   in map printResult $ S.toList $ S.filter ((== fromIntegral target) . fst) $ S.map (head . map runWriter) $ iterate (S.unions . S.map (S.fromList . concatMap applyOp . takePairs)) seed !! (length given - 1)
  where
    takePairs :: [Val] -> [(Val, Val, [Val])]
    takePairs [] = []
    takePairs (l : ls) = takeL ++ dropL
      where
        takeOne (l' : ls') = (l', ls') : map (second (l' :)) (takeOne ls')
        takeOne [] = []
        takeL = map (\(a, s) -> (l, a, s)) (takeOne ls)
        dropL = map (\(a, b, s) -> (a, b, l : s)) (takePairs ls)
    applyOp :: (Val, Val, [Val]) -> [[Val]]
    applyOp (a, b, ls) =
      [ a `addW` b : ls,
        a `subW` b : ls,
        b `subW` a : ls,
        a `mulW` b : ls
      ]
        ++ [a `divW` b : ls | (fst . runWriter) b /= 0] -- avoid divide-by-zero
        ++ [b `divW` a : ls | (fst . runWriter) a /= 0]
    binOp :: (Rational -> Rational -> Rational) -> (Expr -> Expr) -> Val -> Val -> Val
    binOp op f a b = do
      a' <- a
      b' <- mapWriter (second f) b
      return $ op a' b'
    addW :: Val -> Val -> Val
    addW = binOp (+) f
      where
        f x@(AddSub _ _) = x
        f x = AddSub (M.singleton x 1) M.empty
    subW :: Val -> Val -> Val
    subW = binOp (-) f
      where
        f (AddSub a b) = AddSub b a
        f x = AddSub M.empty (M.singleton x 1)
    mulW :: Val -> Val -> Val
    mulW = binOp (*) f
      where
        f x@(MulDiv _ _) = x
        f x = MulDiv (M.singleton x 1) M.empty
    divW :: Val -> Val -> Val
    divW = binOp (/) f
      where
        f (MulDiv a b) = MulDiv b a
        f x = MulDiv M.empty (M.singleton x 1)
    printResult (_, rhs) = show target ++ " = " ++ show rhs

-- | Make the target number using given numbers (not necessary all of them)
--
-- >>> countdown 18 [1,2,4,8,16]
-- ["18 = (1+8)*2", "18 = (1+4)*2+8", "18 = (1+8)*(4-2)", ... , "18 = 8/4-16/(1-2)"]
countdown :: Integer -> [Integer] -> [String]
countdown target given = concatMap (countdown' target) (subsequences given)

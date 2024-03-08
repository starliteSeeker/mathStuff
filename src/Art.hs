module Art (sier, siern, hitomezashi, binaryWave, slant, rule110, cellAutomata, toothpick) where

import Control.Monad (foldM, forM)
import Control.Monad.ST
import Data.Array.ST
import qualified Data.Array.Unboxed as UArr
import Data.Bits
import Data.Foldable (toList)
import Data.Maybe (isNothing)
import Data.STRef
import qualified Data.Union.ST as U
import System.Random

-- >>> putStrLn $ unlines $ sier 3
--        1
--       1 1
--      1   1
--     1 1 1 1
--    1       1
--   1 1     1 1
--  1   1   1   1
-- 1 1 1 1 1 1 1 1
sier :: Int -> [String]
sier = siern 2

-- >>> putStrLn $ unlines $ siern 3 2
--         1
--        1 1
--       1 2 1
--      1     1
--     1 1   1 1
--    1 2 1 1 2 1
--   1     2     1
--  1 1   2 2   1 1
-- 1 2 1 2 1 2 1 2 1
siern :: Int -> Int -> [String]
siern n x = padLeft $ map g $ take h $ iterate ((1 :) . f) [1]
  where
    h = n ^ x
    -- one layer of pascal's triangle to the next
    f (x : y : ys) = mod (x + y) n : f (y : ys)
    f xs = xs
    -- pascal's triangle to sierpinski triangle
    g (x : xs) = (if x /= 0 then show x ++ " " else "  ") ++ g xs
    g _ = []
    -- pad with spaces
    padLeft = zipWith (++) (map (`replicate` ' ') [h - 1, h - 2 .. 0])

-- | Inspired by https://youtu.be/JbfhzlMk2eY
--
-- >>> putStrLn $ unlines $ hitomezashi 20 10 1
--  |_|  _  |_| |_|  _   _| |_   _  |_  |_|
-- _   _| |_   _   _| |_|  _  |_| |_  |_
--  |_|  _  |_| |_|  _   _| |_   _  |_  |_|
-- _   _| |_   _   _| |_|  _  |_| |_  |_
--  |_|  _  |_| |_|  _   _| |_   _  |_  |_|
-- _   _| |_   _   _| |_|  _  |_| |_  |_
--  |_|  _  |_| |_|  _   _| |_   _  |_  |_|
--   _  |_|  _   _  |_| |_   _| |_|  _|  _
-- _| |_   _| |_| |_   _  |_|  _   _|  _| |
--   _  |_|  _   _  |_| |_   _| |_|  _|  _
hitomezashi :: Int -> Int -> Int -> [String]
hitomezashi width height seed = pattern a b
  where
    -- generate random list
    rng = randoms (mkStdGen seed)
    (b, r1) = splitAt width rng
    (a, _) = splitAt height r1
    -- create pattern
    pattern :: [Bool] -> [Bool] -> [String]
    pattern a b = zipWith merge hori vert
    vert = foldr (zipWith (:) . f '|' height) (repeat []) b
    hori = map (f '_' (width * 2)) a
    -- n element list of c's and ' 's, s is offset
    f c n s = take n $ map (\a -> if a then c else ' ') $ iterate not s
    -- merge vertical and horizontal lines
    merge (a : as) (b : bs) = a : b : merge as bs
    merge _ _ = []

-- | Hexoganol tiling
--
-- >>> putStrLn $ unlines $ hexTile 5 3
--  __    __    __
-- /  \__/  \__/  \
-- \__/  \__/  \__/
-- /  \__/  \__/  \
-- \__/  \__/  \__/
-- /  \__/  \__/  \
-- \__/  \__/  \__/
--    \__/  \__/
hexTile :: Int -> Int -> [String]
hexTile a b
  | a <= 0 || b <= 0 = [[]]
hexTile width height = [top] ++ process (mconcat (replicate height middle)) ++ [bottom]
  where
    top = foldMap (\a -> if odd a then " __ " else "  ") [1 .. width]
    middle =
      [ '/' : foldMap (\a -> if odd a then "  \\" else "__/") [1 .. width],
        '\\' : foldMap (\a -> if odd a then "__/" else "  \\") [1 .. width]
      ]
    bottom = ' ' : foldMap (\a -> if odd a then "  " else "\\__/") [1 .. width]
    process = if odd width then id else (\(s : ss) -> init s : ss) -- take away extra '\'

-- Turn range 0..i to array of bools
binaryRep :: Int -> [[Bool]]
binaryRep i = (<$> [0 .. i]) <$> fmap isSet [0 .. height - 1]
  where
    isSet n x = (1 .<<. n) .&. x /= 0 -- is the nth bit set in x
    height = bits i
    bits 0 = 0
    bits x = 1 + bits (x .>>. 1)

-- | Inspired by SPI/I2C diagrams, looks...meh
-- >>> putStrLn $ unlines $ binaryWave 31
-- _/‾\_/‾\_/‾\_/‾\_/‾\_/‾\_/‾\_/‾\_/‾\_/‾\_/‾\_/‾\_/‾\_/‾\_/‾\_/‾‾
-- ___/‾‾‾\___/‾‾‾\___/‾‾‾\___/‾‾‾\___/‾‾‾\___/‾‾‾\___/‾‾‾\___/‾‾‾‾
-- _______/‾‾‾‾‾‾‾\_______/‾‾‾‾‾‾‾\_______/‾‾‾‾‾‾‾\_______/‾‾‾‾‾‾‾‾
-- _______________/‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\_______________/‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
-- _______________________________/‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
binaryWave :: Int -> [String]
binaryWave i = map drawLine $ binaryRep i
  where
    drawLine (b : bb : bs) = drawSegment b bb ++ drawLine (bb : bs)
    drawLine [b] = drawSegment b b
    drawSegment False False = "__"
    drawSegment False True = "_/"
    drawSegment True False = "‾\\"
    drawSegment True True = "‾‾"

-- | Randomly shuffle a list
-- taken from https://wiki.haskell.org/Random_shuffle
--   /O(N)/
shuffle :: [a] -> StdGen -> [a]
shuffle xs gen =
  runST
    ( do
        g <- newSTRef gen
        let randomRST lohi = do
              (a, s') <- fmap (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        forM [1 .. n] $ \i -> do
          j <- randomRST (i, n)
          vi <- readArray ar i
          vj <- readArray ar j
          writeArray ar j vi
          return vj
    )
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs = newListArray (1, n) xs

-- | Pattern of slashes and backslashes that doesn't form a loop, inspired by slant the puzzle game
-- https://en.wikipedia.org/wiki/Gokigen_Naname
-- https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/slant.html
--
-- >>> putStrLn $ unlines $ slant 20 10 1
-- ╱╱╲╲╲╲╲╲╱╲╱╱╲╱╲╱╲╲╲╱
-- ╲╱╲╲╱╱╲╱╲╲╱╱╲╱╲╲╲╱╱╱
-- ╱╲╲╱╱╱╲╱╲╲╱╲╲╱╱╲╲╲╲╲
-- ╱╱╱╲╱╱╱╱╱╲╲╲╱╲╲╲╱╱╱╲
-- ╲╲╲╲╲╲╱╱╱╲╱╲╲╲╲╲╲╱╲╲
-- ╱╱╱╲╲╱╲╱╲╲╱╲╲╱╱╱╱╱╱╱
-- ╲╱╲╲╱╲╲╱╱╱╲╲╲╱╲╲╱╲╱╱
-- ╱╱╱╲╱╱╲╲╱╱╲╱╱╲╲╲╱╱╲╱
-- ╱╲╱╲╱╲╲╲╱╱╱╲╲╲╲╲╱╱╲╱
-- ╱╲╱╱╱╱╲╲╱╲╱╲╱╲╲╲╱╱╲╱
slant :: Int -> Int -> Int -> [[Char]]
slant width height seed =
  let randomList = zip (shuffle [0 .. width * height - 1] (mkStdGen seed)) (randoms $ mkStdGen seed)
   in draw $ runST $ do
        newDsf <- U.new (width' * height') ()
        mapM (slant' newDsf) randomList
  where
    width' = width + 1
    height' = height + 1
    -- decide which slash sould be at position `index`
    slant' :: U.UnionST s () -> (Int, Bool) -> ST s (Int, Bool)
    slant' dsf (index, slash) = do
      -- check if a loop will be formed
      res <-
        ( if slash -- True for backslash '\', False for slash '/'
            then U.merge dsf (\_ _ -> ((), ())) (h * width' + w) ((h + 1) * width' + w + 1)
            else U.merge dsf (\_ _ -> ((), ())) (h * width' + w + 1) ((h + 1) * width' + w)
          )
      return (if isNothing res then (index, not slash) else (index, slash))
      where
        w = mod index width
        h = div index width
    draw :: [(Int, Bool)] -> [[Char]]
    draw ls = chunks $ map toSlash $ UArr.elems (UArr.array (0, width * height - 1) ls :: UArr.UArray Int Bool)
      where
        -- not ascii slashes for prettier printing
        toSlash True = '╲'
        toSlash False = '╱'
        chunks [] = []
        chunks ls = let (as, bs) = splitAt width ls in as : chunks bs

-- * Functions that use cellular automaton

data Cells c = Cells [c] c [c]

instance Functor Cells where
  fmap f (Cells ls c rs) = Cells (fmap f ls) (f c) (fmap f rs)

newtype Cells2D a = Cells2D (Cells (Cells a))

instance Functor Cells2D where
  fmap f (Cells2D (Cells l c r)) = Cells2D (Cells (fmap (fmap f) l) (fmap f c) (fmap (fmap f) r))

duplicate2D :: Cells2D a -> Cells2D (Cells2D a)
duplicate2D cc = Cells2D $ Cells (tail $ iterate (fmap shiftDown) center) center (tail $ iterate (fmap shiftUp) center)
  where
    center = Cells (tail $ iterate shiftRight cc) cc (tail $ iterate shiftLeft cc)
    shiftLeft' (Cells ls c (r : rs)) = Cells (c : ls) r rs
    shiftLeft' _ = error "list not infinite in shiftLeft'"
    shiftRight' (Cells (l : ls) c rs) = Cells ls l (c : rs)
    shiftRight' _ = error "list not infinite in shiftRight'"
    shiftUp (Cells2D cs) = Cells2D $ shiftLeft' cs
    shiftDown (Cells2D cs) = Cells2D $ shiftRight' cs
    shiftRight (Cells2D cs) = Cells2D $ fmap shiftRight' cs
    shiftLeft (Cells2D cs) = Cells2D $ fmap shiftLeft' cs

-- | https://en.wikipedia.org/wiki/Rule_110
-- The most famous elementary cellular automata
--
-- >>> putStrLn $ unlines $ rule110 36 20 0x123456789
--    @  @   @@ @   @ @ @@  @@@@   @  @
--   @@ @@  @@@@@  @@@@@@@ @@  @  @@ @@
--  @@@@@@ @@   @ @@     @@@@ @@ @@@@@@
-- @@    @@@@  @@@@@    @@  @@@@@@    @
-- @@   @@  @ @@   @   @@@ @@    @   @@
--  @  @@@ @@@@@  @@  @@ @@@@   @@  @@@
-- @@ @@ @@@   @ @@@ @@@@@  @  @@@ @@ @
--  @@@@@@ @  @@@@ @@@   @ @@ @@ @@@@@@
-- @@    @@@ @@  @@@ @  @@@@@@@@@@    @
-- @@   @@ @@@@ @@ @@@ @@        @   @@
--  @  @@@@@  @@@@@@ @@@@       @@  @@@
-- @@ @@   @ @@    @@@  @      @@@ @@ @
--  @@@@  @@@@@   @@ @ @@     @@ @@@@@@
-- @@  @ @@   @  @@@@@@@@    @@@@@    @
--  @ @@@@@  @@ @@      @   @@   @   @@
-- @@@@   @ @@@@@@     @@  @@@  @@  @@@
-- @  @  @@@@    @    @@@ @@ @ @@@ @@ @
-- @ @@ @@  @   @@   @@ @@@@@@@@ @@@@@@
-- @@@@@@@ @@  @@@  @@@@@      @@@    @
-- @     @@@@ @@ @ @@   @     @@ @   @@
rule110 :: Int -> Int -> Integer -> [String]
rule110 = cellAutomata 110

-- | 1D cell automata
-- Seed is the pattern of the top row
-- https://mathworld.wolfram.com/ElementaryCellularAutomaton.html
--
-- >>> putStrLn $ unlines $ cellAutomata 13 32 16 0x00008000
--                 @
-- @@@@@@@@@@@@@@@ @ @@@@@@@@@@@@@@
--                 @ @
-- @@@@@@@@@@@@@@@ @ @ @@@@@@@@@@@@
--                 @ @ @
-- @@@@@@@@@@@@@@@ @ @ @ @@@@@@@@@@
--                 @ @ @ @
-- @@@@@@@@@@@@@@@ @ @ @ @ @@@@@@@@
--                 @ @ @ @ @
-- @@@@@@@@@@@@@@@ @ @ @ @ @ @@@@@@
--                 @ @ @ @ @ @
-- @@@@@@@@@@@@@@@ @ @ @ @ @ @ @@@@
--                 @ @ @ @ @ @ @
-- @@@@@@@@@@@@@@@ @ @ @ @ @ @ @ @@
--                 @ @ @ @ @ @ @ @
-- @@@@@@@@@@@@@@@ @ @ @ @ @ @ @ @
cellAutomata :: Int -> Int -> Int -> Integer -> [String]
cellAutomata rule width height seed = fmap showCells $ take height $ iterate (fmap extract . duplicate) seedCells
  where
    -- create starting cells from seed
    f 0 _ = []
    f w s = odd s : f (w - 1) (div s 2)
    seedCells = let c : cs = f width seed in Cells (cs ++ repeat False) c (repeat False)
    -- print Cells
    showCells (Cells ls c _) =
      let p x = if x then '@' else ' '
       in fmap p (reverse (take (width - 1) ls)) ++ [p c]
    -- comonad-y functinos
    duplicate :: Cells Bool -> Cells (Cells Bool)
    duplicate cs = Cells (tail $ iterate shiftRight cs) cs (tail $ iterate shiftLeft cs)
      where
        shiftLeft (Cells ls c (r : rs)) = Cells (c : ls) r rs
        shiftRight (Cells (l : ls) c rs) = Cells ls l (c : rs)
    extract :: Cells Bool -> Bool
    extract (Cells (l : _) c (r : _)) = testBit rule ((if l then 4 else 0) + (if c then 2 else 0) + (if r then 1 else 0))

data Toothpick = None | Hori | Vert
  deriving (Eq)

-- | Graphics for toothpick sequence
-- https://oeis.org/A139250/a139250.anim.html
--
-- >>> putStrLn $ unlines $ toothpick 8
-- ╶┬─┬─┬─┬╴
--  ├┬┤ ├┬┤
--  │├┼─┼┤│
--  ├┴┼┬┼┴┤
--  │ │││ │
--  ├┬┼┴┼┬┤
--  │├┼─┼┤│
--  ├┴┤ ├┴┤
-- ╶┴─┴─┴─┴╴
toothpick :: Int -> [String]
toothpick 0 = []
toothpick n = draw $ iterate (fmap extract . duplicate2D) seed !! (n - 1)
  where
    seed =
      Cells2D
        ( Cells
            (repeat (Cells (repeat None) None (repeat None)))
            (Cells (repeat None) Vert (repeat None))
            (repeat (Cells (repeat None) None (repeat None)))
        )
    extract :: Cells2D Toothpick -> Toothpick
    extract (Cells2D (Cells _ (Cells (l : _) None (r : _)) _))
      | l == Hori && r == None || l == None && r == Hori = Vert
    extract (Cells2D (Cells (Cells _ u _ : _) (Cells _ None _) (Cells _ d _ : _)))
      | u == Vert && d == None || u == None && d == Vert = Hori
    extract (Cells2D (Cells _ (Cells _ c _) _)) = c
    draw cells = cutCells2D (div (n + 1) 2) (draw' <$> duplicate2D cells)
      where
        draw' (Cells2D (Cells (Cells _ u _ : _) (Cells _ Hori _) (Cells _ d _ : _))) = (if u == None then ['─', '┬'] else ['┴', '┼']) !! (if d == None then 0 else 1)
        draw' (Cells2D (Cells _ ((Cells (l : _) Vert (r : _))) _)) = (if l == None then ['│', '├'] else ['┤', '┼']) !! (if r == None then 0 else 1)
        draw' (Cells2D (Cells (Cells _ u _ : _) (Cells (l : _) c (r : _)) (Cells _ d _ : _))) =
          [' ', '╴', '╷', '┐', '╶', '─', '┌', '┬', '╵', '┘', '│', '┤', '└', '┴', '├', '┼'] !! ((if u == Vert then 8 else 0) + (if r == Hori then 4 else 0) + (if d == Vert then 2 else 0) + (if l == Hori then 1 else 0))
        cutCells2D x (Cells2D (Cells us cs ds)) = map (cutCells x) $ reverse (take x us) ++ [cs] ++ take x ds
          where
            cutCells x (Cells l c r) = reverse (take x l) ++ [c] ++ take x r
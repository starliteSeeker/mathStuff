module Art where

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

-- | https://en.wikipedia.org/wiki/Rule_110
data Cells c = Cells [c] c [c]

instance Functor Cells where
  fmap f (Cells ls c rs) = Cells (fmap f ls) (f c) (fmap f rs)

-- seed is the pattern of the top row
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
rule110 width height seed = fmap showCells $ take height $ iterate (fmap extract . duplicate) seedCells
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
    extract (Cells (l : ls) c (r : rs)) = not (l && c && r) && (c || r)

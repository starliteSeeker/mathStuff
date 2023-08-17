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

-- Inspired by https://youtu.be/JbfhzlMk2eY
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

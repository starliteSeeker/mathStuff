{-# LANGUAGE FlexibleContexts #-}

module Internal where

import qualified Data.Array as Arr

data Dir = R | U | L | D
  deriving (Eq, Show)

collect :: (Arr.Ix ix) => (ix, ix) -> [(ix, e)] -> [e]
collect ixRange ls = Arr.elems $ Arr.array ixRange ls

chunks :: Int -> [a] -> [[a]]
chunks width [] = []
chunks width ls = let (as, bs) = splitAt width ls in as : chunks width bs

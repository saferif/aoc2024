import Control.Arrow (first, (&&&))
import Control.Monad (join)
import Data.Array (Array)
import Data.Array.IArray (assocs, IArray, bounds, (!), indices, listArray)
import Data.Ix (Ix, inRange)
import Data.List (nub)

toArray :: [a] -> Array Int a
toArray l = listArray (0, length l - 1) l

maybeAt :: (IArray a e, Ix i) => i -> a i e -> Maybe e
maybeAt i arr = if inRange (bounds arr) i then Just (arr ! i) else Nothing

maybeAt2 :: (IArray a e, IArray a' (a i e), Ix i) => (i, i) -> a' i (a i e) -> Maybe e
maybeAt2 (row, col) arr = join $ fmap (maybeAt col) (maybeAt row arr)

indices2 :: (IArray a e, IArray a' (a i e), Ix i) => a' i (a i e) -> [(i, i)]
indices2 l = [(r, c) | (r, l') <- assocs l, c <- indices l']

addPoints :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPoints (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

parse :: String -> Array Int (Array Int Char)
parse = toArray . (fmap toArray) . lines

trailHeads :: Array Int (Array Int Char) -> [(Int, Int)]
trailHeads m = filter (\p -> maybeAt2 p m == Just '0') (indices2 m)

dfs :: [(Int, Int)] -> Array Int (Array Int Char) -> (Int, Int) -> [[(Int, Int)]]
dfs path m p = let pnext = fmap (addPoints p) [(-1, 0), (1, 0), (0, -1), (0, 1)]
                   curH = maybeAt2 p m
                   nextH = fmap succ curH
                   next = filter (\np -> maybeAt2 np m == nextH) pnext
                   newPath = p : path
               in if curH == Just '9' then [newPath] else next >>= dfs newPath m

main = interact $ show . sum . (uncurry fmap) . (first ((length . nub) .)) . ((dfs []) &&& trailHeads) . parse
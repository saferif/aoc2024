import Control.Arrow ((&&&))
import Control.Monad (join)
import Data.Array (Array, listArray, inRange)
import Data.Array.IArray (assocs, IArray, bounds, (!), indices)
import Data.Ix (Ix)
import Data.Maybe (isJust)
import Data.List (nub)

toArray :: [a] -> Array Int a
toArray l = listArray (0, length l - 1) l

maybeAt :: (IArray a e, Ix i) => i -> a i e -> Maybe e
maybeAt i arr = if inRange (bounds arr) i then Just (arr ! i) else Nothing

maybeAt2 :: (IArray a e, IArray a' (a i e), Ix i) => (i, i) -> a' i (a i e) -> Maybe e
maybeAt2 (row, col) arr = join $ fmap (maybeAt col) (maybeAt row arr)

indices2 :: (IArray a e, IArray a' (a i e), Ix i) => a' i (a i e) -> [(i, i)]
indices2 l = [(r, c) | (r, l') <- assocs l, c <- indices l']

pointsLess :: (Int, Int) -> (Int, Int) -> Bool
pointsLess (r1, c1) (r2, c2) = r1 < r2 || (r1 == r2 && c1 < c2)

pointPairs :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
pointPairs l = [(x, y) | x <- l, y <- l, pointsLess x y]

antinodes :: Array Int (Array Int Char) -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
antinodes mp (p1@(r1, c1), p2@(r2, c2)) = [an | an <- [an1, an2], notEmpty && sameFreq && isJust (maybeAt2 an mp)]
                                          where dr = r2 - r1
                                                dc = c2 - c1
                                                notEmpty = (maybeAt2 p1 mp) /= (Just '.')
                                                sameFreq = (maybeAt2 p1 mp) == (maybeAt2 p2 mp)
                                                an1 = (r1 - dr, c1 - dc)
                                                an2 = (r2 + dr, c2 + dc)

parse :: String -> Array Int (Array Int Char)
parse = toArray . (fmap toArray) . lines

main = interact $ show . length . nub . (uncurry (>>=)) . ((pointPairs . indices2) &&& antinodes) . parse

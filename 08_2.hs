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

maybeAt2 :: (IArray a e, IArray a' (a i e), Ix i) => a' i (a i e) -> (i, i) -> Maybe e
maybeAt2 arr (row, col) = join $ fmap (maybeAt col) (maybeAt row arr)

indices2 :: (IArray a e, IArray a' (a i e), Ix i) => a' i (a i e) -> [(i, i)]
indices2 l = [(r, c) | (r, l') <- assocs l, c <- indices l']

addPoints :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPoints (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

pointsLess :: (Int, Int) -> (Int, Int) -> Bool
pointsLess (r1, c1) (r2, c2) = r1 < r2 || (r1 == r2 && c1 < c2)

pointPairs :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
pointPairs l = [(x, y) | x <- l, y <- l, pointsLess x y]

antinodes :: Array Int (Array Int Char) -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
antinodes mp (p1@(r1, c1), p2@(r2, c2)) = if notEmpty && sameFreq then (an1 ++ an2) else []
                                          where dr = r2 - r1
                                                dc = c2 - c1
                                                notEmpty = (maybeAt2 mp p1) /= (Just '.')
                                                sameFreq = (maybeAt2 mp p1) == (maybeAt2 mp p2)
                                                an1 = takeWhile (isJust . (maybeAt2 mp)) $ iterate (addPoints (-dr, -dc)) p1
                                                an2 = takeWhile (isJust . (maybeAt2 mp)) $ iterate (addPoints (dr, dc)) p2

parse :: String -> Array Int (Array Int Char)
parse = toArray . (fmap toArray) . lines

main = interact $ show . length . nub . (uncurry (>>=)) . ((pointPairs . indices2) &&& antinodes) . parse

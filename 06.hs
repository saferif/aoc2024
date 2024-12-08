import Control.Monad (join)
import Data.Array (Array, listArray, inRange)
import Data.Array.IArray (assocs, IArray, bounds, (!))
import Data.Ix (Ix)
import Data.Foldable (find)
import Data.Maybe (isJust, fromJust)
import Data.List (nub)

toArray :: [a] -> Array Int a
toArray l = listArray (0, length l - 1) l

maybeAt :: (IArray a e, Ix i) => i -> a i e -> Maybe e
maybeAt i arr = if inRange (bounds arr) i then Just (arr ! i) else Nothing

maybeAt2 :: (IArray a e, IArray a' (a i e), Ix i) => (i, i) -> a' i (a i e) -> Maybe e
maybeAt2 (row, col) arr = join $ fmap (maybeAt col) (maybeAt row arr)

addPoints :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPoints (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

parse :: String -> Array Int (Array Int Char)
parse = toArray . (fmap toArray) . lines

turn :: (Int, Int) -> (Int, Int)
turn (row, col) = (col, -row)

findGuard :: Array Int (Array Int Char) -> (Int, Int)
findGuard = fmap (fst . fromJust) . fromJust . (find (isJust . snd)) . assocs . fmap (find ((=='^') . snd) . assocs)

followGuard :: Array Int (Array Int Char) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
followGuard mp position direction = let newPosition = addPoints position direction in
                                         case maybeAt2 newPosition mp of
                                            Nothing -> [position]
                                            Just '#' -> followGuard mp position (turn direction)
                                            otherwise -> (position:followGuard mp newPosition direction)

main = do
    text <- getContents
    let mp = parse text
    let guard = findGuard mp
    let steps = followGuard mp guard (-1, 0)
    print $ length (nub steps)

import Data.Array.Unboxed (listArray, Array, UArray, IArray, Ix, inRange, bounds, (!))
import Data.Maybe (fromMaybe)

parse :: String -> Array Int (UArray Int Char)
parse s = let ll = lines s in listArray (0, length ll - 1) $ fmap (\line -> listArray (0, length line - 1) line) ll

maybeAt :: (IArray a e, Ix i) => i -> a i e -> Maybe e
maybeAt i arr = if inRange (bounds arr) i then Just (arr ! i) else Nothing

addPoints :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPoints (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

checkDirection :: (Array Int (UArray Int Char)) -> String -> (Int, Int) -> (Int, Int) -> Int
checkDirection _ "" _ _ = 1
checkDirection maze (x:xs) p@(r, c) dir = let maybeRow = maybeAt r maze
                                              maybeCell = maybeRow >>= (maybeAt c)
                                              maybeNext = fromMaybe False (fmap (==x) maybeCell)
                                          in if maybeNext then checkDirection maze xs (addPoints p dir) dir else 0    

check :: (Array Int (UArray Int Char)) -> String -> (Int, Int) -> Int
check maze needle p = sum (fmap (checkDirection maze needle p) [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)])

findInRow :: (Array Int (UArray Int Char)) -> (Int, Int) -> Int -> Int
findInRow maze p@(r, c) ans = let row = maze ! r in if inRange (bounds row) c then findInRow maze (r, c + 1) ((check maze "XMAS" p) + ans) else ans

findInMaze :: (Int, Int) -> Int -> Array Int (UArray Int Char) -> Int
findInMaze p@(r, c) ans maze = if inRange (bounds maze) r then findInMaze (r + 1, 0) ((findInRow maze p 0) + ans) maze else ans

main = interact $ show . (findInMaze (0, 0) 0) . parse

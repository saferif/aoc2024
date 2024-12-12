import Control.Monad (join)
import Data.Array (Array)
import Data.Array.IArray(IArray, listArray, bounds, (!), assocs, indices)
import Data.Ix (Ix, inRange)
import Data.List (partition)
import Data.Set (Set, notMember, insert, empty)

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

parse :: String -> Array Int (Array Int Char)
parse = toArray . (fmap toArray) . lines

dfs :: Array Int (Array Int Char) -> Set (Int,Int) -> (Int, Int) -> ((Int, Int), Set (Int,Int))
dfs m seen p = let pnext = fmap (addPoints p) [(-1, 0), (1, 0), (0, -1), (0, 1)]
                   curP = maybeAt2 m p
                   (canGo, noGo) = partition (\np -> maybeAt2 m np == curP) pnext
                   seen' = insert p seen
               in if notMember p seen
                  then foldr (\np (accA, accSeen) -> let (a', newSeen) = dfs m accSeen np in (addPoints accA a', newSeen)) ((1, length noGo), seen') canGo
                  else ((0, 0), seen)

main = do
    text <- getContents
    let m = parse text
    print $ fst $ foldr (\np (acc, seen) -> let ((area, perim), seen') = dfs m seen np in (acc + area * perim, seen')) (0, empty) (indices2 m)

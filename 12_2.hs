import Control.Monad (join, foldM, liftM2)
import Control.Monad.State (State, get, put, evalState, state)
import Data.Array (Array)
import Data.Array.IArray(IArray, listArray, bounds, (!), assocs, indices)
import Data.Ix (Ix, inRange)
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

turnLeft :: (Int, Int) -> (Int, Int)
turnLeft (r, c) = (c, -r)

turnRight :: (Int, Int) -> (Int, Int)
turnRight (r,c) = (-c, r)

directions :: [(Int, Int)]
directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]

buildFence :: Array Int (Array Int Char) -> (Int, Int) -> (Int, Int) -> State (Set ((Int, Int), (Int, Int))) Int
buildFence m p d = let curP = maybeAt2 m p
                       fenceContinues = \np -> maybeAt2 m np == curP && maybeAt2 m (addPoints np d) /= curP
                       leftSide = fmap (\x -> (x, d)) $ takeWhile fenceContinues $ iterate (addPoints $ turnLeft d) p
                       rightSide = fmap (\x -> (x, d)) $ takeWhile fenceContinues $ iterate (addPoints $ turnRight d) p
                   in state $ \fence -> (fromEnum $ notMember (p, d) fence, foldr insert fence ((p, d) : leftSide ++ rightSide))

dfs :: Array Int (Array Int Char) -> (Int, Int) -> State (Set (Int, Int)) (Int, State (Set ((Int, Int), (Int, Int))) Int)
dfs m p = do
    seen <- get
    let pnext = fmap (addPoints p) directions
        curP = maybeAt2 m p
        canGo = filter (\np -> maybeAt2 m np == curP) pnext
        wrongDir = filter (\d -> maybeAt2 m (addPoints p d) /= curP) directions
        sides = foldM (\acc d -> (acc +) <$> (buildFence m p d)) 0 wrongDir
        seen' = insert p seen
        combine (area1, sidesM1) (area2, sidesM2) = (area1 + area2, (liftM2 (+)) sidesM1 sidesM2)
    if notMember p seen
      then put seen' >> foldM (\acc np -> (combine acc) <$> (dfs m np)) (1, sides) canGo
      else return (0, pure 0)

main = do
    text <- getContents
    let m = parse text
        price (a, ma) = a * (evalState ma empty)
        f = foldM (\acc p -> ((acc +) . price) <$> (dfs m p)) 0 (indices2 m)
    print $ evalState f empty
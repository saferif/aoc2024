import Data.Set (Set, empty, insert, member)
import Data.Maybe (listToMaybe)

parse :: [String] -> ((Int, Int), (Int, Int), (Int, Int), Set (Int, Int))
parse m = foldr acc ((0, 0), (0, 0), (0, 0), empty) [(r, c, ch) | (row, r) <- zip m [0..], (ch, c) <- zip row [0..]]
    where acc (r, c, ch) ((rc, cc), start, end, walls) =
            case ch of
                '#' -> ((max rc (r + 1), max cc (c + 1)), start, end, insert (r, c) walls)
                'S' -> ((max rc (r + 1), max cc (c + 1)), (r, c), end, walls)
                'E' -> ((max rc (r + 1), max cc (c + 1)), start, (r, c), walls)
                '.' -> ((max rc (r + 1), max cc (c + 1)), start, end, walls)

addPoints :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPoints (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

distPoints :: (Int, Int) -> (Int, Int) -> Int
distPoints (r1, c1) (r2, c2) = (abs $ r1 - r2) + (abs $ c1 - c2)

dfs :: Int -> Int -> Set (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
dfs rc cc walls end path (pos:queue) =
    let pnext = fmap (addPoints pos) [(0, 1), (0, -1), (1, 0), (-1, 0)]
        legal = filter (\p@(r, c) -> r >= 0 && c >= 0 && r < rc && c < cc && (not $ member p walls)) pnext
        notPrev = filter (\p -> (Just p) /= (listToMaybe path)) legal
        path' = pos:path
    in if pos == end then path' else dfs rc cc walls end path' (notPrev ++ queue)

main = do
    text <- getContents
    let ((rc, cc), start, end, walls) = parse $ lines text
        distances = zip (dfs rc cc walls end [] [start]) [0..]
        ans = [1 | (start, startDist) <- distances, (end, endDist) <- distances, let direct = distPoints start end, startDist > endDist && startDist - (direct + endDist) > 99 && direct <= 2]
    print $ sum ans
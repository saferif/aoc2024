import qualified Data.Set as S (Set, empty, insert, member, findMin, deleteMin, union, size)
import qualified Data.Map as M (Map, empty, insertWith, member, lookup)

data State = State (Int, Int) (Int, Int) Int

instance Eq State where
    (==) (State p1 d1 c1) (State p2 d2 c2) = p1 == p2 && d1 == d2 && c1 == c2

instance Ord State where
    (<=) (State p1 d1 c1) (State p2 d2 c2) = c1 <= c2

parse :: [String] -> ((Int, Int), (Int, Int), (Int, Int), S.Set (Int, Int))
parse m = foldr acc ((0, 0), (0, 0), (0, 0), S.empty) [(r, c, ch) | (row, r) <- zip m [0..], (ch, c) <- zip row [0..]]
    where acc (r, c, ch) ((rc, cc), start, end, walls) =
            case ch of
                '#' -> ((max rc (r + 1), max cc (c + 1)), start, end, S.insert (r, c) walls)
                'S' -> ((max rc (r + 1), max cc (c + 1)), (r, c), end, walls)
                'E' -> ((max rc (r + 1), max cc (c + 1)), start, (r, c), walls)
                '.' -> ((max rc (r + 1), max cc (c + 1)), start, end, walls)

addPoints :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPoints (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

turnLeft :: (Int, Int) -> (Int, Int)
turnLeft (r,c) = (-c, r)

turnRight :: (Int, Int) -> (Int, Int)
turnRight (r, c) = (c, -r)

inside :: Int -> Int -> (Int, Int) -> Bool
inside rc cc (r, c) = r >= 0 && r < rc && c >= 0 && c < cc

dijkstra :: Int -> Int -> S.Set (Int, Int) -> ((Int, Int), (Int, Int)) -> (Int, Int) -> M.Map ((Int, Int), (Int, Int)) Int
dijkstra rc cc walls (start, startDir) end = dijkstra' M.empty S.empty (State start startDir 0)
    where dijkstra' seen queue (State pos dir cost) =
            if pos == end then M.insertWith min (pos, dir) cost seen
            else let forward = addPoints pos dir
                     possiblePositions = [
                            State forward dir (cost + 1),
                            State pos (turnLeft dir) (cost + 1000),
                            State pos (turnRight dir) (cost + 1000)
                        ]
                     newPositions = filter (\(State p d _) -> (inside rc cc p) && (not (S.member p walls)) && (not (M.member (p, d) seen))) possiblePositions
                     queue' = foldr S.insert queue newPositions
                     next = S.findMin queue'
                     rest = S.deleteMin queue'
                     seen' = M.insertWith min (pos, dir) cost seen
                 in dijkstra' seen' rest next

dfs :: M.Map ((Int, Int), (Int, Int)) Int -> (Int, Int) -> S.Set (Int, Int) -> Int -> ((Int, Int), (Int, Int)) -> [(S.Set (Int, Int))]
dfs path end seen cost (pos, dir)
    | M.lookup (pos, dir) path /= Just cost = []
    | pos == end = [seen']
    | otherwise = next >>= (\(cost', st') -> dfs path end seen' cost' st')
    where next = [(cost + 1, (addPoints pos dir, dir)), (cost + 1000, (pos, turnLeft dir)), (cost + 1000, (pos, turnRight dir))]
          seen'= S.insert pos seen

main = do
    text <- getContents
    let ((rc, cc), start, end, walls) = parse (lines text)
        path = dijkstra rc cc walls (start, (0, 1)) end
        paths = dfs path end S.empty 0 (start, (0, 1))
        allTiles = foldr S.union S.empty paths
    print $ S.size allTiles
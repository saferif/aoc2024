import Data.Set (Set, empty, insert, member, findMin, deleteMin)

data State = State (Int, Int) (Int, Int) Int

instance Eq State where
    (==) (State p1 d1 c1) (State p2 d2 c2) = p1 == p2 && d1 == d2 && c1 == c2

instance Ord State where
    (<=) (State p1 d1 c1) (State p2 d2 c2) = c1 <= c2

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

turnLeft :: (Int, Int) -> (Int, Int)
turnLeft (r,c) = (-c, r)

turnRight :: (Int, Int) -> (Int, Int)
turnRight (r, c) = (c, -r)

inside :: Int -> Int -> (Int, Int) -> Bool
inside rc cc (r, c) = r >= 0 && r < rc && c >= 0 && c < cc

dijkstra :: Int -> Int -> Set (Int, Int) -> (Int, Int) -> (Int, Int) -> Int
dijkstra rc cc walls start end = dijkstra' empty empty (State start (0, 1) 0)
    where dijkstra' seen queue (State pos dir cost) =
            if pos == end then cost
            else let forward = addPoints pos dir
                     maybeForward = if (inside rc cc forward) && not (member forward walls) then [State forward  dir (cost + 1)] else []
                     possiblePositions = maybeForward ++ [State pos (turnLeft dir) (cost + 1000), State pos (turnRight dir) (cost + 1000)]
                     newPositions = if member (pos, dir) seen then [] else possiblePositions
                     queue' = foldr insert queue newPositions
                     next = findMin queue'
                     rest = deleteMin queue'
                     seen' = insert (pos, dir) seen
                 in dijkstra' seen' rest next

main = do
    text <- getContents
    let ((rc, cc), start, end, walls) = parse (lines text)
    print $ dijkstra rc cc walls start end

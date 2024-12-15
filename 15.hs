import Data.Set (Set, empty, insert, member, delete, elems)

addPoints :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPoints (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

parse :: [String] -> ((Int, Int), (Int, Int), Set (Int, Int), Set (Int, Int))
parse m = foldr acc ((0, 0), (0, 0), empty, empty) [(r, c, ch) | (row, r) <- zip m [0..], (ch, c) <- zip row [0..]]
    where acc (r, c, ch) (pos, (rc, cc), walls, boxes) =
            case ch of
                '#' -> (pos, (max rc (r + 1), max cc (c + 1)), insert (r, c) walls, boxes)
                'O' -> (pos, (max rc (r + 1), max cc (c + 1)), walls, insert (r, c) boxes)
                '@' -> ((r, c), (max rc (r + 1), max cc (c + 1)), walls, boxes)
                '.' -> (pos, (max rc (r + 1), max cc (c + 1)), walls, boxes)

direction :: Char -> (Int, Int)
direction '^' = (-1, 0)
direction '>' = (0, 1)
direction 'v' = (1, 0)
direction '<' = (0, -1)

inside :: Int -> Int -> (Int, Int) -> Bool
inside rc cc (r, c) = r >= 0 && r < rc && c >= 0 && c < cc

move :: Int -> Int -> ((Int, Int), Set (Int, Int), Set (Int, Int)) -> Char -> ((Int, Int), Set (Int, Int), Set (Int, Int))
move rc cc m@(pos, walls, boxes) ch =
    let dir = direction ch
        nextPos = addPoints pos dir
    in if not (inside rc cc nextPos) || (member nextPos walls) then m
       else if not (member nextPos walls) && not (member nextPos boxes) && (inside rc cc nextPos)
            then (nextPos, walls, boxes)
            else let (pastBoxes, nextNotBox:_) = break (\p -> not (member p boxes)) $ iterate (addPoints dir) nextPos
                 in if not (inside rc cc nextNotBox) || (member nextNotBox walls) then m
                    else let boxes' = foldr (\box acc -> insert (addPoints box dir) (delete box acc)) boxes pastBoxes
                         in (nextPos, walls, boxes')

main = do
    text <- getContents
    let (m, _:d) = break (==[]) $ lines text
        (pos, (rc, cc), walls, boxes) = parse m
        (_, _, boxes') = foldl (move rc cc) (pos, walls, boxes) (concat d)
    print $ sum $ fmap (\(r, c) -> r * 100 + c) (elems boxes')

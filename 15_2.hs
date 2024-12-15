import Control.Monad (msum, foldM)
import Data.Set (Set, empty, insert, member, delete, elems)
import Data.List (isPrefixOf, nub)

replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace from to s = concat $ replace' s
    where replace' s
            | s == [] = []
            | isPrefixOf from s = to : replace' (drop (length from) s)
            | otherwise = [head s] : replace' (tail s)

addPoints :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPoints (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

parse :: [String] -> ((Int, Int), (Int, Int), Set (Int, Int), Set (Int, Int))
parse m = foldr acc ((0, 0), (0, 0), empty, empty) [(r, c, ch) | (row, r) <- zip m [0..], (ch, c) <- zip row [0..]]
    where acc (r, c, ch) (pos, (rc, cc), walls, boxes) =
            case ch of
                '#' -> (pos, (max rc (r + 1), max cc (c + 1)), insert (r, c) walls, boxes)
                '[' -> (pos, (max rc (r + 1), max cc (c + 1)), walls, insert (r, c) boxes)
                '@' -> ((r, c), (max rc (r + 1), max cc (c + 1)), walls, boxes)
                '.' -> (pos, (max rc (r + 1), max cc (c + 1)), walls, boxes)
                ']' -> (pos, (max rc (r + 1), max cc (c + 1)), walls, boxes)

direction :: Char -> (Int, Int)
direction '^' = (-1, 0)
direction '>' = (0, 1)
direction 'v' = (1, 0)
direction '<' = (0, -1)

inside :: Int -> Int -> (Int, Int) -> Bool
inside rc cc (r, c) = r >= 0 && r < rc && c >= 0 && c < cc

findBox :: Set (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
findBox boxes p = let f s e = if member e s then Just e else Nothing in msum [f boxes p, f boxes (addPoints p (0, -1))]

findBoxes :: Set (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
findBoxes boxes p d = nub $ findBoxes' p d
    where findBoxes' p d
            | box == Nothing = []
            | otherwise = box' : (next >>= (\n -> findBoxes' n d))
            where box = findBox boxes p
                  (Just box') = box
                  next = filter (\b -> findBox boxes b /= box) [addPoints box' d, addPoints (addPoints box' (0, 1)) d]

moveBox :: Int -> Int -> (Int, Int) -> Set (Int, Int) -> Set (Int, Int) -> (Int, Int) -> Maybe (Set (Int, Int))
moveBox rc cc d walls boxes box = let p1 = addPoints box d
                                      p2 = addPoints (addPoints box (0, 1)) d
                                      isEmpty p = (inside rc cc p) && not (member p walls)
                                  in if (isEmpty p1) && (isEmpty p2)
                                     then Just $ insert (addPoints box d) boxes
                                     else Nothing

move :: Int -> Int -> ((Int, Int), Set (Int, Int), Set (Int, Int)) -> Char -> ((Int, Int), Set (Int, Int), Set (Int, Int))
move rc cc m@(pos, walls, boxes) ch =
    let dir = direction ch
        nextPos = addPoints pos dir
        isBox p = findBox boxes p /= Nothing
    in if not (inside rc cc nextPos) || (member nextPos walls) then m
       else if not (member nextPos walls) && not (isBox nextPos) && (inside rc cc nextPos)
            then (nextPos, walls, boxes)
            else let affected = findBoxes boxes nextPos dir
                     moved = foldM (moveBox rc cc dir walls) (foldr delete boxes affected) affected
                 in case moved of
                      Nothing -> m
                      Just boxes' -> (nextPos, walls, boxes')

main = do
    text <- getContents
    let expanded = foldr (\(from, to) s -> replace from to s) text [("#", "##"), ("O", "[]"), ("@", "@."), (".", "..")]
        (m, _:d) = break (==[]) $ lines expanded
        (pos, (rc, cc), walls, boxes) = parse m
        (_, _, boxes') = foldl (move rc cc) (pos, walls, boxes) (concat d)
    print $ sum $ fmap (\(r, c) -> r * 100 + c) (elems boxes')

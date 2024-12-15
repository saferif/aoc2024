import Text.Regex.Posix ((=~))

parse :: String -> [((Integer, Integer), (Integer, Integer))]
parse = fmap (parse' . (fmap read) . tail) . (=~ "p=(-?[0-9]+),(-?[0-9]+) v=(-?[0-9]+),(-?[0-9]+)")
  where parse' [px, py, vx, vy] = ((px, py), (vx, vy))

w = 101
h = 103

quadrants = [(0 , 0, w `div` 2, h `div` 2), (w `div` 2 + 1, 0, w, h `div` 2), (0, h `div` 2 + 1, w `div` 2, h), (w `div` 2 + 1, h `div` 2 + 1, w, h)]

step ((px, py), (vx, vy)) = ((add' w px vx, add' h py vy), (vx, vy))
    where add' m a b = let s = a + b
                           s' = if s < 0 then s + m else s
                       in if s' >= m then s' - m else s'
                       
inside (x, y) (lx, ly, hx, hy) = if x >= lx && x < hx && y >= ly && y < hy then 1 else 0

main = interact $ show . product . foldr (zipWith (+)) [0, 0, 0, 0] . fmap (((flip fmap) quadrants) . inside . fst . (!! 100) . (iterate step)) . parse

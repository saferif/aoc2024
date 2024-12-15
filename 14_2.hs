import Control.Monad (void)
import Text.Parsec.String (Parser)
import Text.Parsec (runParser, string, digit, many1, option, char, optional)
import Data.List (find, nub)
import Data.Either (fromRight)

parseInt :: Parser Int
parseInt = do
    op <- option id (char '-' >> return negate)
    a <- many1 digit
    return $ op (read a)

parseRobot :: Parser ((Int, Int), (Int, Int))
parseRobot = do
    void $ string "p="
    px <- parseInt
    void $ char ','
    py <- parseInt
    void $ char ' '
    void $ string "v="
    vx <- parseInt
    void $ char ','
    vy <- parseInt
    optional $ char '\n'
    return ((px, py), (vx, vy))

w = 101
h = 103

quadrants = [(0 , 0, w `div` 2, h `div` 2), (w `div` 2 + 1, 0, w, h `div` 2), (0, h `div` 2 + 1, w `div` 2, h), (w `div` 2 + 1, h `div` 2 + 1, w, h)]

step ((px, py), (vx, vy)) = ((add' w px vx, add' h py vy), (vx, vy))
    where add' m a b = let s = a + b
                           s' = if s < 0 then s + m else s
                       in if s' >= m then s' - m else s'
                       
inside :: (Int, Int) -> (Int, Int, Int, Int) -> Int
inside (x, y) (lx, ly, hx, hy) = if x >= lx && x < hx && y >= ly && y < hy then 1 else 0

draw :: [((Int, Int), (Int, Int))] -> IO ()
draw t = draw' 0 0
    where draw' x y
           | y == h = putChar '\n'
           | x == w = putChar '\n' >> draw' 0 (y + 1)
           | otherwise = (if (find ((== (x, y)) . fst) t) == Nothing then putChar '.' else putChar '*') >> draw' (x + 1) y

intoQs :: [(Int, Int)] -> [Int]
intoQs = (foldr (zipWith (+)) [0, 0, 0, 0]) . (fmap (((flip fmap) quadrants) . inside))

main = do
    text <- getContents
    let robots = fromRight [] $ runParser (many1 parseRobot) () "" text
        sim = zip (iterate (fmap step) robots) [0..]
        qs = fmap (\(rs, i) -> (rs, i, intoQs (fmap fst rs))) sim
        uniques = filter (\(rs, i, qs') -> (length rs) == (length $ nub $ fmap fst rs)) qs
        (tree, ans, q) = head uniques
    print ans
    draw tree

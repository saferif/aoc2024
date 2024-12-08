import Data.IntMap (IntMap, insert, insertWith, empty, singleton, union, intersection, elems, findWithDefault)
import Data.Maybe (listToMaybe)

split :: Char -> String -> [String]
split delim line = case break (==delim) line of
                    (x, "") -> [x]
                    (x, _:y) -> x : split delim y

parseRules :: [String] -> IntMap (IntMap ())
parseRules = let ruleParts = break (=='|')
                 readParts (before, _:after) = (read before, read after)
                 accumulate m x = insertWith union k (singleton v ()) m where (k, v) = readParts (ruleParts x)
             in foldl accumulate empty
             
parsePages :: [String] -> [[Int]]
parsePages = fmap ((fmap read) . (split ','))

swap :: [a] -> Int -> Int -> [a]
swap l i1 i2 = let (prefix, rest) = splitAt i1 l
                   (x:rest') = rest
                   (mid, rest'') = splitAt (i2 - i1 - 1) rest'
                   (y:suffix) = rest''
               in prefix ++ (y:(mid ++ (x:suffix)))
                                        
fixPages :: IntMap (IntMap ()) -> IntMap Int -> Int -> [Int] -> [Int] -> [Int]
fixPages _ _ _ acc [] = reverse acc
fixPages rules seen idx acc cur@(nextPage:rest) = let cont = fixPages rules (insert nextPage idx seen) (idx + 1) (nextPage:acc) rest
                                                      again wi = fixPages rules empty 0 [] (swap ((reverse acc) ++ cur) wi idx)
                                                      wrong = listToMaybe (elems (intersection seen (findWithDefault empty nextPage rules)))
                                                  in maybe cont again wrong

middle :: [Int] -> Int
middle l = l !! ((length l) `div` 2)

main = do
    text <- getContents
    let (rules, _:pages) = break (=="") (lines text)
    let parsedPages = parsePages pages
    let fixedPages = map (fixPages (parseRules rules) empty 0 []) parsedPages
    let filtered = filter (uncurry (==)) (zip parsedPages fixedPages)
    let middles = fmap (middle . snd) filtered
    print $ sum middles

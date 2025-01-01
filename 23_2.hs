import Data.Set (Set, member, insert, empty, elems, singleton, size)
import Data.List (nub, maximumBy, intersperse)
import Data.Ord (comparing)

collectComps :: Set String -> [String] -> Set (String, String) -> Set String
collectComps set [] _ = set
collectComps set (c : comps) conns =
    let set' = if all (\s -> member (c, s) conns) $ elems set then insert c set else set
    in collectComps set' comps conns

main = do
    text <- getContents
    let parsed = fmap (\l -> let (a, _:b) = break (=='-') l in (a, b)) $ lines text
        conns = foldr (\(a, b) s -> insert (a, b) (insert (b, a) s)) empty parsed
        comps = nub $ parsed >>= (\(a, b) -> [a, b])
        sets = fmap (\c -> collectComps (singleton c) comps conns) comps
        largest = maximumBy (comparing size) sets
    putStr $ concat $ intersperse "," $ elems largest

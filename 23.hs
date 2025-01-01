import Data.Set (member, insert, empty)
import Data.List (nub, sort)

main = do
    text <- getContents
    let parsed = fmap (\l -> let (a, _:b) = break (=='-') l in (a, b)) $ lines text
        set = foldr (\(a, b) s -> insert (a, b) (insert (b, a) s)) empty parsed
        comps = nub $ parsed >>= (\(a, b) -> [a, b])
        res = nub [sort [a, b, c] | (a, b) <- parsed, c <- comps, (member (a, c) set) && (member (b, c) set) && (take 1 c == ['t'])]
    print $ length res

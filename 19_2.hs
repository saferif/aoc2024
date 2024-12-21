import Control.Monad.State (State, state, get, evalState)
import Control.Monad (foldM)
import Data.List (isPrefixOf)
import Data.Map.Strict (Map, insert, fromList, (!?), empty, keys)

split :: String -> String -> [String]
split delim s = split' "" delim s
    where
        split' acc _ "" = [reverse acc]
        split' acc delim s@(h : t)
            | isPrefixOf delim s = (reverse acc) : (split' "" delim (drop (length delim) s))
            | otherwise = split' (h : acc) delim t

parse :: String -> ([String], [String])
parse s =
    let (patterns, _:designs) = break (==[]) $ lines s
    in (split ", " (concat patterns), designs)

check :: Map String () -> String -> State (Map String Int) Int
check patterns target
    | target == "" = return 1
    | otherwise = do
        st <- get
        let prefixes = filter ((flip isPrefixOf) target) $ keys patterns
            count = foldM (\count p -> fmap (count +) $ check patterns (drop (length p) target)) 0 prefixes
        case st !? target of
            Just r -> return r
            Nothing -> count >>= (\count' -> state (\st' -> (count', insert target count' st')))

main = do
    text <- getContents
    let (patterns, designs) = parse text
        patterns' = fromList $ fmap (\p -> (p, ())) patterns
    print $ evalState (foldM (\count d -> fmap (count +) $ check patterns' d) 0 designs) empty
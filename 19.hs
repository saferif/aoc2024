import Control.Monad.State (State, state, runState, get, put)
import Control.Monad (when)
import Data.List (isPrefixOf, find)
import Data.Set (Set, insert, member, fromList, toDescList)

split :: String -> String -> [String]
split delim s = split' "" delim s
    where
        split' :: String -> String -> String -> [String]
        split' acc _ "" = [reverse acc]
        split' acc delim s@(h:t)
            | isPrefixOf delim s = (reverse acc) : (split' "" delim (drop (length delim) s))
            | otherwise = split' (h : acc) delim t

parse :: String -> ([String], [String])
parse s =
    let (patterns, _:designs) = break (==[]) $ lines s
    in (split ", " (concat patterns), designs)

check :: String -> State (Set String) Bool
check target = check' ("", target)
    where
        check' :: (String, String) -> State (Set String) Bool
        check' (x, "") = state (\st -> (member x st, st))
        check' (prefix, c:rest) = do
            st <- get
            let cur = prefix ++ [c]
                splitted = find (\p -> member (drop (length p) cur) st) $ filter ((flip isPrefixOf) cur) $ toDescList st
            when (not (member cur st) && splitted /= Nothing) (put $ insert cur st) >> check' (cur, rest)

main = do
    text <- getContents
    let (patterns, designs) = parse text
    print $ fst $ foldr (\d (cnt, st) -> let (ok, st') = runState (check d) st in (fromEnum ok + cnt, st')) (0, fromList patterns) designs

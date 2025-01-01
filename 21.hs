import Control.Monad (foldM)
import Control.Monad.State (State, get, mapState, evalState)
import Data.Map.Strict (Map, fromList, empty, insert, (!), (!?))

withCoords :: [String] -> Map Char (Int, Int)
withCoords s = fromList [(ch, (r, c)) | (r, row) <- zip [0..] s, (c, ch) <- zip [0..] row]

path :: Map Char (Int, Int) -> Char -> Char -> String
path pad from to =
    let (fr, fc) = pad ! from
        (tr, tc) = pad ! to
        (hr, hc) = pad ! ' '
        rc = if tr < fr then '^' else 'v'
        cc = if tc < fc then '<' else '>'
        rm = replicate (abs $ fr - tr) rc
        cm = replicate (abs $ fc - tc) cc
    in if fr == hr && tc == hc then rm ++ cm ++ "A"
        else if fc == hc && tr == hr then cm ++ rm ++ "A"
        else if cc == '<' then cm ++ rm ++ "A"
        else rm ++ cm ++ "A"

numericPad = path $ withCoords ["789", "456", "123", " 0A"]
directionalPad = path $ withCoords [" ^A", "<v>"]

seqLength :: [Char -> Char -> String] -> String -> State (Map (Char, Char, Int) Int) Int
seqLength pads code =
        fld 0 code
    where
        fld :: Int -> String -> State (Map (Char, Char, Int) Int) Int
        fld level dirs = fmap fst $ foldM (\(len, cur) next -> fmap (\r -> (len + r, next)) (go cur next level)) (0, 'A') dirs
        go cur next level =
            let dirs = (pads !! level) cur next
                nextLevel = fld (level + 1) dirs
                res = if (length pads) == (level + 1) then return (length dirs) else nextLevel
                k = (cur, next, level)
            in do
                cache <- get
                case cache !? k of
                    Just v -> return v
                    Nothing -> mapState (\(v, s) -> (v, insert k v s)) res


parse :: String -> [(Int, String)]
parse s = fmap (\l -> (read $ init l, l)) $ lines s

main = do
    text <- getContents
    let buttons = seqLength $ numericPad : (replicate 2 directionalPad)
        res = foldM (\acc (a, l) -> fmap (\x -> acc + a * x) (buttons l)) 0 $ parse text
    print $ evalState res empty
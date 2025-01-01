import Data.Map.Strict (Map, assocs, fromList, member, insert, (!))
import Data.List (find)

type Gate = (String, (Bool -> Bool -> Bool), String, String)

parseOp :: String -> (Bool -> Bool -> Bool)
parseOp "AND" = (&&)
parseOp "OR" = (||)
parseOp "XOR" = (/=)

parseWires :: [String] -> Map String Bool
parseWires wires =
    fromList $ fmap (\wire -> let (name, _:v) = break (==':') wire in (name, read v == 1)) wires

parseGates :: [String] -> [Gate]
parseGates gates =
    fmap (\gate -> let [lhs, op, rhs, _, res] = words gate in (res, parseOp op, lhs, rhs)) gates

parse :: String -> (Map String Bool, [Gate])
parse s = 
    let (wires, _:gates) = break (==[]) $ lines s in (parseWires wires, parseGates gates)

simulate :: Map String Bool -> [Gate] -> Map String Bool
simulate wires gates =
    case find (\(res, _, lhs, rhs) -> (not $ member res wires) && (member lhs wires) && (member rhs wires)) gates of
        Nothing -> wires
        Just (res, op, lhs, rhs) -> simulate (insert res (op (wires ! lhs) (wires ! rhs)) wires) gates

main = do
    text <- getContents
    let (wires, gates) = parse text
        wires' = simulate wires gates
        ans = foldr (\(_, v) acc -> acc * 2 + (fromEnum v)) 0 $ filter (\(name, _) -> take 1 name == "z") $ assocs wires'
    print ans

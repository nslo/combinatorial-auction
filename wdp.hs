import System.Environment
import Data.List
import Numeric.LinearProgramming

main = do
    [s, t] <- getArgs    
    bids <- parseFile s
    let flag = read t :: Int
        gLo = getLowerBound bids
        gHi = getUpperBound bids flag
        in putStrLn (show (solveWDP bids gLo gHi 0 0 flag))

-- parse input file
parseFile path = do
    contents <- readFile path
    let parseBids l =   ( (read (head (words l)) :: Int
                        , read ((words l) !! 1) :: Double)
                        , map (\ x -> read x :: Int) (drop 2 (words l)))
        in return (map parseBids (tail (lines contents)))

-- helper functions
sans x y = filter (/= x) y
bidder bid = fst (fst bid)
price bid = snd (fst bid)
items bid = snd bid
difBidders x y = bidder x /= bidder y
noItemsInCommon x y = and (map (`notElem` (items y)) (items x))
getValid bid remaining = filter (noItemsInCommon bid)
    (filter (difBidders bid) remaining)
greaterBid x y = if price x > price y then x else y
getHighestBid bids = foldl greaterBid ((0, 0), [0]) bids
getUniqueBidders bids = nub (map bidder bids)
numBids bids = length bids
boolToInt True  = 1
boolToInt False = 0
numPlayers bids
    | null bids = 0
    | otherwise = maximum $ map bidder bids
numItems bids
    | null bids = 0
    | otherwise = maximum $ map maximum $ map items bids

-- take the highest bid, then the next highest non-invalidated bid, etc
getLowerBound bids
    | null bids = 0
    | otherwise = price highest + getLowerBound (getValid highest bids)
        where highest = getHighestBid bids

-- get the highest bid from each bidder
getUpperBound bids flag
    | null bids = 0
    | flag == 1 = x 
    | otherwise = sum $ map price $ map getHighestBid $
        map (\id -> filter (\b -> bidder b == id) bids) (getUniqueBidders bids)
    where Optimal (x, _) = simplexBids bids

-- call as: playerConstr bids (numPlayers bids) []
playerConstr bids n constrList
    | n == 0 = constrList
    | otherwise = playerConstr bids (n - 1)
        (((map (boolToInt . (\ b -> (bidder b == n))) bids) :<=: 1) : constrList)

-- call as: itemConstr bids (numItems bids) []
itemConstr bids n constrList
    | n == 0 = constrList
    | otherwise = itemConstr bids (n - 1)
        (((map (boolToInt . (\ b -> (elem n (items b)))) bids) :<=: 1) : constrList)

-- get upper bound with linear programming
simplexBids bids  = simplex prob constr []
    where   prob    = Maximize (map price bids)
            constr  = Dense (pc ++ ic)
            pc      = playerConstr bids (numPlayers bids) []       
            ic      = itemConstr bids (numItems bids) []

--solveWDP :: [((Int, Float), [Int])] -> Float -> Float -> Float
solveWDP remaining gLo gHi revenue n flag
    | revenue == gHi = revenue  -- we're done
    | null remaining = max revenue gLo  -- no more bids
    | rejectUpper > acceptLower && rejectUpper > gLo && acceptUpper > rejectLower
        && acceptUpper > gLo = max wdpAccept wdpReject
    | rejectUpper < acceptLower || rejectUpper < gLo = wdpAccept
    | acceptUpper < rejectLower || acceptUpper < gLo = wdpReject
    | otherwise = revenue
        where   bid         = head remaining
                acceptLower = revenue + (price bid) +
                    getLowerBound (getValid bid remaining)
                acceptUpper = revenue + (price bid) +
                    getUpperBound (getValid bid remaining) flag
                rejectLower = revenue + getLowerBound (sans bid remaining)
                rejectUpper = revenue + getUpperBound (sans bid remaining) flag
                wdpAccept   = solveWDP (getValid bid remaining) updateLoA
                    gHi (revenue + (price bid)) (n + 1) flag
                wdpReject   = solveWDP (sans bid remaining) updateLoR gHi revenue
                    (n + 1) flag
                updateLoA   = max (revenue + (price bid)) gLo
                updateLoR   = max revenue gLo

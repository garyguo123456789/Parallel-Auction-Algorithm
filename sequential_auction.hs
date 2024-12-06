module SequentialAuction (auctionAlgorithm, optimalAssignment) where
  -- This allows us to access the functions in our test files

import Data.List (maximumBy, permutations)
import Data.Ord (comparing)
import qualified Data.Map as Map

type Bidder = Int
type Item = Int
type PayoffMatrix = [[Double]]
type Prices = Map.Map Item Double
type Assignment = Map.Map Bidder Item

-- printAuctionResults :: PayoffMatrix -> Assignment -> IO ()
-- printAuctionResults matrix assignment = do
--     putStrLn "Assignment (Bidder -> Item):"
--     mapM_ (\(bidder, item) -> putStrLn $ "Bidder " ++ show bidder ++ " -> Item " ++ show item)
--           (Map.toList assignment)

--     putStrLn "\nPayoff Matrix:"
--     mapM_ print matrix

--     putStrLn "\nTotal Payoff Breakdown:"
--     let payoffBreakdown = [(bidder, item, matrix !! bidder !! item)
--                            | (bidder, item) <- Map.toList assignment]
--     mapM_ (\(b, i, p) -> putStrLn $ "Bidder " ++ show b ++ " -> Item " ++ show i ++ ": " ++ show p)
--           payoffBreakdown

--     let totalPayoff = sum [matrix !! bidder !! item | (bidder, item) <- Map.toList assignment]
--     putStrLn $ "\nTotal Payoff: " ++ show totalPayoff

auctionAlgorithm :: Double -> PayoffMatrix -> Assignment
auctionAlgorithm epsilon inputMatrix = go initialUnassigned initialPrices Map.empty
  where
    numItems = length (head inputMatrix)

    initialUnassigned = [0 .. length inputMatrix - 1]
    initialPrices = Map.fromList [(j, 0) | j <- [0 .. numItems - 1]]

    go :: [Bidder] -> Prices -> Assignment -> Assignment
    go [] _ assignment = assignment
    go (i : unassignedBidders) prices assignment =
      let
        -- Calculate net payoffs for all items
        netPayoffs = [(j, netPayoff i j prices) | j <- [0 .. numItems - 1]]

        -- Find the best and second-best items
        (bestItem, maxPayoff) = maximumBy (comparing snd) netPayoffs

        -- Find second-best item (or use a lower value if only one item available)
        secondMaxPayoff = if length netPayoffs > 1
                          then maximum $ map snd (filter ((/= bestItem) . fst) netPayoffs)
                          else maxPayoff - epsilon

        -- Update price according to the auction algorithm description
        newPrice = (prices Map.! bestItem) + (maxPayoff - secondMaxPayoff + epsilon)
        updatedPrices = Map.insert bestItem newPrice prices

        -- Handle previous assignment of the item
        (newAssignment, remainingUnassigned) =
          case Map.lookup bestItem assignment of
            -- If the item was previously assigned, reassign it
            Just prevBidder ->
                let updatedAssignment = Map.insert i bestItem (Map.delete prevBidder assignment)
                    updatedUnassigned = if prevBidder /= i
                                        then prevBidder : unassignedBidders
                                        else unassignedBidders
                in (updatedAssignment, updatedUnassigned)
            -- Otherwise, simply assign the item
            Nothing ->
                (Map.insert i bestItem assignment, unassignedBidders)

      in go remainingUnassigned updatedPrices newAssignment

    -- Calculate net payoff for a bidder for a specific item
    netPayoff :: Bidder -> Item -> Prices -> Double
    netPayoff i j prices = inputMatrix !! i !! j - (prices Map.! j)

-- Find the optimal assignment by brute force
optimalAssignment :: PayoffMatrix -> Assignment
optimalAssignment matrix = maximumBy (comparing totalPayoff) assignments
  where
    bidders = [0 .. length matrix - 1]
    assignments = map (Map.fromList . zip bidders) (permutations bidders)
    totalPayoff assignment = sum [matrix !! bidder !! item | (bidder, item) <- Map.toList assignment]

-- main :: IO ()
-- main = do
--     let matrix = [[10.0, 5.0, 8.0],
--                   [7.0, 9.0, 5.0],
--                   [20.0, 7.0, 10.0]]
--         epsilon :: Double
--         epsilon = 0.1  -- Small positive value to break potential cycles
--         assignment_auction_algo = auctionAlgorithm epsilon matrix
--         assignment_optimal = optimalAssignment matrix

--     putStrLn "Auction Algorithm Results:"
--     printAuctionResults matrix assignment_auction_algo
--     putStrLn "\nOptimal Assignment Results:"
--     printAuctionResults matrix assignment_optimal
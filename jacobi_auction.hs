module JacobiAuction (jacobiAuctionAlgorithm) where

import Control.Parallel.Strategies
import Control.Concurrent.Async (async, wait)
import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad (forM_, replicateM_, unless, forever, when)
import Data.List (maximumBy, permutations, foldl')
import Data.Ord (comparing)
import qualified Data.Map as Map

type Bidder = Int
type Item = Int
type Prices = Map.Map Item Double
type Assignment = Map.Map Item Bidder -- changed mapping from item to bidder
type PayoffMatrix = [[Double]]


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

jacobiAuctionAlgorithm :: Double -> PayoffMatrix -> Double
jacobiAuctionAlgorithm epsilon inputMatrix = totalPayoff
  where
    numItems = length (head inputMatrix)
    initialUnassigned = [0 .. length inputMatrix - 1]
    initialPrices = Map.fromList [(j, 0) | j <- [0 .. numItems - 1]]

    -- Encapsulates the entire auction logic, with synchronization
    runSynchronizedAuction :: [Bidder] -> Prices -> Assignment -> Assignment
    runSynchronizedAuction [] _ assignment = assignment
    runSynchronizedAuction unassignedBidders prices assignment =
      let
        bidResults = synchronizedParallelBidding unassignedBidders prices
        updatedPrices = foldl' updatePrices prices bidResults
        (newAssignment, newUnassigned) = resolveConflicts bidResults assignment
      in
        if null newUnassigned
        then newAssignment
        else runSynchronizedAuction newUnassigned updatedPrices newAssignment

    -- Synchronized parallel bidding
    synchronizedParallelBidding :: [Bidder] -> Prices -> [(Bidder, Item, Double)]
    synchronizedParallelBidding bidders prices =
      map (bestBid prices) bidders `using` parList rdeepseq
    -- Find the best item and second-best payoff for a bidder

    bestBid :: Prices -> Bidder -> (Bidder, Item, Double)
    bestBid prices i =
      let
        netPayoffs = [(j, netPayoff i j prices) | j <- [0 .. numItems - 1]]
        (bestItem, maxPayoff) = maximumBy (comparing snd) netPayoffs
        secondMaxPayoff = if length netPayoffs > 1
                          then maximum $ map snd (filter ((/= bestItem) . fst) netPayoffs)
                          else maxPayoff - epsilon
        bidPrice = (prices Map.! bestItem) + (maxPayoff - secondMaxPayoff + epsilon)
      in (i, bestItem, bidPrice)

    -- Resolve conflicts: only one bidder can win an item
    resolveConflicts :: [(Bidder, Item, Double)] -> Assignment -> (Assignment, [Bidder])
    resolveConflicts bids assignment =
      let
        -- Group bids by item
        groupedBids = Map.fromListWith (++) [(item, [(bidder, bidPrice)]) | (bidder, item, bidPrice) <- bids]

        -- Resolve conflicts: choose the highest bidder for each item
        resolvedAssignments =
          Map.mapWithKey (\_ bidders -> fst $ maximumBy (comparing snd) bidders) groupedBids

        -- Update assignment and collect unassigned bidders
        newAssignment =
          foldl' (\acc (item, bidder) -> Map.insert bidder item acc) assignment (Map.toList resolvedAssignments)

        unassignedBidders =
          [bidder | (_, bidders) <- Map.toList groupedBids, (bidder, _) <- bidders, not (Map.member bidder newAssignment)]
      in (newAssignment, unassignedBidders)

    -- Update prices for items based on the winning bids
    updatePrices :: Prices -> (Bidder, Item, Double) -> Prices
    updatePrices prices (_, item, bidPrice) =
      let currentPrice = Map.findWithDefault 0 item prices
      in Map.insert item (max currentPrice bidPrice) prices

    -- Calculate net payoff for a bidder for a specific item
    netPayoff :: Bidder -> Item -> Prices -> Double
    netPayoff i j prices = inputMatrix !! i !! j - (prices Map.! j)

    assignment = runSynchronizedAuction initialUnassigned initialPrices Map.empty
    totalPayoff = sum [inputMatrix !! bidder !! item | (bidder, item) <- Map.toList assignment]

-- main :: IO ()
-- main = do
--     let matrix = [[10.0, 5.0, 8.0],
--                   [7.0, 9.0, 5.0],
--                   [20.0, 7.0, 10.0]]
--         epsilon :: Double
--         epsilon = 0.1  -- Small positive value to break potential cycles
--         assignment_jacobi = jacobiAuctionAlgorithm epsilon matrix

--     putStrLn "Jacobi Auction Algorithm Results:"
--     printAuctionResults matrix assignment_jacobi

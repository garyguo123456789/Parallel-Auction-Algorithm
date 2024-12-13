module JacobiAuction (jacobiAuctionAlgorithm) where

import Control.Parallel.Strategies ( parList, rdeepseq, using )
import Data.List (maximumBy, foldl')
import Data.Ord (comparing)
import qualified Data.Map as Map

type Bidder = Int
type Item = Int
type Prices = Map.Map Item Double
type Assignment = Map.Map Item Bidder -- Mapping from item to bidder
type PayoffMatrix = [[Double]]

jacobiAuctionAlgorithm :: Double -> PayoffMatrix -> (Assignment, Double)
jacobiAuctionAlgorithm epsilon inputMatrix = (finalAssignment, totalPayoff)
  where
    numItems = length (head inputMatrix)
    initialUnassigned = [0 .. length inputMatrix - 1]
    initialPrices = Map.fromList [(j, 0) | j <- [0 .. numItems - 1]]

    -- get the resulting assignment and also the total payoff, to return
    (finalAssignment, _) = runSynchronizedAuction initialUnassigned initialPrices Map.empty
    totalPayoff = sum [inputMatrix !! bidder !! item | (item, bidder) <- Map.toList finalAssignment]

    -- Auction process
    runSynchronizedAuction :: [Bidder] -> Prices -> Assignment -> (Assignment, [Bidder])
    runSynchronizedAuction [] _ assignment = (assignment, [])
    runSynchronizedAuction unassignedBidders prices assignment =
      let
        bidResults = synchronizedParallelBidding unassignedBidders prices
        updatedPrices = foldl' updatePrices prices bidResults
        (newAssignment, newUnassigned) = resolveConflicts bidResults assignment
      in
        if null newUnassigned
        then (newAssignment, newUnassigned)
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
        groupedBids = Map.fromListWith (++) [(item, [(bidder, bidPrice)]) | (bidder, item, bidPrice) <- bids]
        resolvedAssignments =
          Map.mapWithKey (\_ bidders -> fst $ maximumBy (comparing snd) bidders) groupedBids
        newAssignment =
          foldl' (\acc (item, bidder) -> Map.insert item bidder acc) assignment (Map.toList resolvedAssignments)
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

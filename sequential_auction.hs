module SequentialAuction (auctionAlgorithm, optimalAssignment) where

import Data.List (maximumBy, permutations)
import Data.Ord (comparing)
import qualified Data.Map as Map

type Bidder = Int
type Item = Int
type Prices = Map.Map Item Double

-- item is the key, bidder is the value, for consistency with the algorithm from the paper
type Assignment = Map.Map Item Bidder 
type PayoffMatrix = [[Double]]


auctionAlgorithm :: Double -> PayoffMatrix -> (Assignment, Double)
auctionAlgorithm epsilon inputMatrix = (finalAssignment, totalPayoff)
  where
    numItems = length (head inputMatrix)
    numBidders = length inputMatrix

    initialUnassigned = [0 .. numBidders - 1]
    initialPrices = Map.fromList [(j, 0) | j <- [0 .. numItems - 1]]

    finalAssignment = go initialUnassigned initialPrices Map.empty
    totalPayoff = sum [inputMatrix !! bidder !! item | (item, bidder) <- Map.toList finalAssignment]

    go :: [Bidder] -> Prices -> Assignment -> Assignment
    go [] _ assignment = assignment
    go (i : unassignedBidders) prices assignment =
      let
        -- Calculate net payoffs for all items
        netPayoffs = [(j, netPayoff i j prices) | j <- [0 .. numItems - 1]]

        -- Find the best and second-best items
        (bestItem, maxPayoff) = maximumBy (comparing snd) netPayoffs
        secondMaxPayoff = if length netPayoffs > 1
                          then maximum [ p | (j,p) <- netPayoffs, j /= bestItem ]
                          else maxPayoff - epsilon

        -- Update the price of the best item
        newPrice = (prices Map.! bestItem) + (maxPayoff - secondMaxPayoff + epsilon)
        updatedPrices = Map.insert bestItem newPrice prices

        -- Handle previous assignment of the item
        (newAssignment, remainingUnassigned) =
          case Map.lookup bestItem assignment of
            Just prevBidder ->
              -- since bestItem was assigned to prevBidder, remove that assignment and add prevBidder back into U
              let updatedAssignment = Map.insert bestItem i assignment -- reassign item to current bidder i
                  updatedUnassigned = prevBidder : unassignedBidders
              in (updatedAssignment, updatedUnassigned)
            Nothing ->
              (Map.insert bestItem i assignment, unassignedBidders)
      in go remainingUnassigned updatedPrices newAssignment

    netPayoff :: Bidder -> Item -> Prices -> Double
    netPayoff i j prices = inputMatrix !! i !! j - (prices Map.! j)

-- Find the optimal assignment by brute force (adjusted to return item->bidder)
optimalAssignment :: PayoffMatrix -> Assignment
optimalAssignment matrix = maximumBy (comparing totalPayoff) assignments
  where
    bidders = [0 .. length matrix - 1]
    items = bidders -- assume square matrix
    assignments = [Map.fromList (zip items perm) | perm <- permutations bidders]
    totalPayoff assignment = sum [matrix !! b !! i | (i,b) <- Map.toList assignment]

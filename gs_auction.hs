module GSAuction (gsAuctionAlgorithm) where

import Control.Parallel.Strategies
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.Ord (comparing, Down(..))

type PayoffMatrix = [[Double]]
type Bidder = Int
type Item = Int
type Prices = Map.Map Item Double
type Assignment = Map.Map Bidder Item

gsAuctionAlgorithm :: Double -> PayoffMatrix -> (Assignment, Double)
gsAuctionAlgorithm epsilon inputMatrix = (finalAssignment, totalPayoff)
  where
    numItems = length (head inputMatrix)
    initialUnassigned = [0 .. length inputMatrix - 1]
    initialPrices = Map.fromList [(j, 0) | j <- [0 .. numItems - 1]]

    -- get the resulting assignment and also the total payoff, to return
    finalAssignment = go initialUnassigned initialPrices Map.empty
    totalPayoff = sum [inputMatrix !! bidder !! item | (item, bidder) <- Map.toList finalAssignment]

    go :: [Bidder] -> Prices -> Assignment -> Assignment
    go [] _ assignment = assignment
    go (i : unassignedBidders) prices assignment =
      let
        -- calculate net payoffs for all items
        netPayoffs = [(j, netPayoff i j prices) | j <- [0 .. numItems - 1]]

        -- parallelize the search for best and second-best items
        partitions = chunkItems 6400 netPayoffs -- change this number iteratively to find the best size chunk
        -- for chunks: tested 2, 4, 8, 20, 100, 400, 1600, 6400
        partialResults = parMap rpar findBestAndSecond partitions
        (bestItem, maxPayoff, secondMaxPayoff) = mergeResults partialResults epsilon

        -- update price according to the auction algorithm description
        newPrice = (prices Map.! bestItem) + (maxPayoff - secondMaxPayoff + epsilon)
        updatedPrices = Map.insert bestItem newPrice prices

        -- handle previous assignment of the item
        (newAssignment, remainingUnassigned) =
          case Map.lookup bestItem assignment of
            Just prevBidder ->
              let updatedAssignment = Map.insert bestItem i assignment
                  updatedUnassigned = prevBidder : unassignedBidders
              in (updatedAssignment, updatedUnassigned)
            Nothing ->
              (Map.insert bestItem i assignment, unassignedBidders)
      in go remainingUnassigned updatedPrices newAssignment

    -- calculate net payoff for a bidder for a specific item
    netPayoff :: Bidder -> Item -> Prices -> Double
    netPayoff i j prices = inputMatrix !! i !! j - (prices Map.! j)

    -- find the best and second-best items in a partition
    findBestAndSecond :: [(Item, Double)] -> (Item, Double, Maybe Double)
    findBestAndSecond payoffs =
      let (bestItem, maxPayoff) = maximumBy (comparing snd) payoffs
          secondMaxPayoff = if length payoffs > 1
                            then Just $ maximum $ map snd (filter ((/= bestItem) . fst) payoffs)
                            else Nothing
      in (bestItem, maxPayoff, secondMaxPayoff)

    -- merge results from all partitions
    mergeResults :: [(Item, Double, Maybe Double)] -> Double -> (Item, Double, Double)
    mergeResults results epsilon =
      let
        allPayoffsWithItems = concatMap (\(item, p, ms) -> [(item, p), (item, fromMaybe (-1 / 0) ms)]) results
        sortedPayoffsWithItems = sortBy (comparing (Down . snd)) allPayoffsWithItems
        (bestItem, maxPayoff) = head sortedPayoffsWithItems
        secondMaxPayoff = if length sortedPayoffsWithItems > 1
                          then snd (sortedPayoffsWithItems !! 1)
                          else maxPayoff - epsilon
      in (bestItem, maxPayoff, secondMaxPayoff)

    -- split items into equal-sized chunks for parallel processing
    chunkItems :: Int -> [a] -> [[a]]
    chunkItems n items = let (q, r) = length items `quotRem` n
                         in goChunks q r items
      where
        goChunks _ 0 [] = []
        goChunks q r xs = let (chunk, rest) = splitAt (q + if r > 0 then 1 else 0) xs
                          in chunk : goChunks q (max 0 (r - 1)) rest

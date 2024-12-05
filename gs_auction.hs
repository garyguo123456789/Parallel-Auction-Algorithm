module Main (main) where

import Control.Parallel.Strategies
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.Ord (comparing, Down(..))
import Debug.Trace

type PayoffMatrix = [[Double]]
type Bidder = Int
type Item = Int
type Prices = Map.Map Item Double
type Assignment = Map.Map Bidder Item

printAuctionResults :: PayoffMatrix -> Assignment -> IO ()
printAuctionResults matrix assignment = do
    putStrLn "Assignment (Bidder -> Item):"
    mapM_ (\(bidder, item) -> putStrLn $ "Bidder " ++ show bidder ++ " -> Item " ++ show item)
          (Map.toList assignment)

    putStrLn "\nPayoff Matrix:"
    mapM_ print matrix

    putStrLn "\nTotal Payoff Breakdown:"
    let payoffBreakdown = [(bidder, item, matrix !! bidder !! item)
                           | (bidder, item) <- Map.toList assignment]
    mapM_ (\(b, i, p) -> putStrLn $ "Bidder " ++ show b ++ " -> Item " ++ show i ++ ": " ++ show p)
          payoffBreakdown

    let totalPayoff = sum [matrix !! bidder !! item | (bidder, item) <- Map.toList assignment]
    putStrLn $ "\nTotal Payoff: " ++ show totalPayoff

gsAuctionAlgorithm :: Double -> PayoffMatrix -> Assignment
gsAuctionAlgorithm epsilon inputMatrix = go initialUnassigned initialPrices Map.empty
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

        -- Parallelize the search for best and second-best items
        partitions = chunkItems 4 netPayoffs
        partialResults = parMap rpar findBestAndSecond partitions
        (bestItem, maxPayoff, secondMaxPayoff) = mergeResults partialResults epsilon

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

    -- Find the best and second-best items in a partition
    findBestAndSecond :: [(Item, Double)] -> (Item, Double, Maybe Double)
    findBestAndSecond payoffs =
      let (bestItem, maxPayoff) = maximumBy (comparing snd) payoffs
          secondMaxPayoff = if length payoffs > 1
                            then Just $ maximum $ map snd (filter ((/= bestItem) . fst) payoffs)
                            else Nothing
      in (bestItem, maxPayoff, secondMaxPayoff)

    -- Merge results from all partitions
    mergeResults :: [(Item, Double, Maybe Double)] -> Double -> (Item, Double, Double)
    mergeResults results epsilon =
      let
        -- Extract all payoffs with corresponding items
        allPayoffsWithItems = concatMap (\(item, p, ms) -> [(item, p), (item, fromMaybe (- (1 / 0)) ms)]) results
        -- Sort payoffs by value in descending order, keeping track of items
        sortedPayoffsWithItems = sortBy (comparing (Data.Ord.Down . snd)) allPayoffsWithItems

        -- Extract max and second max
        (bestItem, maxPayoff) = head sortedPayoffsWithItems
        secondMaxPayoff = if length sortedPayoffsWithItems > 1
                          then snd (sortedPayoffsWithItems !! 1)
                          else maxPayoff - epsilon
      in (bestItem, maxPayoff, secondMaxPayoff)

    -- Split items into equal-sized chunks for parallel processing
    chunkItems :: Int -> [a] -> [[a]]
    chunkItems n items = let (q, r) = length items `quotRem` n
                         in goChunks q r items
      where
        goChunks _ 0 [] = []
        goChunks q r xs = let (chunk, rest) = splitAt (q + if r > 0 then 1 else 0) xs
                          in chunk : goChunks q (max 0 (r - 1)) rest

main :: IO ()
main = do
    let matrix = [[10.0, 5.0, 8.0],
                  [7.0, 9.0, 5.0],
                  [20.0, 7.0, 10.0]]
        epsilon :: Double
        epsilon = 0.1  -- Small positive value to break potential cycles
        assignment_gs = gsAuctionAlgorithm epsilon matrix

    putStrLn "Gauss-Seidel Auction Algorithm Results:"
    printAuctionResults matrix assignment_gs

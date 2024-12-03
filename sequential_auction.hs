module Main (main) where

import Data.List (maximumBy, delete, permutations)
import Data.Ord (comparing)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

type Bidder = Int
type Item = Int
type PayoffMatrix = [[Double]]
type Prices = Map.Map Item Double
type Assignment = Map.Map Bidder Item

auctionAlgorithm :: Double -> PayoffMatrix -> Assignment
auctionAlgorithm epsilon matrix = go (initialUnassigned matrix) initialPrices Map.empty
  where
    numBidders = length matrix
    numItems = length (head matrix)
    
    initialUnassigned matrix = [0 .. length matrix - 1]
    initialPrices = Map.fromList [(j, 0) | j <- [0 .. numItems - 1]]
    
    go :: [Bidder] -> Prices -> Assignment -> Assignment
    go [] _ assignment = assignment
    go (i:unassignedBidders) prices assignment =
      let 
        -- Calculate net payoffs for all items
        netPayoffs = [(j, netPayoff i j prices) | j <- [0 .. numItems - 1], j `notElem` Map.elems assignment]
        
        -- Find the best and second-best items
        (bestItem, maxPayoff) = maximumBy (comparing snd) netPayoffs
        
        -- Find second-best item (or use a lower value if only one item available)
        secondMaxPayoff = if length netPayoffs > 1
                          then maximum $ map snd (delete (bestItem, maxPayoff) netPayoffs)
                          else maxPayoff - epsilon
        
        -- Update price according to the auction algorithm description
        newPrice = (prices Map.! bestItem) + (maxPayoff - secondMaxPayoff + epsilon)
        updatedPrices = Map.insert bestItem newPrice prices
        
        -- Handle previous assignment of the item
        (newAssignment, remainingUnassigned) = 
          case Map.lookup bestItem assignment of
            -- If item was previously assigned, add previous bidder back to unassigned
            Just prevBidder -> 
              (Map.delete prevBidder $ Map.insert i bestItem assignment, 
               prevBidder : delete i unassignedBidders)
            -- Otherwise, simply assign the item
            Nothing -> 
              (Map.insert i bestItem assignment, delete i unassignedBidders)
      
      in go remainingUnassigned updatedPrices newAssignment
    
    -- Calculate net payoff for a bidder for a specific item
    netPayoff :: Bidder -> Item -> Prices -> Double
    netPayoff i j prices = matrix !! i !! j - (prices Map.! j)

-- Find the optimal assignment by maximizing the total payoff
optimalAssignment :: PayoffMatrix -> Assignment
optimalAssignment matrix = maximumBy (comparing totalPayoff) assignments
  where
    bidders = [0 .. length matrix - 1]
    assignments = map (\perm -> Map.fromList (zip bidders perm)) (permutations bidders)
    totalPayoff assignment = sum [matrix !! bidder !! item | (bidder, item) <- Map.toList assignment]

main :: IO ()
main = do
    let matrix = [[10.0, 5.0, 8.0],
                  [7.0, 9.0, 5.0],
                  [20.0, 7.0, 10.0]]
        epsilon = 0.1  -- Small positive value to break potential cycles
        assignment = optimalAssignment matrix
    
    putStrLn "Optimal Assignment (Bidder -> Item):"
    mapM_ (\(bidder, item) -> putStrLn $ "Bidder " ++ show bidder ++ " -> Item " ++ show item) 
          (Map.toList assignment)
    
    putStrLn "\nPayoff Matrix:"
    mapM_ (putStrLn . show) matrix
    
    putStrLn "\nTotal Payoff Breakdown:"
    let payoffBreakdown = [(bidder, item, matrix !! bidder !! item) 
                           | (bidder, item) <- Map.toList assignment]
    mapM_ (\(b, i, p) -> putStrLn $ "Bidder " ++ show b ++ " -> Item " ++ show i ++ ": " ++ show p) 
          payoffBreakdown
    
    let totalPayoff = sum [matrix !! bidder !! item | (bidder, item) <- Map.toList assignment]
    putStrLn $ "\nTotal Payoff: " ++ show totalPayoff
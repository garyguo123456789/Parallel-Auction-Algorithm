module Main (main) where

import SequentialAuction (auctionAlgorithm, optimalAssignment)
import JacobiAuction (jacobiAuctionAlgorithm)
import GSAuction (gsAuctionAlgorithm)
import qualified Data.Map as Map
import Control.Monad (unless)

type Bidder = Int
type Item = Int
type PayoffMatrix = [[Double]]
type Prices = Map.Map Item Double
type Assignment = Map.Map Bidder Item


printAuctionResults :: PayoffMatrix -> Assignment -> IO ()
printAuctionResults matrix assignment = do
    putStrLn "Total Payoff Breakdown:"
    -- changed to iterate over items instead of bidders
    let payoffBreakdown = [(bidder, item, matrix !! bidder !! item)
                           | (item, bidder) <- Map.toList assignment]
    mapM_ (\(b, i, p) -> putStrLn $ "Bidder " ++ show b ++ " -> Item " ++ show i ++ ": " ++ show p)
          payoffBreakdown

    let totalPayoff = sum [matrix !! bidder !! item | (item, bidder) <- Map.toList assignment]
    putStrLn $ "\nTotal Payoff: " ++ show totalPayoff


runTest :: (Double -> PayoffMatrix -> Assignment) -> PayoffMatrix -> Double -> IO ()
runTest algorithm matrix epsilon = do
    let assignment = algorithm epsilon matrix
        optimal = optimalAssignment matrix
        -- testResult = assignment == optimal

        -- compare total payoffs instead
        totalPayoffAlgo = sum [matrix !! bidder !! item | (item, bidder) <- Map.toList assignment]
        totalPayoffOptimal = sum [matrix !! bidder !! item | (item, bidder) <- Map.toList optimal]
        testResult = totalPayoffAlgo == totalPayoffOptimal

    unless testResult $ do -- unless recommended by vscode for more succinct
        putStrLn "--------------------------------- \nTest failed, printing results..."
        putStrLn "\nPayoff Matrix:"
        mapM_ print matrix
        putStrLn "\nActual..."
        printAuctionResults matrix assignment
        putStrLn "\nExpected..."
        printAuctionResults matrix optimal


main :: IO ()
main = do
    let matrices = [ ([[10.0, 5.0, 8.0],
                       [7.0, 9.0, 5.0],
                       [20.0, 7.0, 10.0]], 0.01),

                     ([[10.0, 15.0, 20.0, 5.0],
                       [5.0, 10.0, 15.0, 20.0],
                       [20.0, 5.0, 10.0, 15.0],
                       [15.0, 20.0, 5.0, 10.0]], 0.01),

                    -- not sure how we would deal with this case anyways
                    --  ([[0.0, 0.0, 0.0],
                    --    [0.0, 0.0, 0.0],
                    --    [0.0, 0.0, 0.0]], 0.01),

                     ([[1.0, 2.0, 3.0, 4.0],
                       [4.0, 3.0, 2.0, 1.0],
                       [0.0, 1.0, 0.0, 1.0],
                       [1.0, 0.0, 1.0, 0.0]], 0.01),

                     ([[10.0, 5.0, 2.0, 1.0],
                       [3.0, 8.0, 7.0, 5.0],
                       [9.0, 6.0, 3.0, 2.0],
                       [8.0, 7.0, 4.0, 1.0]], 0.01),

                     ([[5.0, 6.0, 7.0, 8.0, 9.0],
                       [9.0, 8.0, 7.0, 6.0, 5.0],
                       [4.0, 3.0, 2.0, 1.0, 0.0],
                       [0.0, 1.0, 2.0, 3.0, 4.0],
                       [5.0, 5.0, 5.0, 5.0, 5.0]], 0.01),

                     ([[20.0, 15.0, 10.0],
                       [10.0, 20.0, 15.0],
                       [15.0, 10.0, 20.0]], 0.01),

                     ([[3.0, 2.0],
                       [2.0, 3.0]], 0.01),

                     ([[10.0, 12.0, 14.0, 16.0],
                       [16.0, 14.0, 12.0, 10.0],
                       [11.0, 13.0, 15.0, 17.0],
                       [17.0, 15.0, 13.0, 11.0]], 0.01),

                     ([[0.0, 1.0, 2.0, 3.0, 4.0, 5.0],
                       [5.0, 4.0, 3.0, 2.0, 1.0, 0.0],
                       [1.0, 2.0, 3.0, 4.0, 5.0, 6.0],
                       [6.0, 5.0, 4.0, 3.0, 2.0, 1.0],
                       [2.0, 3.0, 4.0, 5.0, 6.0, 7.0],
                       [7.0, 6.0, 5.0, 4.0, 3.0, 2.0]], 0.01)
                   ]

    putStrLn "\n------- sequential tests -------"
    mapM_ (uncurry (runTest auctionAlgorithm)) matrices -- vscode recommended "uncurry"

    -- putStrLn "\n------- jacobi tests -------"
    -- mapM_ (uncurry (runTest auctionAlgorithm)) matrices

    -- putStrLn "\n------- GS tests -------"
    -- mapM_ (uncurry (runTest auctionAlgorithm)) matrices

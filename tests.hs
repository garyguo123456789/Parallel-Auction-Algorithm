module Main (main) where

import SequentialAuction (auctionAlgorithm)
import JacobiAuction (jacobiAuctionAlgorithm)
import GSAuction (gsAuctionAlgorithm)
import qualified Data.Map as Map
import Control.Monad (unless)
import System.Random (mkStdGen, randomRs, StdGen, split)
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import System.IO.Error (catchIOError)

type Bidder = Int
type Item = Int
type PayoffMatrix = [[Double]]
type Assignment = Map.Map Item Bidder

roundToTenths :: Double -> Double
roundToTenths x = fromIntegral (round (x * 10)) / 10

printMatrix :: PayoffMatrix -> IO ()
printMatrix m = do
    putStrLn "Price matrix:"
    mapM_ (putStrLn . formatRow . map roundToTenths) m
  where
    formatRow :: [Double] -> String
    formatRow row = "[" ++ unwords (map show row) ++ "]"


printAuctionResults :: PayoffMatrix -> Assignment -> Double -> IO ()
printAuctionResults matrix assignment totalPayoff = do
    putStrLn "\nAssignments and payoffs:"
    let payoffBreakdown = [(item, bidder, matrix !! bidder !! item) | (item, bidder) <- Map.toList assignment] -- !! is same as matrix[bidder][item]
    mapM_ (\(i, b, p) -> putStrLn $ "Item " ++ show i ++ " -> Bidder " ++ show b ++ ": " ++ show (roundToTenths p)) payoffBreakdown
    putStrLn $ "\nTotal payoff: " ++ show (roundToTenths totalPayoff)

runAlgorithm :: (Double -> PayoffMatrix -> (Assignment, Double)) -> PayoffMatrix -> IO (Assignment, Double)
runAlgorithm algorithm matrix = do
    let (assignment, totalPayoff) = algorithm 0.01 matrix -- always assume 0.01 is sufficient for epsilon
    return (assignment, totalPayoff)

main :: IO ()
main = runProgram `catchIOError` \_ ->
    die "ERROR, try making sure the command-line arguments are formatted correctly"

runProgram :: IO ()
runProgram = do
    args <- getArgs
    case args of
        [sizeStr, algStr] -> do
            let maybeSize = reads sizeStr :: [(Int, String)] -- read the input, and cast as Int, String tuple
            case maybeSize of
                [(n, "")] -> do
                    algFunc <- case algStr of
                                    "seq"    -> return auctionAlgorithm
                                    "gs"     -> return gsAuctionAlgorithm
                                    "jacobi" -> return jacobiAuctionAlgorithm
                                    _        -> die "ERROR, please enter 'seq', 'gs', or 'jacobi'"
                    let seed = 100 -- causes generated matrix to stay the same if file isn't reloaded
                        gen = mkStdGen seed
                        (matrix, _) = generateMatrix gen n n
                    (assignment, totalPayoff) <- runAlgorithm algFunc matrix

                    -- show the results when the matrix is resonably small
                    -- assume that this case is used for testing correctness by hand
                    if n < 6 
                        then do
                            printMatrix matrix
                            printAuctionResults matrix assignment totalPayoff

                        -- assume that this case is used for testing runtime on large matrices   
                        else do
                            putStrLn $ "Total payoff: " ++ show (roundToTenths totalPayoff)
                _ -> do
                    pn <- getProgName
                    die $ "ERROR, Invalid command line arguments. Usage: " ++ pn
        _ -> do
            pn <- getProgName
            die $ "ERROR, Usage: " ++ pn


generateMatrix :: StdGen -> Int -> Int -> (PayoffMatrix, StdGen)
generateMatrix gen rows cols = (matrix, finalGen)
    where randomNumbers = take (rows * cols) $ randomRs (0.0, 100.0) gen
          (finalGen, _) = split gen
          matrix = chunksOf cols randomNumbers

-- splits the randomNumbers list into chunks of size cols
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (ys, zs) = splitAt n xs in ys : chunksOf n zs



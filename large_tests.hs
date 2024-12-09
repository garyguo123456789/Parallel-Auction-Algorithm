import System.Random (mkStdGen, randomRs)

-- Generate a pseudo-random matrix of given dimensions and seed
generateMatrix :: Int -> Int -> Int -> [[Double]]
generateMatrix rows cols seed =
  let gen = mkStdGen seed
      randomValues = randomRs (0.0, 10.0) gen -- Random numbers in the range [0.0, 10.0]
  in take rows $ chunksOf cols randomValues

-- Helper function to split a list into chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Example: Generate matrices
matrix1000x1000 :: [[Double]]
matrix1000x1000 = generateMatrix 1000 1000 42 -- Seed 42

matrix10000x10000 :: [[Double]]
matrix10000x10000 = generateMatrix 10000 10000 42 -- Seed 42

matrix50000x50000 :: [[Double]]
matrix50000x50000 = generateMatrix 50000 50000 42 -- Seed 42
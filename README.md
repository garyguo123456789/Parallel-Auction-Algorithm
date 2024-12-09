# Disclaimer

This repo is created for the purpose as the final project of the Parallel Function Programming class. 

# Author 

Ava Penn, Yuanqing Lei, Haolin Guo

# Supervisor

Prof. Stephen Edwards

# Code Breakdown

1. LICENSE

Contains the licensing terms for this project.

2. README.md

This documentation provides an overview of the repository, its structure, and instructions for running the code.

3. sequential_auction.hs

This file implements the sequential version of the auction algorithm:
	•	Description: The algorithm assigns agents to tasks iteratively, finding the optimal assignment by updating bids and prices sequentially.
	•	Use Case: Serves as the baseline implementation to validate correctness and compare performance against parallel versions.

4. gs_auction.hs

This file implements the Gauss-Seidel parallel version:
	•	Description: The bidding phase is parallelized by dividing the tasks into partitions, with each thread searching within its partition. After the searches, the results are merged to find the overall best and second-best bids.
	•	Challenges: Synchronization during merging introduces overhead, especially for large task sets.
	•	Strengths: Simplifies parallelization and is effective for smaller parallel setups.

5. jacobi_auction.hs

This file implements the Jacobi parallel version:
	•	Description: The algorithm parallelizes the bidding phase by processing multiple agents concurrently. Synchronization is performed at the end of each iteration to resolve conflicts if multiple agents bid for the same task.
	•	Asynchronous Variant: Includes an optional mode where price updates are performed without synchronization, relying on the theoretical correctness of monotonic price updates.
	•	Strengths: Scales well for larger datasets and utilizes multiple CPU cores efficiently.

6. tests.hs

Contains unit tests and benchmarks for all implementations:
	•	Correctness Tests: Ensures that the outputs of all versions match and conform to the assignment problem’s optimal solution.
	•	Performance Tests: Benchmarks the execution time for varying input sizes to compare sequential and parallel implementations.

# How to run the code

1. Setup
Ensure that GHC (Glasgow Haskell Compiler) is installed on your system.
2.	Compile
Use ghc to compile individual files. For example:

ghc -o sequential sequential_auction.hs
ghc -o gs gs_auction.hs
ghc -o jacobi jacobi_auction.hs

3. Run the computed binaries
./sequential
./gs
./jacobi

4. Run test
ghc -o tests tests.hs
./tests


# Results

## Correctness

All versions of the algorithm produce the same optimal assignment, verified using small hand-annotated test cases.

## Performance

TBD

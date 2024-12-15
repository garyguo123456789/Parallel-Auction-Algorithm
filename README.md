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

This file implements the sequential version of the auction algorithm,

4. gs_auction.hs

This file implements the Gauss-Seidel parallel version,

5. jacobi_auction.hs

This file implements the Jacobi parallel version,

6. tests.hs

Contains unit tests and benchmarks for all implementations. THe tests.hs file imports the different implementations so that the user can test different implementations and compare run results.  

# How to run the code

## 1. Setup
Ensure that GHC (Glasgow Haskell Compiler) is installed on your system.

## 2. Compile
Use ghc to compile individual files. The format is

```
ghc -threaded -rtsopts -O2 -o auction_test tests.hs sequential_auction.hs gs_auction.hs jacobi_auction.hs -package containers -package stm -package async -package random
```

```
./auction_test [matrix size as integer] [gs, jacobi, or seq] +RTS -N[number of cores] -l
```
For example: 
```
./auction_test 1000 gs +RTS -N8 -l
```


## 3. Threadscope Analysis

To see Threadscope for the compilation, run
```
threadscope auction_test.eventlog
```

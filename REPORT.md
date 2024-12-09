# Parallel-Auction-Algorithm

# Introduction

In this project, we explore the implementation of both sequential and parallel versions of the auction algorithm in Haskell. The auction algorithm is a well-known optimization technique commonly used for solving assignment problems, where the goal is to match agents to tasks in a way that minimizes or maximizes a given cost function.

This algorithm is inspired by economic principles where agents bid for tasks, leading to iterative improvements in the solution until optimality is achieved. It is particularly appealing due to its decentralized nature and potential for parallelization, making it suitable for high-performance computing environments.

The project leverages Haskell’s functional programming paradigm to provide clean, concise, and robust implementations of the auction algorithm. The focus is on comparing the efficiency and performance of sequential and parallel versions of the algorithm, showcasing the strengths of functional programming for parallel computation.

# The Auction Algorithm

## Objective

The auction algorithm addresses the classic assignment problem, which involves assigning  n  agents to  n  tasks to optimize a given objective function. Common objectives include minimizing the total cost or maximizing the total reward of the assignment. The problem is represented as a cost matrix  C, where  C_{i,j}  indicates the cost of assigning agent  i  to task j.


## Implementation

The auction algorithm operates as follows:
	1.	Initialization: Each agent starts unassigned, and all task prices are initially set to zero.
	2.	Bidding Phase: Each unassigned agent selects its most preferred task (based on costs and current task prices) and places a bid. The bid reflects the agent’s willingness to pay for the task and ensures competitiveness by considering the second-best alternative.
	3.	Assignment Phase: Tasks are reassigned based on the received bids, and task prices are updated accordingly.
	4.	Convergence: The process repeats until all agents are assigned tasks, and the solution meets the optimality criteria.

## Parallelization

### 1. Gauss-Seidel Version

The Gauss-Seidel version focuses on parallelizing Step (2) of the auction algorithm, where each bidder searches for the best and second-best items to bid on. This parallelization divides the items among  p  threads, allowing each thread to search its partition independently.

Advantages:

The natural division of items into disjoint subsets simplifies implementation.
Each thread works independently within its partition, reducing contention during the search phase.

Drawbacks:

Synchronization Costs: After the individual searches, the results must be merged to determine the overall best and second-best items. This synchronization stage introduces significant overhead, as all threads must wait for the merging process to complete before proceeding to the next iteration.
Load Imbalance: If the item partitions are not evenly distributed or if the search complexity varies, some threads may finish earlier, leaving others idle.

Despite its simplicity, the synchronization cost often bottlenecks the Gauss-Seidel version’s performance, particularly when  p  is large.

### 2. The Jacobi version 

The Jacobi version takes a different approach, parallelizing the algorithm by allowing multiple bidders to search for their bids simultaneously. Each thread handles one or more bidders, aiming to reduce the number of iterations up to p-fold.

Advantages:

Reduced Iterations: Multiple bidders bidding simultaneously can drastically cut down the total number of iterations, as  p  or more bidders are handled in parallel.
Better Scalability: By focusing on bidders rather than items, the Jacobi version avoids the merging overhead present in the Gauss-Seidel version.

Challenges:

Conflicts: If multiple bidders bid for the same item in one iteration, only one can be assigned as the item’s tentative owner. Resolving these conflicts requires synchronization, introducing some overhead.
Outdated Prices: The price updates for items may be outdated when other threads access them. However, this issue can be mitigated using an asynchronous Jacobi variant, where outdated prices are allowed as long as new prices are higher than the previous values.

#### Asynchronous Jacobi Variant

Interestingly, the Jacobi version can operate without synchronization during the bidding phase by proving that updates remain valid as long as prices monotonically increase. This asynchronous approach eliminates bottlenecks, but it requires careful proof and implementation.

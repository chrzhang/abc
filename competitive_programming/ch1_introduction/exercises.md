### 1.1.1
    Imagine the houses are all along the same y dimension for simplicity. Consider
    4 houses, one at x=0, x=5, x=6, and x=10. Using the greedy strategy, the
    houses at x=5 and x=6 will be matched first, then the remaining houses. The
    total cost will be 1 + 10 = 11. An optimal match could be found by matching
    x=0 and x=5 followed by x=6 with x=10 for a total cost of 5 + 4 = 9.

### 1.1.2
    The naive complete search solution finds all ways of choosing a pairing and
    the search space for such a task is the binomial coefficient. A factorial
    complexity scales quickly out of hand.

### 1.1.3
    The pruning involved would apply after enumerating X solutions and marking
    the lowest total cost so far. We can stop exploring further solutions if the
    partial cost found so far has exceeded our lowest cost since it can only
    increase.

### 1.2.3.2
[Print pi to N (N <= 15) digits](pi_printer.h)

### 1.2.3.3
[Find day of week](day_of_week.h)

### 1.2.3.4
[Print distinct ordered integers](distinct.h)

### 1.2.3.5
[Order dates in MMDDYYYY order](order_dates.h)

### 1.2.3.6
[Binary search for 1M items](binary_search.h)

### 1.2.3.7
[Generate all permutations](permutations.h)

### 1.2.3.8
[Generate all subsets](subsets.h)

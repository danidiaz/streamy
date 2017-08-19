# streamy-sig

## Feature matrix

There are gaps in coverage in implementation of functions.  Below is a feature matrix saying which functions are supported by which libraries.

    Key | Module name
    ----|--------------------------
    S   | Streamy.Streaming
    P   | Streamy.Pipes
    C   | Streamy.Conduit

**Types**

    |                 | S | P | C |
    |-----------------|---|---|---|
    | Stream          | X | X | X |
    | Groups          |   |   |   |

**Basic functions**

    |                 | S | P | C |
    |-----------------|---|---|---|
    | yield           | X | X | X |
    | each            | X | X | X |
    | toList          | X | X | X |
    | toList_         | X | X | X |
    | chain           | X | X | X |
    | effects         | X | X | X |
    | concat          | X | X | X |
    | for             | X | X | X |
    | repeat          | X | X | X |
    | repeatM         | X | X | X |
    | take            | X | X | X |
    | takeWhile       | X | X | X |
    | map             | X | X | X |
    | mapM            | X | X | X |
    | mapM_           | X | X | X |
    | drop            | X | X | X |
    | dropWhile       | X | X | X |
    | filter          | X | X | X |
    | filterM         | X | X | X |
    | replicate       | X | X | X |
    | replicateM      | X | X | X |
    | all_            | X | X | X |
    | any_            | X | X | X |
    | fold            | X | X | X |
    | fold_           | X | X | X |
    | foldM           | X | X | X |
    | foldM_          | X | X | X |
    | scan            | X | X | X |
    | scanM           | X | X | X |

**Splitting and grouping functions**

    |                 | S | P | C |
    |-----------------|---|---|---|
    | group           |   |   |   |
    | groupBy         |   |   |   |



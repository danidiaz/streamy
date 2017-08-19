# streamy-sig

This package provides a *Streamy* signature with types and functions for
working with effectful streams. Implementations of subsets of this signature
can be found in *streamy-streaming*, *streamy-pipes* and *streamy-conduit*.

The interface tries to follow the nomenclature and overall philosophy of the
*streaming* package, for example eschewing a separate type for intermediate
stages.

The abstract signature covers basic functionality like:

- Yielding elements downstream.
- Mapping over streams.
- Folding over streams.
- Grouping operations that preserve streaming . Only a single level of grouping
is supported. (Not universally implemented; check the feature matrix.)

Library-specific features that are not covered:

- *pipe*'s bidirectionality.
- *conduit*'s integrated leftovers.
- *streaming*'s ability to use different functors.
- any kind of single-stepping.

The signature doesn't provide applicative sinks as a separate type, but the
various fold functions allow interoperability with the folds in the *foldl*
package.

## Feature matrix

There are gaps in coverage in implementation of functions.  Below is a feature
matrix saying which functions are supported by which libraries.

    Key | Module name
    ----|--------------------------
    S   | Streamy.Streaming
    P   | Streamy.Pipes
    C   | Streamy.Conduit

**Types**

    |                 | S | P | C |
    |-----------------|---|---|---|
    | Stream          | X | X | X |
    | Groups          | X | X | X |

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
    | group           | X | X |   |
    | groupBy         | X | X |   |
    | chunksOf        | X | X |   |
    | maps            | X | X |   |
    | concats         | X | X |   |
    | intercalates    | X | X |   |
    | yields          | X | X |   |
    | takes           | X | X |   |
    | splitAt         | X | X |   |
    | span            | X | X |   |


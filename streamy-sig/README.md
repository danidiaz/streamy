# streamy-sig

## Feature matrix

There are gaps in coverage in implementation of functions.  Below is a feature matrix saying which functions are supported by which libraries.

    Key | Module name
    ----|--------------------------
    P   | Streamy.Pipes
    S   | Streamy.Streaming
    C   | Streamy.Conduit

**Stream types**

    |                 | S | T | C |
    |-----------------|---|---|---|
    | Stream          | X | X | X |

**Basic functions**

    |                 | S | T | C |
    |-----------------|---|---|---|
    | yield           | X | X | X |
    | each            | X | X | X |
    | toList          | X | X | X |
    | toList_         | X | X | X |
    | chain           | X | X | X |
    | effects         | X | X | X |


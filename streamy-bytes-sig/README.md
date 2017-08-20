# streamy-bytes-sig

This package provides a *Streamy.Bytes* signature with types and functions for
working with effectful byte streams. Implementations of subsets of this
signature can be found in *streamy-streaming*, *streamy-pipes* and
*streamy-conduit*.

The interface tries to follow the nomenclature and overall philosophy of the
*streaming* package, for example eschewing a separate type for intermediate
stages.

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
    | ByteStream      | X | X |   |
    | SingleByte      | X | X |   |
    | Bytes           | X | X |   |
    | ByteIndex       | X | X |   |

**Basic functions**

    |                 | S | P | C |
    |-----------------|---|---|---|
    | empty           | X | X |   |  
    | singleton       | X | X |   | 
    | pack            | X | X |   | 
    | unpack          | X | X |   | 
    | fromChunks      | X | X |   | 
    | toChunks        | X | X |   | 
    | fromStrict      | X | X |   | 
    | toStrict        | X | X |   | 
    | toStrict_       | X | X |   | 
    | splitAt         | X | X |   | 
    | take            | X | X |   | 
    | fromHandle      | X | X |   | 
    | toHandle        | X | X |   | 



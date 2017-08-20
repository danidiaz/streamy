# streamy-bytes-sig

This signature package expands *streamy-sig* with a *Streamy.Bytes* module
signature containing types and functions for working with effectful byte
streams. Implementations of subsets of this signature can be found in
*streamy-streaming*, *streamy-pipes* and *streamy-conduit*.

The interface tries to follow the nomenclature and overall philosophy of the
*streaming* package, for example eschewing a separate type for intermediate
stages.

If you don't need to work with byte streams, depend on *streamy-sig* alone.

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
    | ByteStream      | X | X | X |
    | SingleByte      | X | X | X |
    | Bytes           | X | X | X |
    | ByteIndex       | X | X | X |

**Basic functions**

    |                 | S | P | C |
    |-----------------|---|---|---|
    | empty           | X | X | X |  
    | singleton       | X | X | X | 
    | pack            | X | X | X | 
    | unpack          | X | X | X | 
    | fromChunks      | X | X | X | 
    | toChunks        | X | X | X | 
    | fromStrict      | X | X | X | 
    | toStrict        | X | X | X | 
    | toStrict_       | X | X | X | 
    | splitAt         | X | X |   | 
    | take            | X | X | X | 
    | fromHandle      | X | X | X | 
    | toHandle        | X | X | X | 



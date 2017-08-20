# streamy-streaming

Implementation of the *streamy-sig* and *streamy-bytes-sig* signatures using
the *streaming* package ecosystem.

This package wraps streams in the *WrappedStream* newtype and delimited streams
in the *WrappedGroups* newtype. You should import the newtypes if you plan to
use functions taken directly from the *streaming* package. 

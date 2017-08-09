# stream-t

## What's this

I'm trying to use the Backpack module system to give a common interface to the
main streaming libraries of Haskell: conduit, pipes, streaming. (Inspired by
how [str-sig](http://next.hackage.haskell.org:8080/package/str-sig) gives a
common interface to String-like types.)

The idea is that the abstract signature would cover some very basic
functionality like yielding stuff downstream, but not library-specific features
like:

- pipe's bidirectionality.
- conduit's integrated leftovers.
- streaming's ability to use different functors.
- any kind of single-stepping (too different 

Things that *perhaps* could get included:

- applicative sinks.
- grouping operations?

## Structure of this project 

- **stream-t-sig** is the abstract signature.
- **stream-t-pipes** is the pipes bridge package.
- **stream-t-testsuite** makes several copies of the signature and matches each
  one with a different implementation (only pipes for now). 

## Do I need all these packages?

Not really, even if you still want to use Backpack to abstract your streaming
library.

You can always define a self-contained signature in your own library, and the
users of your library can define their own "bridge" implementations to make the
types line up.

The advantage of these premade packages is not having to invent your own
signatures and bridge modules.

## Building instructions

I'm using cabal-install 2.0.

I'm trying to build with

> cabal new-build all --enable-tests

But right now it's giving me the following error:

<pre>
Building library instantiated with
  Control.Monad.Trans.Stream = stream-t-pipes-0.1.0.0-inplace:Control.Monad.Trans.Stream.Pipes
  for stream-t-sig-0.1.0.0..
  /usr/bin/ar: /media/sf_hs/stream-t/dist-newstyle/build/x86_64-linux/ghc-8.2.1/stream-t-sig-0.1.0.0/stream-t-sig-0.1.0.0-inplace+D90cCEuLMuMBL3FC6AfBOf/build/stream-t-sig-0.1.0.0-inplace+D90cCEuLMuMBL3FC6AfBOf/objs-6766/libHSstream-t-sig-0.1.0.0-inplace+D90cCEuLMuMBL3FC6AfBOf.a: Operation not permitted
  cabal: Failed to build stream-t-sig-0.1.0.0 (which is required by test:tests
  from stream-t-testsuite-0.1.0.0).
</pre>


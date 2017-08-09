# stream-t

## What's this

As a learning exercise, I'm trying to use the
[Backpack](https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst)
module system to give a common interface to the main streaming libraries of
Haskell: [conduit](http://hackage.haskell.org/package/conduit), [pipes](http://hackage.haskell.org/package/pipes), [streaming](http://hackage.haskell.org/package/streaming). (Inspired by how
[str-sig](http://next.hackage.haskell.org:8080/package/str-sig) gives a common
interface to String-like types.)

The idea is that the abstract signature would cover some very basic
functionality like yielding stuff downstream, but not library-specific features
like:

- pipe's bidirectionality.
- conduit's integrated leftovers.
- streaming's ability to use different functors.
- any kind of single-stepping (too different across libraries).

Things that *perhaps* could get included:

- applicative sinks?
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

## Explanation of the "mixins" stanza

In the package.cabal of stream-t-testsuite, we find the following:

```
mixins:
    stream-t-sig requires (Control.Monad.Trans.Stream as Control.Monad.Trans.Stream.Test.Pipes),
    stream-t-pipes (Control.Monad.Trans.Stream.Pipes as Control.Monad.Trans.Stream.Test.Pipes)
```

What is happening here?

From a Backpack perspective, libraries provide modules but also have "holes"
(the signatures defined and/or used in the library). Libraries that only have
signatures can be seen as "nothing but holes", in a sense.

The lines of the mixins stanza create aliases for both modules and signatures
(signatures are aliased with "requires"). 

Backpack works by lining up signatures and implementation modules by name, and
then checking that they fit together.

For simple cases, it is often enough to simply "slide" an implementation module
into the signature, and not alias the signature at all. Here we are not doing
that; my plan is have multiple aliases of the signature and instantiate each
one differently.

## Building instructions

I'm using [cabal-install](http://hackage.haskell.org/package/cabal-install) 2.0.

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


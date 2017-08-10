# streamy

## What's this?

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

- **streamy-sig** is the abstract signature.
- **streamy-pipes** is the pipes bridge package.
- **streamy-testsuite** makes several copies of the signature and matches each
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

In the package.cabal of the streamy-testsuite, we find the following:

```
mixins:
    test-common 
            (Test.Common as Test.Pipes.Common) 
            requires (Test.Common.Streamy as Test.Pipes.Common.Streamy),
    streamy-pipes (Streamy.Pipes as Test.Pipes.Common.Streamy)
```

What is happening here?

From a Backpack perspective, libraries provide modules but also have "holes"
(the signatures defined and/or used in the library). Libraries that only have
signatures can be seen as "nothing but holes", in a sense.

Each entry of the mixins stance creates a modified "copy" of a library
dependency. For each copy, we can rename both the modules it provides and the
"holes", the abstract signatures on which the provided modules depend. 

We can make multiple simultaneous copies of a library dependency, in need be.

Backpack works by lining up signatures and implementation modules by name, and
then checking that they fit together.

For simple cases, it is often enough to simply "slide" an implementation module
into the signature, and not bother renaming the signature at all.

## Building instructions

Built using [cabal-install](http://hackage.haskell.org/package/cabal-install)
2.0.

> cabal new-build all --enable-tests

## Where can I find further information on Backpack

Edward Z. Yang's [thesis](https://github.com/ezyang/thesis/releases) is a good
resource.

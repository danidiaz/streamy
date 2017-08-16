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

- grouping operations?
- bytestring-specific functionality?

I don't plan to include a separate "applicative sink" datatype, but perhaps
I will include a few fold functions.

## Structure of this project 

- **streamy-sig** is the abstract signature.
- **streamy-pipes** is the pipes "bridge" package.
- **streamy-streaming** 
- **streamy-conduit** 
- **streamy-testsuite** test each of the implementations using a shared set of tests.

## Do I need all these packages?

Not really, even if you still want to use Backpack to abstract your streaming
library.

You can always define a self-contained signature in your own library, and the
users of your library can define their own "bridge" implementations to make the
types line up.

The advantage of these premade packages is not having to invent your own
signatures and bridge modules.

## Building instructions

Built using [cabal-install](http://hackage.haskell.org/package/cabal-install)
2.0.

> cabal new-build all --enable-tests

## Where can I find further information on Backpack?

Edward Z. Yang's [thesis](https://github.com/ezyang/thesis/releases) is quite readable.

[Designing the Backpack signature
ecosystem](http://blog.ezyang.com/2017/03/designing-the-backpack-signature-ecosystem/)
explains the usefulness of signature thinning (see also section 2.7 of the thesis).

The [str-sig](http://next.hackage.haskell.org:8080/package/str-sig) signature
package and its various implementations.

I wrote a few Backpack tips & tricks
[here](https://medium.com/@danidiaz/backpacking-tips-3adb727bb8f7).

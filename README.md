# streamy

## What's this?

As a learning exercise, I'm trying to use the
[Backpack](https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst)
module system to give a common interface to the main streaming libraries of
Haskell: [conduit](http://hackage.haskell.org/package/conduit), [pipes](http://hackage.haskell.org/package/pipes), [streaming](http://hackage.haskell.org/package/streaming). (Inspired by how
[str-sig](http://next.hackage.haskell.org:8080/package/str-sig) gives a common
interface to String-like types.)

## Structure of this project 

- **streamy-sig** is the abstract signature.
- **streamy-bytes-sig** expands *streamy-sig* with a module signature for byte streams.
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

Built using [cabal 2.0](https://www.haskell.org/cabal/).

> cabal new-build all --enable-tests

> cabal new-test streamy-testsuite

## Where can I find further information on Backpack?

Edward Z. Yang's [thesis](https://github.com/ezyang/thesis/releases) is quite readable.

[Designing the Backpack signature
ecosystem](http://blog.ezyang.com/2017/03/designing-the-backpack-signature-ecosystem/)
explains the usefulness of signature thinning (see also section 2.7 of the thesis).

[Backpack for deep learning](http://blog.ezyang.com/2017/08/backpack-for-deep-learning/).

The [str-sig](http://next.hackage.haskell.org:8080/package/str-sig) signature
package and its various implementations.

I wrote a few Backpack tips & tricks
[here](https://medium.com/@danidiaz/backpacking-tips-3adb727bb8f7) and [here](https://medium.com/@danidiaz/backpacking-tips-ii-47fa86e5bf2).


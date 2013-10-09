This library provides Haskell bindings to
[LevelDB](http://leveldb.googlecode.com)

[![Build Status](https://secure.travis-ci.org/kim/leveldb-haskell.png)](http://travis-ci.org/kim/leveldb-haskell)

## History

Version 0.2.0:

* requires LevelDB v1.7
* support for filter policy (LevelDB v1.5), either custom or using the built-in
  bloom filter implementation
* write batch values no longer require a `memcpy` to be early-finalizer-safe
  (introduced in 0.1.1)

Version 0.1.0:

* memory (foreign pointers) is managed through
  [ResourceT](http://hackage.haskell.org/package/resourcet). Note that this
  requires to lift monadic actions inside the `MonadResource` monad, see the
  examples.
* links against shared library (LevelDB v1.3 or higher)
* LevelDB 1.3 API fully supported (including custom comparators, excluding
  custom environments)

Version 0.0.x:

* experimental releases

## Installation

Prerequisites:

* [GHC 7.*](http://www.haskell.org/ghc)
* [Cabal](http://www.haskell.org/cabal), version 1.3 or higher

To install from checked-out source:

```shell
$ ./get.sh
$ cabal configure
$ cabal install
```

To create a source Cabal package for distribution:

```
$ runghc ./Setup.sh sdist
```

## Notes

This library is in very early stage and has seen very limited testing. Comments
and contributions are welcome.

## Bugs and Contributing

Please report issues via http://github.com/kim/leveldb-haskell/issues.<br />
Patches are best submitted as pull requests, or via email
(kim.altintop@gmail.com).

## License

BSD 3, see LICENSE file.

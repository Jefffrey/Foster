###Build instructions

To build the package you'll need the Haskell Platform and then `cd` in the project directory and call:

```
cabal sandbox init
cabal configure
cabal install --only-dependencies
cabal build
```

Then you'll be able to run the executable with:

```
cabal run
```

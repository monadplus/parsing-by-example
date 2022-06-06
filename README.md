# parsing-by-example [![Haskell CI](https://github.com/monadplus/parsing-by-example/actions/workflows/ci.yaml/badge.svg)](https://github.com/monadplus/parsing-by-example/actions/workflows/ci.yaml)

This project explores different methods and libraries for parsing CFGs in Haskell.

## JSON: Earley & megaparsec

This example includes lexing with [megaparsec](https://hackage.haskell.org/package/megaparsec) and parsing with [Earley](https://hackage.haskell.org/package/Earley).

Sources at [./src/JSON](./src/JSON).

Running the example:

``` sh
cabal run parsing-by-example -- -f examples/random.json
```

Running the property-based tests:

``` sh
cabal run tasty
```

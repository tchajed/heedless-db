# Heedless DB

[![Build Status](https://travis-ci.org/tchajed/heedless-db.svg?branch=master)](https://travis-ci.org/tchajed/heedless-db)

A simple, persistent key-value store implemented in Haskell. This is a prototype for a verified version, along the same lines as [specious-db](https://github.com/tchajed/specious-db), but implemented in Haskell to more closely resemble our shallow embedding and extraction approach for getting runnable code.

The goal is to implement the same functionality as specious-db and benchmark to identify whether Haskell performance is acceptable and what we need to do to ensure that.

## TODO

- Figure out how to handle available operations from data structures. This resembles the problem we have in Coq of keeping track of references to data structures and the filesystem. The current solution for the filesystem involves writing the implementation in a typeclass that has MonadIO + all the operations we want; this is pretty good because it looks like a layer, but it's awkward when there are lots of operations.
- First test: get just a write-ahead log working (meaning port `wal.go`) and run the database only enough to fill the log, compare performance of that.
- Need to figure out how efficient array access in Haskell is using mutable arrays from the `array` package.

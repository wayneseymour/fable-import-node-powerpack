# fable-import-node-powerpack

Utilities for Fable Nodejs apps

[![Build Status](https://travis-ci.org/intel-hpdd/fable-import-node-powerpack.svg?branch=master)](https://travis-ci.org/intel-hpdd/fable-import-node-powerpack)
[![codecov](https://codecov.io/gh/intel-hpdd/fable-import-node-powerpack/branch/master/graph/badge.svg)](https://codecov.io/gh/intel-hpdd/fable-import-node-powerpack)

## Requirements

* [dotnet SDK](https://www.microsoft.com/net/download/core) 2.0 or higher
* [node.js](https://nodejs.org) 6.11 or higher

> npm comes bundled with node.js, but we recommend to use at least npm 5. If you
> have npm installed, you can upgrade it by running `npm install -g npm`.

Although is not a Fable requirement, on macOS and Linux you'll need
[Mono](http://www.mono-project.com/) for other F# tooling like Paket or editor
support.

## Building and running the app

* Install JS dependencies: `npm i`
* Install F# dependencies: `dotnet restore`
* Run the tests `dotnet fable npm-run test`
* Run the tests and output code coverage `dotnet fable npm-run coverage`
* Run the tests in watch mode:
  * In one terminal `dotnet fable start`
  * In a second terminal `npm run test-watch`
    * This will allow you to run all, or just a subset of tests, and will
      re-test the changed files on save.

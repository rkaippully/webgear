# WebGear User Guide

## Introduction
WebGear is a library to build composable, extensible, type-safe APIs. This guide is designed to help you get started
with WebGear. It follows a tutorial style; if you are looking for reference material, see the [API
documentation](https://hackage.haskell.org/package/webgear-server).

## Requirements
This guide assumes that you have a good idea of how HTTP APIs work in general. You also need a good understanding of
Haskell programming language features such as type classes, monads, monad transformers etc.

WebGear uses a small set of GHC haskell features that are generally considered advanced. But this guide does not assume
that you are an expert in these. All such features will be explained in this guide.

However, it is assumed that you understand some basic GHC language extensions such as
[OverloadedStrings](https://downloads.haskell.org/~ghc/8.10.2/docs/html/users_guide/glasgow_exts.html#extension-OverloadedStrings),
[TypeApplications](https://downloads.haskell.org/~ghc/8.10.2/docs/html/users_guide/glasgow_exts.html#extension-TypeApplications),
and
[FlexibleContexts](https://downloads.haskell.org/~ghc/8.10.2/docs/html/users_guide/glasgow_exts.html#extension-FlexibleContexts). A
cursory understanding of
[QuasiQuotes](https://downloads.haskell.org/~ghc/8.10.2/docs/html/users_guide/glasgow_exts.html#extension-QuasiQuotes)
is useful but not essential.

You need a working haskell development environment with either [stack](https://haskellstack.org) or
[cabal](https://cabal.readthedocs.io). If you are using cabal, use GHC version 8.8.4.

## Quickstart
Let us set up a project first.

### Create a project

=== "Stack"
    ```shell
    stack new webgear-guide simple --resolver lts-16.12
    cd webgear-guide
    ```

=== "Cabal"
    ```shell
    cabal update
    mkdir webgear-guide
    cd webgear-guide
    cabal init --application-dir=src
    ```

### Adjust dependencies
Edit `webgear-guide.cabal` and add the following dependencies under `build-depends` section:

```yaml
build-depends: base                  ==4.13.0.0
             , webgear-server        ==0.2.0
             , mtl                   ==2.2.2
             , wai                   ==3.2.2.1
             , warp                  ==3.3.13
             , text                  ==1.2.4.0
             , bytestring            ==0.10.10.1
             , utf8-string           ==1.0.1.1
             , time                  ==1.9.3
             , unordered-containers  ==0.2.10.0
             , hashable              ==1.3.0.0
             , aeson                 ==1.4.7.1
             , http-types            ==0.12.3
```

In addition to this, if you are using stack, add the following to `stack.yaml`:

```yaml
extra-deps:
- webgear-server-0.2.0
```

### Build the project
=== "Stack"
    ```shell
    stack build
    ```
=== "Cabal"
    ```shell
    cabal build
    ```

!!! tip
    If this is the first time, it will take a long time to download and build the dependencies. Subsequent builds will
    use cached artifacts and will be faster.

# `registry-hedgehog` [![Hackage](https://img.shields.io/hackage/v/registry-hedgehog.svg)](https://hackage.haskell.org/package/registry-hedgehog) [![Build Status](https://github.com/etorreborre/registry-hedgehog/workflows/ci/badge.svg)](https://github.com/etorreborre/registry-hedgehog/actions)

##### *It's functions all the way down* <img src="doc/images/unboxed-bottomup.jpg" border="0"/>

#### Presentation

This library is an add-on to [`registry`](https://github.com/etorreborre/registry), providing useful functions to create and run Hedgehog properties.

 - it unifies the idea of a test and a property: a test is which runs only once
 - it provides an easy way to wire Hedgehog generators using the [`registry`](https://github.com/etorreborre/registry) library
 - it gives you ways to precisely control the data generation by modifying existing generators and compose those modifications

The main modules are:

 - `Data.Registry.Hedgehog`: functions used to modify a registry containing generators
 - `Test.Tasty.Hedgehogx`: supplement to the `Test.Tasty.Hedgehog` module to declare and modify tests/properties

#### Tutorials and examples

 - some examples of usage in the form of a [small specification](./test/Test/Data/Registry/HedgehogSpec.hs)
 - [tutorial with a data model](./doc/tutorial.md)

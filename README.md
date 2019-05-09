# `registry-hedgehog

##### *It's functions all the way down* <img src="doc/images/unboxed-bottomup.jpg" border="0"/>

#### Presentation

This library is an add-on to [`registry`](https://github.com/etorreborre/registry), providing useful functions to create and run Hedgehog properties.

 - it unifies the idea of a test and a property: a test is which runs only once
 - it provides an easy way to wire Hedgehog generators using the [`registry`](https://github.com/etorreborre/registry) library
 - it gives you ways to precisely control the data generation by modifying existing generators and compose those modifications

The main modules are:

 - `Data.Registry.Hedgehog`: functions used to modify a registry containing generators
 - `Test.Tasty.Hedgehogx`: supplement to the `Test.Tasty.Hedgehog` module to declare and modify tests/properties

There is for now a small tutorial in the form of a [small specification](./test/Test/Data/Registry/HedgehogSpec.hs)

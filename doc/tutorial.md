# Registry-Hedgehog tutorial

## Introduction

[registry](http://github.com/etorreborre/registry) is a library offering an alternative to typeclasses for implicitly assembling functionalities in Haskell.
For example a typeclass for an `Encoder` will implicitly retrieve other `Encoder`s and use them to build a new one:
```
instance (Encoder Int) => Encode Age where
  encode (Age n) = encode n
```

This can also be written "manually" as a function call:
```
newtype Encoder a = Encode { encode :: a -> Text }

ageEncoder :: Encoder Int -> Encoder Age
ageEncoder intEncoder = Encoder (\Age n -> encode intEncoder n)
```

`registry` automates the call of such functions when it is necessary to build more complex structures like an application or data generators.
The benefits are:

 - the construction of instance is still "type-directed" as with typeclasses, you can make an instance by specifying its type only, like `Encoder Age`
 - the construction can be altered by injecting some hand-crafted values in order to replace normal components with mocks for example
 - it is also possible to inject values in a specific context only, for example a specific `Encoder Int` used by the `Encoder Age` but not by the `Encoder Year`

If you want to get a good mental picture of what a registry is, you can visualize:

 - an ordered list of values and functions
 - an algorithm applying those functions to values in order to obtain other values based on their type
 - some settings to "tweak" the algorithm (more on that later)

The algorithm goes like this:

 1. to get a value of type `a` check if there already is a value of that type in the registry and _take the first available one_
 2. otherwise check if there is a function returning the type `a` (and _take the first available one_)
 3. if there is one, create all the values required to call the function
 4. then call the function and get a value of type `a`

This is pretty straightforward and we can see that "taking the first available value" allows us to "override" the registry by adding any value (or function actually) "on top" since this is an ordered list.

Let's see on a few examples what this means for generating data.

## Application to generators wiring

### Introduction

[Hedgehog](https://github.com/hedgehogqa/haskell-hedgehog) is a library departing from [QuickCheck](https://hackage.haskell.org/package/QuickCheck) for doing property-based testing:

 - it does not use typeclasses for generating data
 - shrinking is integrated with the generation so it respects filters on generators
 - generators have a notion of `Range` specifying how values are supposed to grow or to be shrinked with respect to a size parameter (which increases with the number of tests)
 - failures are reported inline with the source code, showing which values were generated and which assertions executed
 - it uses visual diffs to display differences between expected and actual values
 - it is designed around monad transformers to unify generation, assertions and general effects into the same code

You can watch a presentation about Hedgehog [here](https://www.youtube.com/watch?v=AIv_9T0xKEo).

Similarly to how `registry` can be used to assemble application components we can use `registry` to make generators. However when working with generators there are more things we would like to do:

 - we want to use generators for `ADT`s because we have `ADT`s in our data models
 - there are many registry tweaks we might want to apply in order to specify what to generate exactly
 - we would like to have "effectful" generators for example to specify that we want to cycle across all the data constructors of a given ADT or to generate only distinct values

The [`registry-hedgehog`](https://github.com/etorreborre/registry-hedgehog) library provides some specific support for those use cases and more:

 - there is an integration with `Tasty` so that Hedgehog properties can be executed with [`tasty`](https://hackage.haskell.org/package/tasty)
 - it unifies the notion of a "test" and a "property". A "test" is simply a property that you run once. This means that you can use generators
   to easily create data for your tests

In the following exercises we will learn to:

 - create generators for a simple data model using the combinators provided by `registry-hedgehog`
 - create generators for relationships: lists, maybes, etc...
 - create generators for ADTs
 - replace a default generators with some a specific one
 - use stateful registry transformations to reuse some generators setups

### Our first registry for generators

We are going to create generators for the following data model (see `test/Test/Tutorial/DataModel.hs`)
```
data Company = Company {
  companyName :: Text
, departments :: [Department]
} deriving (Eq, Show)

data Department = Department {
  departmentName :: Text
, employees      :: [Employee]
} deriving (Eq, Show)

data Employee = Employee {
  employeeName   :: Text
, employeeStatus :: EmployeeStatus
, salary         :: Int
, bonus          :: Maybe Int
} deriving (Eq, Show)

data EmployeeStatus =
   Permanent
 | Temporary Int -- number of days
 deriving (Eq, Show)
```

One first step could be to write Hedgehog generators manually:
```
genEmployee :: Gen Text -> Gen EmployeeStatus -> Gen Int -> Gen (Maybe Int) -> Gen Employee
```

But we are going to use `registry-hedgehog` directly to create the generators.

### Exercise 1

To create a registry for generators

1. import `Data.Registry.Hedgehog` and create a registry with the following operators:
     - `genFun` to add a constructor (like `Company`)
     - `genVal` to add a generator (like `genInt :: Gen Int` to generate integers)
     -  `<:` to append elements to the registry

2. try to make a `GenIO Company` with `make @(GenIO Company) registry`. This should not compile because
   there are missing generators. You can still create an "unchecked" registry with `+:` instead of `<:`

_Notes_:

 - `genFun` is an alias for `funTo @GenIO`. We are using `GenIO a`, which is an alias for `GenT IO a` instead of just `Gen a` (alias for `GenT Identity a`)
   to be able to support to stateful generators later on
 - you will need to import `Hedgehog`, `Hedgehog.Gen` and `Hedgehog.Range` to create `Int` and `Text` generators
 - `EmployeeStatus` is an ADT. For now only generate `Permanent` employees, we will see later how to deal with ADTs

### Exercise 2

We are missing generators for "relationships" like `[Employee]` or `Maybe Int`.

1. add them using the `listOf @a` and `maybeOf @a` functions
2. great, we can now write our first Hedgehog property!
```
import Test.Tasty.Hedgehogx

forall :: forall a . (Typeable a, Show a) => PropertyT IO a
forall = forAllT $ genWith @a registry

test_company = test "make a company" $ do
  collect =<< forall @Company
```
3. run the property in the repl with `run test_company`

_Notes_:

  - `genWith @a` is a function making a `GenIO a` from a registry
  - `forall` is a helper function to avoid repeating the registry name in every test
  - `collect` is a Hedgehog function displaying the frequencies for generated values

### Exercise 3

We need to create a better generator for the `EmployeeStatus` ADT. The general pattern to do this with `registry` is to do the following:
```
genEmployeeStatus :: GenIO (Tag "Permanent" EmployeeStatus) -> GenIO (Tag "Temporary" EmployeeStatus) -> GenIOEmployeeStatus
genEmployeeStatus genPermanent genEmployeeStatus = choice [unTag <$> genPermanent, unTag genTemporary]

registry =
     funTo @Gen (tag "Permanent" Permanent)
  <: funTo @Gen (tag "Temporary" Temporary)
  <: fun genEmployeeStatus
```
This uses both constructors of the ADT to generate individual "branches" and the `choice` combinator to choose which alternative to take.

Since this is some repetitive code, this has been automated with TemplateHaskell.

1. use the `$(makeGenerators ''EmployeeStatus)` function to add a better `EmployeeStatus` generator to the registry (you can add it on top of the previous registry)
2. run the following property to check the generation of `EmployeeStatus`
```
test_employee_status = prop "make an employee status" $ do
  collect =<< forall @EmployeeStatus
```

_Notes_:

 - there are other useful functions in Hedgehog to check the coverage: `classify` and `cover`
 - the `makeGenerators` TemplateHaskell function provides additional functionality with the ability to "cycle" across constructors instead of randomly selecting one (see Exercise 14)

### Exercise 4

Since we are using a registry to make our generators we can now override generators in some specific places to generate more accurate data.
For example let's generate some department names with only 5 upper-case letters.

1. create a better `Text` generator for department names
```
genDepartmentName = T.take 5 . T.toUpper <$> genText
```

2. specialize the registry `@(GenIO Department)` to use that generator instead of the default one
```
registry' = specializeGen @Department registry
```

3. run the following property to check department names
```
test_department_name = prop "make a department" $ do
  department <- forall @Department
  collect (departmentName department)
```

_Notes_: how would you do this with QuickCheck `Arbitrary` typeclass?

### Exercise 5

In real tests we might have to specialize the generation in many places, for example to generate only companies with 1 department, or with just one employee per department, with certain name or status, and so on. This means that we need to modify the registry several times. In order to do this we can use a `State` monad and take advantage that a `Hedgehog` property is actually a `PropertyT` monad transformer.

1. create a function `setDepartmentName` to set a specific generator for department names. This uses `specializeGenS` to create a `PropertyT (StateT (Registry _ _) IO ())`
```
setDepartmentName = specializeGenS @Department genDepartmentName
```

2. use the `addFunS` and `listOfMinMax` functions to add new functions in the registry to limit the generation of lists of departments and employees
```
setOneDepartment = addFunS $ listOfMinMax @Department 1 1
setOneEmployee   = addFunS $ listOfMinMax @Employee 1 1
```

3. Since those functions are returning a `PropertyT StateT ...` they can be composed with `>>`. Do this to create a "small company"
```
setSmallCompany = setOneEmployee >> setOneDepartment
```

4. Now write a property to check the generated companies
```
test_small_company = prop "make a small company" $ runS registry $ do
  setSmallCompany
  setDepartmentName
  collect =<< forallS @Company
```

`runS` is a helper function to run a stateful `Property (StateT Registry) IO ()` property and make it a normal Hedgehog property `PropertyT IO ()`.

You should noticed something that doesn't look good. We have specialized the `Text` generation for everything "under" `Department`, including the employee name. Can you fix this and use this generator instead for employee names?
```
genEmployeeName :: Gen Text
genEmployeeName = T.take 10 . T.toLower <$> genText
```

_Notes_:

  - the `S` in the `registry-hedgehog` combinators `specializeGenS`, `addFunS`, ... signifies that we are using a `State` monad

### Exercise 6

We can now be a bit more fancy tweak our generation a bit more, try to generate data with the following modifications (use the `collect` function to visualize generated values):

1. force the generator for `Int` to always return `1`
```
setGenS @Int (pure 1)
```

2. "cycle" the constructors of `EmployeeStatus`
```
setCycleChooserS @EmployeeStatus
```

3. make sure the department names are all distinct
```
setDistinctForS @Department @Text
```

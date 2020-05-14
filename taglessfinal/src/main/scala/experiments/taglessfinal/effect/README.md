Using Effects in Functional Programming
=======================================

Looking into the concepts from John De Goes' talk ['FP to the Max']. See also the corresponding [Gist].

['FP to the Max']: https://www.youtube.com/watch?v=sxudIMiOo68
[Gist]: https://gist.github.com/jdegoes/1b43f43e2d1e845201de853815ab3cb9

Content of this package
-----------------------
| filename                | description                                                                                                         |
|-------------------------|---------------------------------------------------------------------------------------------------------------------|
| `BaseApplication.scala` | the initial code sample                                                                                             |
| `FixBugs.scala`         | illiminate the partial (not total) functions, i.o.w. fix the runtime errors                                         |
| `monadic/*`             | convert any side effect (interaction with the world) into a description of the effect                               |
| `abstraction/*`         | introducing type classes to abstract over `IO` types, such that other monads can be used as well (e.g. for testing) |
| `structuredMessages/*`  | deferring creation of unstructured data (`String`) by using data structures that describe the unstructured data     |

Some notes from the presentation
--------------------------------

* FP is about programming with functions. Functions must be
    * **total**: for every input there is an output;
    * **deterministic**: for every same input, it returns the same output;
    * **pure**: the only effect is computing the output; no side-effects.
* Examples:
    * `def println(s: String): Unit` - total and deterministic, but not pure
    * `def readLine(): String` - total (almost), not deterministic, not pure
    * `def parseInt(s: String): Int` - not total, deterministic, pure
* FP gives a number of benefits:
    * reason about a program using equational reasoning
    * reason about a program using type-based reasoning
    * refactor a program without changing its meaning
    * simplify testing a program
* Make sure the program doesn't have partial (non-total) functions. They lead to runtime errors.
* (_after creating the `monadic` package_) we're still not satisfied:
    * `IO` is just modeling the execution of a chunk of code. We can't do anything with it apart
      from running and composing with other `IO`s.
    * When we run this code, it is still going to perform side effects. Hence we still cannot test
      that our code works using Unit Tests (ScalaTest, JUnit, etc.)
    * `IO` could describe any effect. So from a type-based perspective we cannot tell what side-effects
      `String => IO[Unit]` actually performs.
    * _Solution_: abstraction over 'things that look like `IO`'. By abstracting, we can get more
      specific data types that represent a more fine-grained kind of effect.
* (_after creating the `abstraction` package_)
    * We abstracted over `IO` and got a fine-grained effect system, where various kinds of effects
      could be expressed independent of each other.
    * Now `IO` is just a specialization, in which case the actual code is executed (e.g. random numbers,
      console read/write). It is quite easy to exchange the 'real execution' for something that
      captures the effects in a `List`, which can be inspected later. For this, the `State` monad can
      be specialized with a `TestData` datastructure, by implementing the effect interfaces (`Program`,
      `Random` and `Console`).
    * **What is still a problem**, though, is that we throw away a lot of structured information by
      storing it in a `String`. `println(s: String)` requires parsing of printed `String` on testing.
      We can avoid this by defering the conversion to `String` until the very last moment in the
      `IO` monad.
* (_after creating the `structuredMessages` package_)
    * Now the testability has increased, because we're no longer 'printing actual strings', but
      we're modelling the `String`s as data structures. On these we can validate during a test that
      the right parameters were used.
    * Using this pattern, we can also easily swap out the actual `String`s that result from these
      data structures. Addition of different localisations and formattings are made much more easy.

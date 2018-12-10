# Feature Compare

本文基于 [Comparing Scala relational database access libraries](https://softwaremill.com/comparing-scala-relational-database-access-libraries)/

| Feature | Slick | Dobbie | Quill | ScalikeJDBC | scala-sql |
------- | ----- | ------ | ----- | ----------- | --------- |
|Model representation| Case classes/tuples, or hand-mapped types | Case classes, primitives, tuples, HLists, Shapeless records | Case classes | None OR case classes w/ macros add-on | Case classes, extensible Primitives |
|Meta-model| Lifted | None | By converntion | None OR by convention when using macros add-on | None
|Query language| Embedded DSL | SQL | Quoted DSL | SQL / embedded DSL | SQL |
|Type-safe queries| Yes | No | Yes | Partial | Yes |
|Asynchronous| Async layer on top of blocking drivers | No | No OR fully asynchronous, depending on driver | No OR fully asynchronous (in development) | No
|Results wrapped in (monad)| Future | IOLite, Task (scalaz, monix, fs2) or any custom | None (sync), Future (async) or IO | None | None
|Transactions| Compose queries | Compose queries | Pass implicit ExecutionContext OR compose queries | Pass implicit Session | withTransaction API
|Query representation| Compositional | Compositional | Single statement OR compositional | Single statement | Single statement|
|Streaming| Reactive streams | fs2 | No | Reactive streams | No


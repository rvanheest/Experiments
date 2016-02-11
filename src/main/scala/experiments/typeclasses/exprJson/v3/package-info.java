/**
 * Using implicitly instead of implicit
 *
 * 1. A typeclass is represented by a parameterized trait (e.g. Json[A]), defining operations
 * on member types
 * 2. A type T is a member of typeclass TC[_] if there is a value of type TC[T] available in
 * implicit scope
 * 3. A context bound [T : TC] in the type parameter list for a class or method asserts that
 * T is a member of TC[_] (similar to [T <: U])
 *
 * subtype polymorphism -> subtype tightly coupled to supertype
 *  ad-hoc polymorphism -> no coupling between typeclasses and their members
 */
package experiments.typeclasses.exprJson.v3;

package exercises

import lectures.part4implicits.TypeClasses.User

object EqualityPlayground extends App{

  /*
  *
  * Equality
  * */

  val john = User("John", 32, "john@rockthejvm.com")
  val victor = User("Victor", 23, "victor@rockthejvm.com")

  trait Equal[T] {
    def apply(a: T, b: T): Boolean
  }

  object UserNameEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name
  }

  implicit object UserAgeEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.age == b.age
  }

  /*
  Exercise: Implement the TC pattern for the Equality tc.
  */

  object Equal {
    def apply[T](a: T, b: T)(implicit equality: Equal[T]): Boolean =
      equality.apply(a, b)
  }

  println(Equal.apply(john, victor))
  println(Equal.apply(john, john))

  /*
    Exercise - improve the Equal TC with an implicit conversion class
    ===(anotherValue: T)
    !==(anotherValue: T)
   */

  implicit class EqualEnrichment[T](value: T) {
    def ===(anotherValue: T)(implicit equal: Equal[T]): Boolean = equal(value, anotherValue)
    def !==(anotherValue: T)(implicit equal: Equal[T]): Boolean = !equal(value, anotherValue)
  }

  println()
  println(john === victor)
  println(john !== victor)
  println(john === john)

  /*
    john.===(anotherJohn)
    new EqualEnrichment[User](john).===(anotherJohn)
    new EqualEnrichment[User](john).===(anotherJohn)(NameEquality)
   */

  /*
    TYPE SAFE
   */
  println(john == 43)
  //  println(john === 43) // TYPE SAFE (the compiler don't let me to compile de code)

}

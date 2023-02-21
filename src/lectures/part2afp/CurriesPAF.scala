package lectures.part2afp

object CurriesPAF extends App{

  // curried functions
  val superAdder: Int => Int => Int = x => y => x + y

  val add3 = superAdder(3)
  println(add3(5))
  println(superAdder(3)(5)) // curried function

  // METHOD!
  def curriedAdder(x: Int)(y: Int): Int = x + y // curried method

  val add4: Int => Int = curriedAdder(4)
  // lifting = ETA-EXPANSION

  // functions != methods (JVM limitation)
  def inc(x: Int) = x + 1
  List(1, 2, 3).map(x => inc(x)) // ETA-expansion

  // Partial function applications
  // val add5 = curriedAdder(5)(_)
  val add5 = curriedAdder(5) _

  // EXERCISE
  val simpleAddFunction = (x: Int, y: Int) => x + y

  def simpleAddMethod(x: Int, y: Int) = x + y

  def curriedAddMethod(x: Int)(y: Int) = x + y

  // add7: Int => Int = y => 7 + y
  // as many different implementations of add7 using the above
  // be creative!

  // my solution...
  val add7_1: Int => Int = (y: Int) => simpleAddFunction(7 , y)

  val add7_2: Int => Int = (y: Int) => simpleAddMethod(7 , y)

  val add7_3: Int => Int = (y: Int) => curriedAddMethod(7)(y)

  val add7_4: Int => Int = curriedAddMethod(7)

  val add7_5: Int => Int = simpleAddFunction(7, _)

  val add7_6: Int => Int = simpleAddMethod(7, _)

  // solution
  val solution_add7 = (x: Int) => simpleAddFunction(7, x) // simplest
  val solution_add7_2 = simpleAddFunction.curried(7)
  val solution_add7_6 = simpleAddFunction(7, _: Int) // works as well

  val solution_add7_3 = curriedAddMethod(7) _ // PAF
  val solution_add7_4 = curriedAddMethod(7)(_) // PAF = alternative syntax

  val solution_add7_5 = simpleAddMethod(7, _: Int) // alternative syntax for turning methods into function values
  // y => simpleAddMethod(7, y)

  // EXERCISES
  /*
    1.  Process a list of numbers and return their string representations with different formats
        Use the %4.2f, %8.6f and %14.12f with a curried formatter function.
   */

  // My solution
  val curriedFormatter: String => Double => String = selectedFormat => number => selectedFormat.format(number)
  val format_1 = curriedFormatter("%4.2f")
  val format_2 = curriedFormatter("%8.6f")
  val format_3 = curriedFormatter("%14.12f")
  println(format_1(math.Pi))
  println(format_2(math.Pi))
  println(format_3(math.Pi))

  /*
    2.  difference between
        - functions vs methods
        - parameters: by-name vs 0-lambda
   */
  def byName(n: => Int) = n + 1

  def byFunction(f: () => Int) = f() + 1

  def method: Int = 42

  def parenMethod(): Int = 42

  /*
    calling byName and byFunction
    - int
    - method
    - parenMethod
    - lambda
    - PAF
   */
  println(byName(23)) // ok
  println(byName(method)) // ok
  println(byName(parenMethod())) // ok
  println(byName(parenMethod)) // ok
  // println(byName(() => 42)) // no ok

  // byFunction(45) // not ok
  // byFunction(method) // not ok!!!!!! does not do ETA-expansion!
  println(byFunction(parenMethod)) // compiler does ETA-expansion
  println(byFunction(() => 46))
  byFunction(parenMethod _) // also works, but warning- unnecessary
}

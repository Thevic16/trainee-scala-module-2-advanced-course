package exercises

import scala.annotation.tailrec

/*
  Exercise: implement a lazily evaluated, singly linked STREAM of elements.
  naturals = MyStream.from(1)(x => x + 1) = stream of natural numbers (potentially infinite!)
  naturals.take(100).foreach(println) // lazily evaluated stream of the first 100 naturals (finite stream)
  naturals.foreach(println) // will crash - infinite!
  naturals.map(_ * 2) // stream of all even numbers (potentially infinite)
 */
abstract class MyStream[+A] {
  def isEmpty: Boolean

  def head: A

  def tail: MyStream[A]

  def #::[B >: A](element: B): MyStream[B] // prepend operator

  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] // concatenate two streams

  def foreach(f: A => Unit): Unit

  def map[B](f: A => B): MyStream[B]

  def flatMap[B](f: A => MyStream[B]): MyStream[B]

  def filter(predicate: A => Boolean): MyStream[A]

  def take(n: Int): MyStream[A] // takes the first n elements out of this stream

  def takeAsList(n: Int): List[A]

  def toList: List[A]
}

case object EmptyStream extends MyStream[Nothing] {
  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException

  override def tail: MyStream[Nothing] = throw new NoSuchElementException

  override def #::[B >: Nothing](element: B): MyStream[B] = new Cons[B](element, EmptyStream)

  override def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream

  override def foreach(f: Nothing => Unit): Unit = ()

  override def map[B](f: Nothing => B): MyStream[B] = EmptyStream

  override def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = EmptyStream

  override def filter(predicate: Nothing => Boolean): MyStream[Nothing] = EmptyStream

  override def take(n: Int): MyStream[Nothing] = EmptyStream

  override def takeAsList(n: Int): List[Nothing] = List()

  override def toList: List[Nothing] = List()

}

class Cons[+A](hd: A, tl: => MyStream[A]) extends MyStream[A] {
  override def isEmpty: Boolean = false

  override val head: A = hd

  override lazy val tail: MyStream[A] = tl // call by need

  override def #::[B >: A](element: B): MyStream[B] = new Cons[B](element, this)

  override def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = new Cons[B](head, tail ++ anotherStream)

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def map[B](f: A => B): MyStream[B] = new Cons[B](f(head), tail.map(f))

  override def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)

  override def filter(predicate: A => Boolean): MyStream[A] = {
    if (predicate(head)) new Cons[A](head, tail.filter(predicate))
    else tail.filter(predicate) // preserves lazy eval!
  }

  override def take(n: Int): MyStream[A] = {
    if (n == 1) new Cons[A](head, EmptyStream)
    else if (n > 1) new Cons[A](head, tail.take(n - 1))
    else new Cons[A](head, EmptyStream)
  }

  override def takeAsList(n: Int): List[A] = {
    val stream: MyStream[A] = take(n)
    stream.toList
  }

  override def toList: List[A] = {
    def go(acc: List[A]): List[A] = {
      if (isEmpty) acc
      else acc ++ tail.toList :+ head
    }

    go(List[A]())
  }
}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] =
    new Cons[A](start, MyStream.from(generator(start))(generator))
}


object StreamsPlayground extends App {
  val naturals = MyStream.from(1)(_ + 1)
  println(naturals.head)
  println(naturals.tail.head)
  println(naturals.tail.tail.head)

  val startFrom0 = 0 #:: naturals // naturals.#::(0)
  println(startFrom0.head)

  startFrom0.take(100).foreach(println)

  // map, flatMap
  println(startFrom0.map(_ * 2).take(100).toList)
  val streanFlatMap = startFrom0.flatMap(x => new Cons(x, new Cons(x + 1, EmptyStream))).take(10)
  println(streanFlatMap.toList)
  println(startFrom0.filter(_ < 10).take(10).take(20).toList)

  // Exercises on streams
  // 1 - stream of Fibonacci numbers
  // 2 - stream of prime numbers with Eratosthenes' sieve
  /*
    [ 2 3 4 ... ]
    filter out all numbers divisible by 2
    [ 2 3 5 7 9 11 ...]
    filter  out all numbers divisible by 3
    [ 2 3 5 7 11 13 17 ... ]
    filter out all numbers divisible by 5
      ...
   */

  // 1.
 // Solution
  println("\n fibonacci")
 def fibonacci(first: BigInt, second: BigInt): MyStream[BigInt] =
   new Cons(first, fibonacci(second, first + second))

  println(fibonacci(1, 1).take(100).toList)


  println("\n prime numbers with Eratosthenes' sieve")
  println("My solution ...")
  def filterNaturalsByPrimeList(primeList: List[Int], natural: BigInt): Boolean = {
    val listBoolean: List[Boolean] = primeList.map(prime => natural % prime != 0) // True if the number if no divisible for the prime
    listBoolean.foldRight(true)(_ && _) // true if natural if no divisible for any prime
  }

  def primeGenerator(primeList: List[Int], naturals: MyStream[Int]): MyStream[Int] = {
    val filteredNaturals = naturals.filter(filterNaturalsByPrimeList(primeList,_))
    val nexPrime = filteredNaturals.head

    new Cons(nexPrime, primeGenerator(primeList :+ nexPrime, filteredNaturals.tail))
  }

  val naturalsFromTwo = MyStream.from(2)(_ + 1)
  println(primeGenerator(List(), naturalsFromTwo).take(15).toList.reverse)

  // Solution video.
  /*
    [ 2 3 4 5 6 7 8 9 10 11 12 ...
    [ 2 3 5 7 9 11 13 ...
    [ 2 eratosthenes applied to (numbers filtered by n % 2 != 0)
    [ 2 3 eratosthenes applied to [ 5 7 9 11 ... ] filtered by n % 3 != 0
    [ 2 3 5
   */
  // eratosthenes sieve
  def eratosthenes(numbers: MyStream[Int]): MyStream[Int] =
  if (numbers.isEmpty) numbers
  else new Cons(numbers.head, eratosthenes(numbers.tail.filter(_ % numbers.head != 0)))

  println(eratosthenes(MyStream.from(2)(_ + 1)).take(100).toList)

}

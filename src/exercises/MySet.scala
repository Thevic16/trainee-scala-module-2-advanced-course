package exercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {

  /*
    EXERCISE - implement a functional set
   */

  def apply(elem: A): Boolean =
    contains(elem)

  def contains(elem: A): Boolean

  def +(elem: A): MySet[A]

  def ++(anotherSet: MySet[A]): MySet[A] // union

  def map[B](f: A => B): MySet[B]

  def flatMap[B](f: A => MySet[B]): MySet[B]

  def filter(predicate: A => Boolean): MySet[A]

  def foreach(f: A => Unit): Unit

  /*
   EXERCISE #2
   - removing an element
   - intersection with another set
   - difference with another set
  */
  def -(elem: A): MySet[A]

  def --(anotherSet: MySet[A]): MySet[A] // difference

  def &(anotherSet: MySet[A]): MySet[A] // intersection

  // EXERCISE #3 - implement a unary_! = NEGATION of a set
  // set[1,2,3] =>
  def unary_! : MySet[A]
}

class EmptySet[A] extends MySet[A] {

  override def contains(elem: A): Boolean = false

  override def +(elem: A): MySet[A] = NoEmptySet(elem, new EmptySet[A])

  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet match { // This is the same that just returning anotherSet
    case set @ NoEmptySet(_, _) => set
    case _ => new EmptySet[A]
  }

  override def map[B](f: A => B): MySet[B] = new EmptySet[B]

  override def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]

  override def filter(predicate: A => Boolean): MySet[A] = new EmptySet[A]

  override def foreach(f: A => Unit): Unit = ()

  override def -(elem: A): MySet[A] = this

  override def --(anotherSet: MySet[A]): MySet[A] = this

  override def &(anotherSet: MySet[A]): MySet[A] = this

  def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)
}

case class NoEmptySet[A](head: A, tail: MySet[A]) extends MySet[A]{

  override def contains(elem: A): Boolean = head == elem || tail.contains(elem)

  override def +(elem: A): MySet[A] = if(contains(elem)) this else NoEmptySet(elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = {
    anotherSet match {
      case NoEmptySet(anotherHead, anotherTail) =>  this + anotherHead ++ anotherTail
      case _ => this
    }
  }

  override def map[B](f: A => B): MySet[B] = NoEmptySet(f(head), tail.map(f))

  override def flatMap[B](f: A => MySet[B]): MySet[B] = f(head) ++ tail.flatMap(f)

  override def filter(predicate: A => Boolean): MySet[A] = {
    if (!predicate(head)) tail.filter(predicate)
    else NoEmptySet(head, tail.filter(predicate))
  }

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def -(elem: A): MySet[A] = filter(_ != elem)

  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet.contains(_))

  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet.contains)

  def unary_! : MySet[A] = new PropertyBasedSet[A](!contains(_))
}

object MySet {
  /*
    val s = MySet(1,2,3) = buildSet(seq(1,2,3), [])
    = buildSet(seq(2,3), [] + 1)
    = buildSet(seq(3), [1] + 2)
    = buildSet(seq(), [1, 2] + 3)
    = [1,2,3]
   */
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)

    buildSet(values.toSeq, new EmptySet[A])
  }
}

// all elements of type A which satisfy a property
// { x in A | property(x) }
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {
  override def contains(elem: A): Boolean = property(elem)

  // { x in A | property(x) } + element = { x in A | property(x) || x == element }
  override def +(elem: A): MySet[A] = new PropertyBasedSet[A](x => property(x) || x == elem)

  // { x in A | property(x) } ++ set => { x in A | property(x) || set contains x }
  override def ++(anotherSet: MySet[A]): MySet[A] = new PropertyBasedSet[A](x => property(x) || anotherSet(x))

  override def map[B](f: A => B): MySet[B] = politelyFail

  override def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail

  override def filter(predicate: A => Boolean): MySet[A] = new PropertyBasedSet[A](x => property(x) && predicate(x))

  override def foreach(f: A => Unit): Unit = politelyFail

  override def -(elem: A): MySet[A] = filter(_ != elem)

  override def --(anotherSet: MySet[A]): MySet[A] = filter(x => !anotherSet(x))

  override def &(anotherSet: MySet[A]): MySet[A] = filter(x => anotherSet(x))

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))

  def politelyFail = throw new IllegalArgumentException("Really deep rabbit hole!")
}

object MySetPlayground extends App {
  val s = MySet(1,2,3,4)
  println(s(1))
  println(s(6))
  s + 5 ++ MySet(-1, -2) + 3 flatMap(x => MySet(x, 10 * x)) filter (_ % 2 == 0) foreach println

  val s1 = MySet(1,2,3,4)
  val s2 = MySet(3,4,5,6)
  println("\n Exercise 2")
  println("removing an element")
  s1 - 3 foreach println
  println("difference with another set")
  s1 -- s2 foreach println
  println("intersection with another set")
  s1 & s2 foreach println

  println("\nExercise 3")
  val negative = !s // s.unary_! = all the naturals not equal to 1,2,3,4
  println(negative(2))
  println(negative(5))

  val negativeEven = negative.filter(_ % 2 == 0)
  println(negativeEven(5))

  val negativeEven5 = negativeEven + 5 // all the even numbers > 4 + 5
  println(negativeEven5(5))
}

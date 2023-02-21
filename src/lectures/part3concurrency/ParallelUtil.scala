package lectures.part3concurrency

import java.util.concurrent.atomic.AtomicReference

import scala.collection.parallel.{ForkJoinTaskSupport, Task, TaskSupport}
import scala.collection.parallel.immutable.ParVector
import scala.concurrent.forkjoin.ForkJoinPool


object ParallelUtil extends App{

  // 1 - parallel collections

  val parList = List(1, 2, 3).par

  val aParVector = ParVector[Int](1, 2, 3)

  /*
    Seq
    Vector
    Array
    Map - Hash, Trie
    Set - Hash, Trie
   */

  def measure[T](operation: => T): Long = {
    val time = System.currentTimeMillis()
    operation
    System.currentTimeMillis() - time
  }

  val list = (1 to 10000).toList // small
  //val list = (1 to 10000000).toList // large

  val serialTime = measure {
    list.map(_ + 1)
  }
  println("serial time: " + serialTime)

  val parallelTime = measure {
    list.par.map(_ + 1)
  }

  println("parallel time: " + parallelTime)

  /*
    Map-reduce model
    - split the elements into chunks - Splitter
    - operation
    - recombine - Combiner
   */

  // map, flatMap, filter, foreach, reduce, fold

  // fold, reduce with non-associative operators
  println(List(1, 2, 3).reduce(_ - _))
  println(List(1, 2, 3).par.reduce(_ - _))

  // synchronization
  var sum = 0
  List(1, 2, 3).par.foreach(sum += _)
  println(sum) // race conditions!

  // configuring
  aParVector.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(2))
  /*
    alternatives
    - ThreadPoolTaskSupport - deprecated
    - ExecutionContextTaskSupport(EC)
   */

  //  aParVector.tasksupport = new TaskSupport {
  //    override def execute[R, Tp](fjtask: Task[R, Tp]): () => R = ???
  //
  //    override def executeAndWaitResult[R, Tp](task: Task[R, Tp]): R = ???
  //
  //    override def parallelismLevel: Int = ???
  //
  //    override val environment: AnyRef = ???
  //  }

  // 2 - atomic ops and references
  println("\n atomic")

  val atomic = new AtomicReference[Int](2)

  val currentValue = atomic.get() // thread-safe read
  atomic.set(4) // thread-safe write

  println(atomic.getAndSet(5)) // thread safe combo

  println(atomic.get())

  atomic.compareAndSet(5, 56)
  // if the value is 5, then set to 56
  println(atomic.get())
  // reference equality

  println(atomic.updateAndGet(_ + 1)) // thread-safe function run
  println(atomic.getAndUpdate(_ + 1))

  println(atomic.accumulateAndGet(10, _ + _)) // thread-safe accumulation
  println(atomic.getAndAccumulate(10, _ + _))
  println(atomic.get())
}
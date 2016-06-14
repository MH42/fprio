package de.bstudios.prioritization

import scala.collection.mutable

// Some helpers
trait Helpers {

  implicit def liftOps[T](f: T => Int): OrderingOps[T] = new OrderingOps(f)

  implicit def lift[T](f: T => Int): Ordering[T] = new Ordering[T] {
    // compares x with y.
    //   returns 0 if equal
    //   returns < 0 iff x < y
    //   returns > 0 iff x > y
    def compare(x: T, y: T) = f(x) - f(y)
  }

  implicit class OrderingOps[T](self: Ordering[T]) {
    def ~>(other: Ordering[T]) = chainOrdering(self, other)
  }

  def chainOrdering[T](first: Ordering[T], second: Ordering[T]) = new Ordering[T] {
    def compare(x: T, y: T): Int = first.compare(x, y) match {
      case 0 => second.compare(x, y)
      case r => r
    }
  }

  def memo[R, T](f: R => T): R => T = {
    lazy val cache = mutable.WeakHashMap.empty[R, T]
    in => cache.getOrElseUpdate(in, f(in))
  }

  def showTable[T](t: Map[(String, String),T]): String = {
    val rows = t.keys.map(_._1)
    val cols = t.keys.map(_._2)

    def showRow[T](row: String): String =
      cols.map(col => t.getOrElse((row, col), "-")).mkString(" | ")

    "   " + (cols mkString " | ") + "\n" +
    rows.map(row => s"$row |" + showRow(row)).mkString("\n")
  }

  def fromText(s: String): List[String] = (s split "\n").toList

  def setUpFor(t: String): String = {
    val test = (t split "_").head
    s"${test}_setUp"
  }

  def tearDownFor(t: String): String = {
    val test = (t split "_").head
    s"${test}_tearDown"
  }

  def withTime[T](block: => T): (T, Long) = {
    val before = System.currentTimeMillis
    val result = block
    val after = System.currentTimeMillis
    (result, after - before)
  }
}

object helpersTest extends App with Helpers {
  val fst: List[Int] => Int = _(0)
  val snd: List[Int] => Int = _(1)
  val trd: List[Int] => Int = _(2)
  val fth: List[Int] => Int = _(3)

  val all = fst ~> snd ~> trd ~> fth

  assert(all.compare(List(1, 2, 3, 4), List(1, 2, 3, 4)) == 0)
  assert(all.compare(List(1, 2, 3, 4), List(2, 2, 3, 4)) < 0)
  assert(all.compare(List(1, 2, 3, 4), List(0, 2, 3, 4)) > 0)
  assert(all.compare(List(1, 2, 3, 4), List(1, 3, 3, 4)) < 0)
  assert(all.compare(List(1, 2, 3, 4), List(1, 1, 3, 4)) > 0)
  assert(all.compare(List(1, 2, 3, 4), List(1, 2, 4, 4)) < 0)
  assert(all.compare(List(1, 2, 3, 4), List(1, 2, 1, 4)) > 0)
  assert(all.compare(List(1, 2, 3, 4), List(1, 2, 3, 5)) < 0)
  assert(all.compare(List(1, 2, 3, 4), List(1, 2, 3, 2)) > 0)
}

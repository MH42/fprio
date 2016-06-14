package de.bstudios.prioritization

trait Eval { self: Algorithms =>

  // for every bug: Set of tests that reveal that bug
  def bugs: Seq[Set[T]]

  private def testExecutesCid(t: T, c: C): Boolean = f(t, c) > 0

  private def findChangesForTest(t: T): Seq[C] =
    codeΔ.toSeq filter (c => testExecutesCid(t, c))

  // helper method to infer bugs
  def findBugsFor(failingTests: Set[T]): Seq[Set[T]] = {
    val allExecutedChanges = failingTests.flatMap(findChangesForTest)
    allExecutedChanges.toSeq.map { c =>
      tests.toSet.filter { t => testExecutesCid(t, c) }
    }	
  }

  lazy val revealing = bugs.flatten

  private def isRevealing(t: T): Boolean = revealing contains t

  private def reveals(t: T): Seq[Int] =
    for {
      (ts, i) <- bugs.zipWithIndex
      if ts contains t
    } yield (i + 1)

  private def indexOfFirstRevealing(revealing: Set[T], ordering: Seq[T]) =
    ordering.zipWithIndex.filter {
      case (t, i) => revealing contains t
    } map {
      case (t, i) => i + 1
    } min

  def score(ordering: Seq[T]): Double = {
    // find indices for revealing tests
//    println(bugs.size)
//    println(tests.size)
    (1.0d - (bugs.map(rt => indexOfFirstRevealing(rt, ordering)).sum.toDouble / (bugs.size * tests.size).toDouble) + (1 / (2d * tests.size.toDouble))) * 100
  }

  def printOrdering(name: String, tests: => Seq[T], pc1: T => Int) = {
    val (ts, time) = withTime(tests)
    val renderedTests = ts map {
      case t => reveals(t) match {
        case Seq() => s"$t (${pc1(t)})"
        case revs  => s"""$t (${pc1(t)}) (reveals: ${revs mkString ", "})"""
      }
    } mkString "\n"

    println("number of tests: "+tests.size)
    s"---$name---\n$renderedTests\nscore: ${score(ts)}\ntime: ${time}ms\n"
  }
}

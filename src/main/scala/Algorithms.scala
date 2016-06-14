package de.bstudios.prioritization

trait Algorithms extends Helpers { outer =>

  // Test
  type T

  // Code-Element
  type C

  // List of all tests
  def tests: Iterable[T]

  // List of all code-elements
  def code: Iterable[C]

  // List of modified code-elements
  def codeΔ: Iterable[C]

  // the table of code change and test to frequency
  def f: (T, C) => Int

  def cidsForTest: T => Seq[C]

  // sum of all frequencies for a test (global maximum), not used...
  lazy val Σ: T => Int = memo { t => code.map { f(t, _) }.sum }

  // Sum of change frequencies for a test
  lazy val ΣΔ: T => Int = memo { t => codeΔ.map { f(t, _) }.sum }

  // Count of code-entities, covered by a test, should be optimized and immediately be given.
  lazy val count: T => Int = memo { t =>
    code.filter { f(t, _) > 0 }.size
  }

  // Count of changes, covered by a test
  lazy val countΔ: T => Int = memo { t => codeΔ.filter { f(t, _) > 0 }.size }

  // Highest frequency for a code-element
  lazy val sup: C => Int = memo { c => tests.map { f(_, c) }.max }

  // Frequency of highest win, if any (0 otherwise)
  val highestWinΔ : T => Int = memo { t =>
    codeΔ.map { c => if (f(t, c) == sup(c)) f(t, c) else 0 }.max
  }

  // Total count of code-changes where the test wins
  def countWinΔ : T => Int = memo { t =>
    codeΔ.filter { c => f(t, c) == sup(c) }.size
  }

  // somewhat random, but stable to ordering
  def random: T => Int = _.hashCode

  lazy val GFP: Ordering[T] = ΣΔ          ~> countΔ      ~> count ~> random
  lazy val LFP: Ordering[T] = highestWinΔ ~> countΔ      ~> count ~> random
  lazy val CFP: Ordering[T] = countΔ      ~> ΣΔ          ~> count ~> random

  // 1. Winning tests
  // 2. Broad spectrum of touched code (structural coverage)
  // 3. often executed changes
  // 4. Random
  lazy val JFP: Ordering[T] = countWinΔ   ~> count       ~> ΣΔ    ~> random

  lazy val CFP2: Ordering[T] = countΔ     ~> highestWinΔ ~> count ~> random

  def prio(ord: Ordering[T]): Seq[T] = tests.toSeq.sorted(ord.reverse)

  def dynamic(
      ord: Algorithms { type T = outer.T } => Ordering[T],
      revealingTests: Seq[T],
      groups: Set[Set[T]] = Set.empty[Set[T]]): Seq[T] =
    Dynamic.run[T, C](outer, ord, revealingTests, groups)
}


object test extends Algorithms with App { self =>

  type T = String
  type C = String

  type Table[Row, Col, Values] = Map[Col, Map[Row, Values]]

  val table: Table[C, T, Int] = Map(
    "t1" -> Map("c1" -> 5, "c2" -> 2, "c3" -> 0, "c4" -> 0, "c5" -> 0, "c6" -> 3, "c7" -> 1),
    "t2" -> Map("c1" -> 2, "c2" -> 1, "c3" -> 3, "c4" -> 0, "c5" -> 0, "c6" -> 2, "c7" -> 2),
    "t3" -> Map("c1" -> 0, "c2" -> 0, "c3" -> 0, "c4" -> 7, "c5" -> 0, "c6" -> 0, "c7" -> 0),
    // this is a bit different from the paper example to also use PC 3 in GFP and CFP
    "t4" -> Map("c1" -> 0, "c2" -> 2, "c3" -> 0, "c4" -> 1, "c5" -> 3, "c6" -> 1, "c7" -> 0)
  )

  val tests = table.keys
  val codeΔ = Seq("c1", "c2", "c3", "c4", "c5")
  val code = codeΔ ++ Seq("c6", "c7")

  val cidsForTest: T => Seq[C] = t => table.map {
    case (t_i, freqs) => t_i -> freqs.filter{
      case (c_i, freq) => freq > 0 && (codeΔ contains c_i)
    }.keys
  }.getOrElse(t, Nil).toSeq


  assert(cidsForTest("t1").toSet == Set("c1", "c2"))

  val revealing = Seq("t1", "t3")

  def f: (T, C) => Int = { case (t, c) => table(t)(c) }


  // lazy val GFP: Ordering[T] = ΣΔ          ~> countΔ      ~> count ~> random
  // lazy val LFP: Ordering[T] = highestWinΔ ~> countΔ      ~> count ~> random
  // lazy val CFP: Ordering[T] = countΔ      ~> ΣΔ          ~> count ~> random

  assert(prio(GFP) == Seq("t1", "t3", "t2", "t4"))
  assert(prio(LFP) == Seq("t3", "t1", "t2", "t4"))
  assert(prio(CFP) == Seq("t2", "t4", "t1", "t3"))
  assert(prio(JFP) == Seq("t1", "t4", "t2", "t3"))

  //     c1Δ c2Δ c3Δ c4Δ c5Δ c6  c7  | ΣΔ | countΔ | count |
  // t1: 5   2   0   0   0   3   1   |  7 |      2 |     4 |
  // t2: 2   1   3   0   0   2   2   |  6 |      3 |     5 |
  // t3: 0   0   0   7   0   0   0   |  7 |      1 |     1 |
  // t4: 0   2   0   1   3   1   0   |  6 |      3 |     4 |
  // -> t1, [t3, t2, t4]

  //     c1Δ c2Δ c3Δ c4Δ c5Δ c6  c7  | ΣΔ | countΔ | count |
  // t2: 0   0   3   0   0   2   2   |  3 |      1 |     3 |
  // t3: 0   0   0   7   0   0   0   |  7 |      1 |     1 |
  // t4: 0   0   0   1   3   1   0   |  4 |      2 |     3 |
  // -> t1, t3, t4, t2
  assert(dynamic(_.GFP, revealing, Set(Set("t1", "t2"))) ==
    Seq("t1", "t3", "t4", "t2"))

  assert(dynamic(_.GFP, Seq("t1"), Set(Set())) ==
    Seq("t1", "t3", "t4", "t2"))
  assert(dynamic(_.GFP, Seq("t1"), Set(Set("t1", "t4"))) ==
    Seq("t1", "t3", "t2", "t4"))
  assert(dynamic(_.GFP, Seq("t3"), Set(Set("t3", "t4"))) ==
    Seq("t1", "t3", "t2", "t4"))
  assert(dynamic(_.GFP, Seq("t1"), Set(Set("t1", "t3", "t4"))) ==
    Seq("t1", "t2", "t3", "t4"))
}


object figureX extends Algorithms with App { self =>

  type T = String
  type C = String

  type Table[Row, Col, Values] = Map[Col, Map[Row, Values]]

  //    c1 c2 c3 c4 c5  c6 c7    sumΔ  countΔ   winΔ   count
  // t1  4  0  4  2  0 | 0  1 ||  10 |     3 |    4  |    4
  // t2  0  4  3  0  1 | 7  0 ||   8 |     3 |    0  |    4
  // t3  2  1  4  0  2 | 2  1 ||   9 |     4 |    4  |    6
  // t4  2  5  0  1  0 | 0  0 ||   8 |     3 |    5  |    3

  val table: Table[C, T, Int] = Map(
    "t1" -> Map("c1" -> 4,  "c2" -> 0, "c3" -> 4, "c4" -> 2, "c5" -> 0, "c6" -> 0, "c7" -> 1),
    "t2" -> Map("c1" -> 0,  "c2" -> 4, "c3" -> 3, "c4" -> 0, "c5" -> 1, "c6" -> 7, "c7" -> 0),
    "t3" -> Map("c1" -> 2,  "c2" -> 1, "c3" -> 4, "c4" -> 0, "c5" -> 2, "c6" -> 2, "c7" -> 1),
    "t4" -> Map("c1" -> 2,  "c2" -> 5, "c3" -> 0, "c4" -> 1, "c5" -> 0, "c6" -> 0, "c7" -> 0)
  )

  val tests = table.keys
  val codeΔ = Seq("c1", "c2", "c3", "c4", "c5")
  val code = codeΔ ++ Seq("c6", "c7")

  val cidsForTest: T => Seq[C] = t => table.map {
    case (t_i, freqs) => t_i -> freqs.filter{
      case (c_i, freq) => freq > 0 && (codeΔ contains c_i)
    }.keys
  }.getOrElse(t, Nil).toSeq

  val revealing = Seq("t1")

  def f: (T, C) => Int = { case (t, c) => table(t)(c) }


  // lazy val GFP: Ordering[T] = ΣΔ          ~> countΔ      ~> count ~> random
  // lazy val LFP: Ordering[T] = highestWinΔ ~> countΔ      ~> count ~> random
  // lazy val CFP: Ordering[T] = countΔ      ~> ΣΔ          ~> count ~> random

  println(s"t1: ", ΣΔ("t1"), countΔ("t1"), count("t1"), highestWinΔ("t1"))
  println(s"t2: ", ΣΔ("t2"), countΔ("t2"), count("t2"), highestWinΔ("t2"))
  println(s"t3: ", ΣΔ("t3"), countΔ("t3"), count("t3"), highestWinΔ("t3"))
  println(s"t4: ", ΣΔ("t4"), countΔ("t4"), count("t4"), highestWinΔ("t4"))

  println(s"GFP: ${prio(GFP)}")
  println(s"LFP: ${prio(LFP)}")
  println(s"CFP: ${prio(CFP)}")

  println("\nDynamic with t1 as revealing test:")
  println(s"""DGFP: ${dynamic(_.GFP, Seq("t1"))}""")
  println(s"""DLFP: ${dynamic(_.LFP, Seq("t1"))}""")
  println(s"""DCFP: ${dynamic(_.CFP, Seq("t1"))}""")

  println("\nDynamic with t2 as revealing test:")
  println(s"""DGFP: ${dynamic(_.GFP, Seq("t2"))}""")
  println(s"""DLFP: ${dynamic(_.LFP, Seq("t2"))}""")
  println(s"""DCFP: ${dynamic(_.CFP, Seq("t2"))}""")

  println("\nDynamic with t3 as revealing test:")
  println(s"""DGFP: ${dynamic(_.GFP, Seq("t3"))}""")
  println(s"""DLFP: ${dynamic(_.LFP, Seq("t3"))}""")
  println(s"""DCFP: ${dynamic(_.CFP, Seq("t3"))}""")

  println("\nDynamic with t4 as revealing test:")
  println(s"""DGFP: ${dynamic(_.GFP, Seq("t4"))}""")
  println(s"""DLFP: ${dynamic(_.LFP, Seq("t4"))}""")
  println(s"""DCFP: ${dynamic(_.CFP, Seq("t4"))}""")

  assert(prio(GFP) == Seq("t1", "t3", "t2", "t4"))
  assert(prio(LFP) == Seq("t4", "t3", "t1", "t2"))
  assert(prio(CFP) == Seq("t3", "t1", "t2", "t4"))
}

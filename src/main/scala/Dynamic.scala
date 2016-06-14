package de.bstudios.prioritization

// Instances of this class represent the Algorithm based on a pruned table
case class Dynamic[TT, CC](
  // The original sorting algorithms
  original: Algorithms { type T = TT; type C = CC },
  // only include those candidates
  candidates: Seq[TT] = Nil,
  // exclude those CIDs from the table
  excludedCID: Set[CC] = Set.empty[TT]) extends Algorithms { outer =>

  type T = TT
  type C = CC

  lazy val tests: Iterable[T] = candidates
  lazy val codeΔ: Iterable[C] = original.codeΔ.filterNot(excludedCID.contains)
  def f = original.f

  // Since original count might be optimized for CID count we only compute
  // the incremental delta.
  lazy val code: Iterable[C] = ???
  def cidsForTest = original.cidsForTest

  override lazy val count: T => Int = memo { t =>
    val excludedCount = cidsForTest(t).filter(excludedCID.contains).size
    original.count(t) - excludedCount
  }

  // TODO this is very inefficient
  def abort: Boolean = codeΔ.isEmpty || tests.forall { t =>
    !codeΔ.exists { c => f(t, c) > 0 }
  }

  def success: T => Dynamic[T, C] = t =>
    Dynamic(original, candidates filterNot (_ == t), excludedCID)

  def fail: T => Dynamic[T, C] = t =>
    Dynamic(original, candidates filterNot (_ == t), cidsForTest(t).toSet ++ excludedCID)

  def apply(ord: Algorithms { type T = outer.T } => Ordering[T]): Seq[T] =
    prio(ord(outer))
}

object Dynamic {
  def run[TT, CC](
    alg: Algorithms { type T = TT; type C = CC },
    ord: Algorithms { type T = TT } => Ordering[TT],
    revealing: Seq[TT],
    // test groups, if one test fails all tests fail
    groups: Set[Set[TT]] = Set.empty[Set[TT]]
  ): Seq[TT] = {

    // this fails the test, but let's a potential corresponding group succeed
    def failTest(t: TT, d: Dynamic[TT, CC]): Dynamic[TT, CC] =
      groupFor(t).foldLeft(d fail t)(_ success _)

    def appendGroup(d: Dynamic[TT, CC], t: TT): Seq[TT] = {
      val group = groupFor(t)
      d(ord) filter (group contains _)
    }

    def groupFor(t: TT): Set[TT] =
      groups.filter(_ contains t).flatten - t

    def process(d: Dynamic[TT, CC]): Seq[TT] = {

      d(ord) match {
        case Seq() => Seq()
        // we just look at the first test
        case ts if revealing contains ts.head =>
          val d2 = failTest(ts.head, d)
          if (d2.abort)
            d(ord)
          else
            ts.head +: (process(d2) ++ appendGroup(d, ts.head))

        case ts => ts.head +: process(d success ts.head)
      }
    }
    process(Dynamic(alg, alg.tests.toSeq))
  }
}

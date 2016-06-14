package de.bstudios.prioritization

import scalikejdbc._

// The `count` criterion is the only one using unchanged CIDs.
// Since there are so many more CIDs that are not changed, we can optimize their
// usage by letting the DB evaluate the criterion. Hence `code` is never to be
// used and can stay undefined!
trait OptimizedAlgo extends Algorithms {

  def code = sys error "This is never to be used directly by any criterion!"
  def countFromDB: Map[T, Int]

  override lazy val count = countFromDB
}

trait DBAlgo extends OptimizedAlgo with Eval {
  val before = System.currentTimeMillis

  type T = String
  type C = String

  def db: String
  def user: String
  def pw: String

  def logs_table: SQLSyntax
  def seeded_table: SQLSyntax

  Class.forName("com.mysql.jdbc.Driver")
  ConnectionPool.singleton(s"jdbc:mysql://localhost/$db", user, pw)
  implicit lazy val session = AutoSession

  private val granularity = sqls"""
      ($seeded_table.statement = 1
        or
      $seeded_table.field_decl = 1
        or
      $seeded_table.method_decl = 1)
    """

  val testnames = sql"""
      select distinct(testname) from $logs_table where
        testname not like '%UnknownTest%' and
        testname not like '%UndefinedTest%' and
        testname not like '%_setUp%' and
        testname not like '%_tearDown%' and
        testname not like '%_static%';
    """.map(_.string("testname")).list.apply();

  val countFromDB = sql"""
      select testname, count(distinct cid) as freq from $logs_table group by testname;
    """.map(rs => rs.string("testname") -> rs.int("freq")).list.apply().toMap;

  val codeΔ = sql"""
      select distinct(cid) from $seeded_table where cid<>"null" and $granularity;
    """.map(_.string("cid")).list.apply();


  val table = sql"""
      select testname, $seeded_table.cid, sum(amount) as freq
        from $logs_table
        inner join $seeded_table
        on $seeded_table.cid = $logs_table.cid collate latin1_general_ci
        where $granularity
        group by testname, $seeded_table.cid;
    """.map(rs => (rs.string("testname"), rs.string("cid")) -> rs.int("freq")).list.apply().toMap

  // TODO refactor this. It is horrible
  private lazy val allCidsForTests = sql"""
      select $logs_table.testname, $logs_table.cid from $logs_table
    """.map(rs => (rs.string("testname"), rs.string("cid")))
       .list.apply()
       .groupBy(_._1)
       .mapValues(_.map(_._2))

  lazy val cidsForTest: T => Seq[C] = t => codeΔ.filter(f(t, _) > 0)
  //allCidsForTests(t).distinct

  // All cids for a test that this tests executes AND that
  // correspond to a code change.
  // lazy val cidsForTest = memo { (t: T) => sql"""
  //   select distinct($seeded_table.cid)
  //     from $logs_table
  //     inner join $seeded_table
  //     on $seeded_table.cid=$logs_table.cid
  //   where testname = $t and ($seeded_table.statement = 1
  //     or
  //   $seeded_table.field_decl = 1
  //     or
  //   $seeded_table.method_decl = 1);
  // """.map(rs => rs.string("cid")).list.apply().toSeq  }

  def tests = testnames

  // also add setup and teardown
  // Be careful: Teardown and Setup methods will count twice due to
  // stripping and reattaching in tearDownFor etc.
  def f = (t, c) => {
    val setup      = table.getOrElse((setUpFor(t), c), 0)
    val teardown   = table.getOrElse((tearDownFor(t), c), 0)
    val testItself = table.getOrElse((t, c), 0)
    setup + teardown + testItself
  }

  // todo sort by name
  override def random: T => Int = t => {
    0
  }

  println(printOrdering("GFP", prio(GFP), ΣΔ))
  //println(printOrdering("LFP", prio(LFP), highestWinΔ))
  //println(printOrdering("CFP", prio(CFP), countΔ))
  //println(printOrdering("DGFP", dynamic(_.GFP, revealing), ΣΔ))
  //println(printOrdering("DLFP", dynamic(_.LFP, revealing), highestWinΔ))
  //println(printOrdering("DCFP", dynamic(_.CFP, revealing), countΔ))
  println("total time: "+(System.currentTimeMillis-before))
}

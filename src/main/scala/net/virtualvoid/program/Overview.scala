package net.virtualvoid.program

object Overview extends App {
  import ProblemRepository._

  val top50 = annotated.filter(_._2 >= 0).take(50)
  top50.foreach(t ⇒ println(t._1.id + " " + t._2))

  println()
  println(f"With folds: ${withFolds.size}%5d Without folds: ${withoutFolds.size}")
  println(s"Easier than 10 mio: $easierThan10Million")
  println(s"Total: $total Solved: $solved Unsolved: $unsolved Failed: $failed")
}

object ProblemRepository {
  val allProblems = Main.loadProblems()

  def total = allProblems.size
  def solved = allProblems.count(_.solved.exists(identity))
  def unsolved = allProblems.count(_.solved.isEmpty)
  def failed = allProblems.count(_.solved.exists(!_))

  val problems = allProblems.filter(_.canBeSolved)
  val (withFolds, withoutFolds) = problems.partition(_.hasFold)
  val annotatedWithOverflowed = problems.map(annotateDifficulty).sortBy(_._2)
  val annotated = annotatedWithOverflowed.filter(_._2 > 0)

  val easierThan10Million = annotated.count(_._2 < 10000000)

  def annotateDifficulty(p: Problem) = p -> p.numSolutions
}

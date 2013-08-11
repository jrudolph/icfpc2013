package net.virtualvoid.program

import scala.concurrent.Future

object Overview extends App {
  import ProblemRepository._
  import Client.system.dispatcher

  val reload = if (false) Main.reloadProblems() else Future.successful(None)

  reload.foreach { _ ⇒
    val top50 = annotated.filter(_._2 >= 0).take(50)
    top50.foreach { t ⇒
      import t._1._
      println(f"${t._2}%15d $isBonus%5s $size%2d $id")
    }

    println("Histo, 2 ^ x | n | cumulative")
    var cum = 0L
    var cumT = 0d
    (0 until 62).foreach { i ⇒
      val l = Math.pow(2, i - 1)
      val p = Math.pow(2, i)
      val num = annotated.count(x ⇒ x._2 >= l && x._2 < p)
      cum += num
      cumT += num.toLong * p
      val dur = cumT.toDouble / 2000000 / 3600 // 2 mio ops per sec
      if (num > 0) println(f"2^$i%2d $num%4d $cum%4d $dur%6.2f h")
    }
    println()

    println(f"With folds: ${withFolds.size}%5d")
    println(f"Without folds: ${withoutFolds.size}")

    println("With if0: " + problems.count(_.hasIf0))
    println("Without hard: " + problems.count(_.hasNoIf0AndFold))
    println("With plus: " + problems.count(_.hasPlus))

    println(s"Easier than 150 mio: $easierThan10Million")
    println("Out of long range: " + annotated.count(_._2 < 0))

    println("Bonus: " + problems.count(_.isBonus))
    println(s"Total: $total Solved: $solved Unsolved: $unsolved Failed: $failed")

    Client.shutdown()
  }
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

  val easierThan10Million = annotated.count(_._2 < 5000000000L)

  def annotateDifficulty(p: Problem) = p -> p.numSolutions
}

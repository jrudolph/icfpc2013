package net.virtualvoid.program

import java.io.File
import scala.io.Source

object PostMortem extends App {
  val ResultLine =
    """Result of guess Guess\(([^,]+),(.*)\) is Success\(GuessResponse\(win,None,None,Some\((false|true)\)\)\)""".r

  def log2(d: Double): Double = math.log(d) / math.log(2)

  val winLog = new File(Main.logFolder, "win.log")
  require(winLog.exists())

  case class OpsInfo(operators: Seq[String], unOps: Int, binOps: Int, if0: Boolean, tfold: Boolean, fold: Boolean, bonus: Boolean) {
    override def toString = "%s%1d%1d%1s%1s%1s" format (
      flag('b, bonus), unOps, binOps, flag('i, if0), flag('t, tfold), flag('f, fold))

    def flag(name: Symbol, value: Boolean) = if (value) name.name else "-"
  }

  def opsInfo(ops: Seq[String]) =
    OpsInfo(
      operators = ops,
      unOps = ops.count(Synthesis.unops),
      binOps = ops.count(Synthesis.binops),
      if0 = ops.contains("if0"),
      tfold = ops.contains("tfold"),
      fold = ops.contains("fold"),
      bonus = ops.contains("bonus"))

  case class WinInfo(id: String, program: Program, lightning: Boolean, problem: Option[Problem], examples: Seq[PositiveExample]) {
    def isTraining = problem.isEmpty
    def isBonus = problem.exists(_.isBonus)

    lazy val size = problem.get.size
    lazy val ops = opsInfo(problem.get.operators)
    lazy val difficulty = cleanDiff(problem.get.numSolutions)

    lazy val solutionSize = Metadata.size(program)
    lazy val solutionOps = opsInfo(Metadata.operators(program))
    lazy val solutionDifficulty = difficulty(solutionSize, solutionOps)

    lazy val simplified = Interpreter.simplify(program)
    lazy val simplifiedSize = Metadata.size(simplified)
    lazy val simplifiedOps = opsInfo(Metadata.operators(simplified))
    lazy val simplifiedDifficulty = difficulty(simplifiedSize, simplifiedOps)

    def numExamples = examples.size

    def difficulty(size: Int, ops: OpsInfo): Long = cleanDiff {
      if (isBonus) Synthesis.numSolutionsBonus(Problem("", size, ops.operators, None, None))
      else Synthesis.numSolutionsNew(size, ops.operators: _*)
    }

    cleanDiff(Synthesis.numSolutionsNew(simplifiedSize, simplifiedOps.operators: _*))
    def cleanDiff(sol: Long) = log2(if (sol > 0) sol else Long.MaxValue).toInt

    require(examples.forall(_.matchesProgram(program)), "Examples don't match program for " + id)
    require(examples.forall(_.matchesProgram(simplified)),
      s"Examples don't match simplified program ($simplified) for $id ($program)")
  }

  val problemsById = ProblemRepository.allProblems.map(x ⇒ (x.id, x)).toMap

  val winsWithTraining =
    Source.fromFile(winLog).getLines().map {
      case ResultLine(id, prog, light) ⇒
        WinInfo(id, prog, light == "true", problemsById.get(id), examplesFor(id))
    }.toIndexedSeq

  val wins = winsWithTraining.filterNot(_.isTraining)
  val (bonusWins, normalWins) = wins.partition(_.isBonus)

  def examplesFor(id: String): Seq[PositiveExample] =
    Seq(new File(Main.logFolder, s"examples/p-$id.log"),
      new File(Main.logFolder, s"examples2/p-$id.log")).flatMap(examplesFromFile)

  lazy val ExampleLine =
    """Example\(0x([a-f0-9A-F]+)L,0x([a-f0-9A-F]+)L\)""".r
  def examplesFromFile(f: File): Seq[PositiveExample] =
    if (!f.exists()) Nil
    else Source.fromFile(f).getLines().toIndexedSeq.flatMap {
      case null ⇒ None
      case ExampleLine(input, output) ⇒
        Some(PositiveExample(parseLong(input), parseLong(output)))
    }

  lazy val lightningProblems = wins.filter(_.lightning)

  def analysis(wins: Seq[WinInfo]): Unit = {
    val num = wins.size
    val byDifficulty = wins.sortBy(x ⇒ if (x.difficulty > 0) x.difficulty else Long.MaxValue)

    def info(win: WinInfo) = {
      import win._
      f"""problem $difficulty%2d [$size%2d|$ops%s] solution $solutionDifficulty%2d [$solutionSize%2d|$solutionOps] simplified $simplifiedDifficulty%2d [$simplifiedSize%2d|$simplifiedOps] $simplified"""
    }

    println(f"$num%3d Problems solved")

    def avg(wins: Seq[WinInfo])(by: WinInfo ⇒ Double) = wins.map(by).sum / wins.size

    println(f"Average problem size: ${avg(wins)(_.size)}%5.2f")
    println(f"Average solution size: ${avg(wins)(_.solutionSize)}%5.2f")
    println(f"Average simplified solution size: ${avg(wins)(_.simplifiedSize)}%5.2f")
    println()
    val top10 = wins.groupBy(_.simplified).mapValues(_.size).toSeq.sortBy(-_._2).take(10)

    println(f"Top 10 solutions (making up for ${top10.map(_._2).sum.toDouble / num * 100}%5.2f %% of all in this category)")
    println()
    top10.map {
      case (prog, number) ⇒ f"$number%3d $prog"
    }.foreach(println)
    println()

    byDifficulty.map(info).foreach(println)
    println()

    println("Size histogram")
    wins.groupBy(_.size).mapValues(x ⇒ (x.size, avg(x)(_.simplifiedDifficulty))).toSeq.sortBy(_._1).foreach {
      case (size, (number, avgDiff)) ⇒ println(f"$size%2d $number%3d $avgDiff%5.2f")
    }

    println()

    val avgDiff = avg(byDifficulty)(_.difficulty)
    val avgSolDiff = avg(byDifficulty)(_.simplifiedDifficulty)

    println(f"Average log2 - problem difficulty $avgDiff%5.2f")
    println(f"Average log2 - solution difficulty $avgSolDiff%5.2f")
  }

  println("All solved problems analysis")
  println()
  analysis(wins)

  println()
  println("Lightning problems analysis")
  println()
  analysis(lightningProblems)

  println()
  println("Normal problems analysis")
  println()
  analysis(normalWins)

  println()
  println("Bonus problems analysis")
  println()
  analysis(bonusWins)
}

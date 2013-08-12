package net.virtualvoid.program

import java.io.{ FileWriter, File }
import scala.io.Source
import scala.concurrent.Future
import scala.util.{ Success, Try, Failure }

object Main {
  import Client.system.dispatcher

  val logFolder = new File("log")
  if (!logFolder.exists()) logFolder.mkdirs()
  val exampleFolder = new File("log/examples")
  if (!exampleFolder.exists()) exampleFolder.mkdirs()
  val log = new File(logFolder, "all.log")
  val logStream = new FileWriter(log, true)
  def allProblemLogs = (0 to 1000).map(i ⇒ new File(logFolder, f"problem$i%05d.log"))
  def nextProblemLog() =
    new FileWriter(allProblemLogs.find(!_.exists()).get)

  def problemLog(id: String) =
    new FileWriter(new File(exampleFolder, s"p-$id.log"), true)

  def println(s: String) = {
    Console.out.println(s)
    logStream.append(s)
    logStream.append("\n")
    logStream.flush()
  }

  def reloadProblems() =
    Client.newProblem().map { ps ⇒
      println(s"Got ${ps.size} problems: $ps")
      val thisLog = nextProblemLog()
      ps.foreach(p ⇒ thisLog.append(renderProblem(p) + "\n"))
      thisLog.close()
    }

  def trySolvingOneProblem(c: Problem, num: Int = 5, examples: Seq[PositiveExample] = Nil): Future[(Seq[PositiveExample], GuessResponse)] = {
    val myLog = problemLog(c.id)
    println(s"Selected $c of difficulty ${c.numSolutions}")
    if (examples.isEmpty) println(s"Fetching $num examples...") else println(s"Trying with ${examples.size} examples")
    val exampleF =
      if (examples.isEmpty) Client.fetchExamples(c.id, num = num)
      else Future.successful(examples)

    exampleF.flatMap { exs ⇒
      println(s"Got ${exs.size} examples: ${exs.mkString("\n")}")
      exs.foreach { e ⇒
        myLog.append("Example(")
        myLog.append(e.input.formatted("0x%XL"))
        myLog.append(',')
        myLog.append(e.output.formatted("0x%XL"))
        myLog.append(")\n")
      }
      myLog.close()

      println("Running solver...")
      val result = Synthesis.findMatching(c, exs)
      result match {
        case None ⇒
          println("Found no result")
          Future.failed(new RuntimeException(s"Found no result for ${c.id}"))
        case Some(x) ⇒

          println(s"Found '$x'")
          val guess = Guess(c.id, Printer.print(x))

          val res = Client.deliverGuess(guess)
          res.onComplete { res ⇒
            println(s"Result of guess $guess is $res")
          }
          res.map(r ⇒ (exs, r))
      }
    }
  }

  def onFinished(next: Problem, num: Int, rest: List[Problem])(result: Try[(Seq[PositiveExample], GuessResponse)]): Unit = result match {
    case Success((_, GuessResponse("win", _, _, _))) ⇒ trySeveral(rest, num)
    case Success((exs, GuessResponse("mismatch", Some(Seq(input, output, myOutput)), _, _))) ⇒
      if (exs.size < 12)
        trySolvingOneProblem(next, num, examples = exs :+ PositiveExample(Client.parseLong(input.drop(2)), Client.parseLong(output.drop(2))))
          .onComplete(onFinished(next, num, rest))
      else trySeveral(rest, num)
    case Failure(x) ⇒
      println(s"Failure ${x.getMessage}, trying next...")
      trySeveral(rest, num)
  }

  def trySeveral(all: List[Problem], num: Int = 10): Unit = all match {
    case next :: rest ⇒
      if (next.deadline.exists(_.isOverdue())) trySeveral(rest, num)
      else trySolvingOneProblem(next, num = num).onComplete(onFinished(next, num, rest))
    case Nil ⇒ println("Completed all problems!")
  }

  def tryBonus(sizeFilter: Int ⇒ Boolean) = {
    val bonus = ProblemRepository.annotated.filter(_._1.isBonus)
    val selected = bonus.filter(p ⇒ sizeFilter(p._1.size)).sortBy(_._2).take(50)
    val min = selected.minBy(_._2)._2
    val max = selected.maxBy(_._2)._2
    println(s"Selected ${selected.size} problem of difficulties $min to $max")
    trySeveral(selected.map(_._1).toList, num = 6)
  }

  def tryHardest() = {
    val selected =
      ProblemRepository.annotated.filter(x ⇒ !x._1.isBonus && x._1.hasIf0 && !x._1.hasFold)
        .sortBy(-_._2)
        .map(_._1).toList
    println(s"Selected ${selected.size}")
    trySeveral(selected, 6)
  }
  def tryEasiest() = {
    val easiest50 =
      ProblemRepository.problems.sortBy {
        case x if x.isBonus ⇒ x.size >> 32 | (2L << 32)
        case x              ⇒ x.size >> 32 | (1L << 32)
      }.toList

    println(s"Trying a total of ${easiest50.size} problems")
    //ProblemRepository.annotated.filter(x ⇒ !x._1.isBonus && x._1.hasIf0 && !x._1.hasFold).map(_._1).toList

    val (even, odd) = easiest50.zipWithIndex.partition(_._2 % 2 == 0)

    //trySeveral(easiest50, 6)
    Seq(even, odd).foreach(x ⇒ trySeveral(x.map(_._1), 6))
    //trySeveral(easiest50)
  }

  def renderProblem(p: Problem): String = {
    import p._
    s"$id $size $solved $timeLeft ${operators.mkString(" ")}"
  }

  def latestFile = allProblemLogs.reverse.dropWhile(!_.exists()).head
  val SomeP = """Some\(([0-9]+)\)""".r
  def loadProblems(): Seq[Problem] = {
    def loadLine(line: String) = {
      val id :: sizeStr :: solvedStr :: timeLeftStr :: ops = line.split(" ").toList
      val solved: Option[Boolean] = solvedStr match {
        case "None"        ⇒ None
        case "Some(true)"  ⇒ Some(true)
        case "Some(false)" ⇒ Some(false)
      }
      val timeLeft: Option[Int] = timeLeftStr match {
        case "None"   ⇒ None
        case SomeP(p) ⇒ Some(p.toInt)
      }

      Problem(id, sizeStr.toInt, ops, solved, timeLeft)
    }
    val latest = latestFile
    println(s"Loading from $latest")
    Source.fromFile(latest).getLines().toIndexedSeq.map(loadLine)
  }
}

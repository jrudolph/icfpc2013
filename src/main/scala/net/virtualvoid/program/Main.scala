package net.virtualvoid.program

import java.io.{ FileWriter, File }
import scala.io.Source
import scala.concurrent.Future

object Main {
  import Client.system.dispatcher

  val logFolder = new File("log")
  if (!logFolder.exists()) logFolder.mkdirs()
  val exampleFolder = new File("log/examples")
  if (!exampleFolder.exists()) exampleFolder.mkdirs()
  val log = new File(logFolder, "all.log")
  val logStream = new FileWriter(log, true)
  def nextProblemLog() =
    new FileWriter((0 to 1000).map(i ⇒ new File(logFolder, f"problem$i%05d.log")).find(!_.exists()).get)

  def problemLog(id: String) =
    new FileWriter(new File(exampleFolder, s"p-$id.log"), true)

  def println(s: String) = {
    Console.out.println(s)
    logStream.append(s)
    logStream.append("\n")
    logStream.flush()
  }

  def reloadProblems() = {
    Client.newProblem().foreach { ps ⇒
      println(s"Got ${ps.size} problems: $ps")
      val thisLog = nextProblemLog()
      ps.foreach(p ⇒ thisLog.append(renderProblem(p) + "\n"))
      thisLog.close()
    }
  }

  def trySolvingOneProblem(c: Problem, examples: Seq[Example] = Nil): Future[(Seq[Example], GuessResponse)] = {
    val myLog = problemLog(c.id)
    println(s"Selected $c of difficulty ${c.numSolutions}")
    println("Fetching examples...")
    val exampleF =
      if (examples.isEmpty) Client.fetchExamples(c.id)
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
      val result = Synthesis.findMatching(exs, c.size, c.operators: _*)
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

  def tryEasiest() = {
    val easiest50 = ProblemRepository.annotated.take(50).map(_._1).toList

    def onFinished(next: Problem, rest: List[Problem])(result: (Seq[Example], GuessResponse)): Unit = result match {
      case (_, GuessResponse("win", _, _, _)) ⇒ runOne(rest)
      case (exs, GuessResponse("mismatch", Some(Seq(input, output, myOutput)), _, _)) ⇒
        trySolvingOneProblem(next, exs :+ Example(Client.parseLong(input.drop(2)), Client.parseLong(output.drop(2)))).foreach(onFinished(next, rest))
    }

    def runOne(all: List[Problem]): Unit = all match {
      case next :: rest ⇒
        trySolvingOneProblem(next).foreach(onFinished(next, rest))
      case Nil ⇒ println("Completed all problems!")
    }
    runOne(easiest50)
  }

  def renderProblem(p: Problem): String = {
    import p._
    s"$id $size $solved $timeLeft ${operators.mkString(" ")}"
  }

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
    Source.fromFile("log/problem00013.log").getLines().toIndexedSeq.map(loadLine)
  }
}

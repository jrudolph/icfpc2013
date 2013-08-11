package net.virtualvoid.program

object AutoProblemSolverMain extends App {
  val target = args(0).toInt
  println(s"Trying problems of size $target")

  import Client.system.dispatcher
  Main.reloadProblems().onComplete { _ ⇒
    //Main.tryBonus(target ==)
    Main.tryEasiest()
  }
}

object Hardest extends App {
  Main.tryHardest()
}

package net.virtualvoid.program

object AutoProblemSolverMain extends App {
  import Client.system.dispatcher
  Main.reloadProblems().onComplete { _ ⇒
    Main.tryEasiest()
  }
}

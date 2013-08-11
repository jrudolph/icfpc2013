package net.virtualvoid.program

import spray.json.DefaultJsonProtocol

/*
  interface Problem {
    id: string;
    size: number;
    operators: string[];
    solved?: boolean;
    timeLeft?: number
  }
*/
case class Problem(id: String, size: Int, operators: Seq[String], solved: Option[Boolean], timeLeft: Option[Int]) {
  def numSolutions: Long =
    if (isBonus) Synthesis.numSolutionsBonus(this)
    else Synthesis.numSolutionsNew(size, operators: _*)

  override def toString: String =
    s"""Problem("$id", $size, Seq(${operators.map("\"" + _ + "\"").mkString(", ")}), $solved, $timeLeft)"""

  def hasFold: Boolean = operators.exists(_.contains("fold"))
  def hasIf0 = operators.contains("if0")
  def hasNoIf0AndFold = !hasFold && !hasIf0
  def hasPlus = operators.contains("plus")
  def isSolved = solved.exists(identity)
  def canBeSolved: Boolean = !isSolved && timeLeft.forall(_ > 0)
  def isBonus = operators.contains("bonus")
}

/*
  interface EvalRequest {
    id?: string;
    program?: string;
    arguments: string[];
  }
*/
case class EvalRequest(id: Option[String], program: Option[String], arguments: Seq[String])

/*
  interface EvalResponse {
    status: string;
    outputs?: string[];
    message?: string;
  }
   */
case class EvalResponse(status: String, outputs: Option[Seq[String]], message: Option[String])

/*
  interface Guess {
    id: string;
    program: string;
  }
*/
case class Guess(id: String, program: String)

/*
  interface GuessResponse {
    status: string;
    values?: string[];
    message?: string;
    lightning?: bool;
  }
*/
case class GuessResponse(status: String, values: Option[Seq[String]], message: Option[String], lightning: Option[Boolean])

/*
  interface TrainRequest {
    size?: number;
    operators?: string[];
  }
*/
case class TrainRequest(size: Option[Int], operators: Option[Seq[String]])

/*
  interface TrainingProblem {
    challenge: string;
    id: string;
    size: number;
    operators: string[];
  }
*/
case class TrainingProblem(challenge: String, id: String, size: Int, operators: Seq[String]) {
  def problem: Problem = Problem(id, size, operators, None, None)
  override def toString = s"""TrainingProblem(\"$challenge\", \"$id\", $size, Seq(${operators.map("\"" + _ + "\"").mkString(", ")}))"""
}

/*
  interface Status {
    easyChairId: number;
    contestScore: number;
    lightningScore: number;
    trainingScore: number;
    mismatches: number;
    numRequests: number;
    requestWindow: {
      resetsIn: number;
      amount: number;
      limit: number
    };
    cpuWindow: {
      resetsIn: number;
      amount: number;
      limit: number
    };
    cpuTotalTime:number;
  }
*/
case class Window(resetsIn: Int, amount: Int, limit: Int)
case class Status(
  easyChairId: Int,
  contestScore: Int,
  lightningScore: Int,
  trainingScore: Int,
  mismatches: Int,
  numRequests: Int,
  requestWindow: Window,
  cpuWindow: Window,
  cpuTotalTime: Int)

object JsonProtocol extends DefaultJsonProtocol {
  implicit val problemF = jsonFormat5(Problem)
  implicit val evalRequestF = jsonFormat3(EvalRequest)
  implicit val evalResponseF = jsonFormat3(EvalResponse)
  implicit val guessF = jsonFormat2(Guess)
  implicit val guessResponseF = jsonFormat4(GuessResponse)
  implicit val trainReqF = jsonFormat2(TrainRequest)
  implicit val trainProbF = jsonFormat4(TrainingProblem)
  implicit val windowF = jsonFormat3(Window)
  implicit val statusF = jsonFormat9(Status)
}

package net.virtualvoid.program

/*
  interface Problem {
    id: string;
    size: number;
    operators: string[];
    solved?: boolean;
    timeLeft?: number
  }
*/
case class Problem(id: String, size: Int, operators: Seq[String], solved: Option[Boolean], timeLeft: Option[Int])

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

case class EvalResponse(status: String, outputs: Option[Seq[String]], message: String)

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
case class TrainingProblem(challenge: String, id: String, size: Int, operators: Seq[String])

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
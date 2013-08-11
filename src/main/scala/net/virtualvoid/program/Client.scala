package net.virtualvoid.program

import akka.actor.{ Props, Actor, ActorSystem }
import akka.pattern.ask
import akka.io.IO
import scala.concurrent.duration._
import spray.can.Http
import spray.can.Http.{ HostConnectorInfo, HostConnectorSetup }
import scala.concurrent.{ Future, Await }
import com.typesafe.config.ConfigFactory
import spray.client.pipelining._
import akka.util.Timeout
import spray.httpx.marshalling.Marshaller
import java.lang.Long.parseLong
import java.math.BigInteger
import spray.http.{ HttpResponse, HttpRequest }

object Client {
  implicit val system = ActorSystem()
  import system.dispatcher
  val key = ConfigFactory.load("auth").getString("app.key")
  implicit val timeout: Timeout = 60.seconds

  val hostConnectorInfo = Await.result((IO(Http) ? HostConnectorSetup("icfpc2013.cloudapp.net")).mapTo[HostConnectorInfo], 1.second)

  class RateLimitedSender extends Actor {
    var nextSend: Deadline = Deadline.now
    def receive = {
      case req: HttpRequest ⇒
        if (nextSend.isOverdue()) {
          hostConnectorInfo.hostConnector.tell(req, sender)
          nextSend = Deadline.now + 4.seconds
        } else {
          val deadline = nextSend.timeLeft + 1.second
          println(s"Delaying request for ${deadline.toMillis} ms")
          val s = sender
          context.system.scheduler.scheduleOnce(deadline) {
            self.tell(req, s)
          }
        }
    }
  }
  val rateLimitedNetwork = system.actorOf(Props[RateLimitedSender])

  val network: SendReceive = req ⇒ (rateLimitedNetwork ? req).mapTo[HttpResponse]

  import JsonProtocol._
  import spray.httpx.SprayJsonSupport._
  val trainPipeline = network ~> unmarshal[TrainingProblem]
  def train(request: TrainRequest): Future[TrainingProblem] =
    trainPipeline(req("train", request))

  val evalPipeline = network ~> unmarshal[EvalResponse]
  def eval(id: String, arguments: Seq[Long]): Future[EvalResponse] =
    evalPipeline(req("eval", EvalRequest(Some(id), None, arguments)))

  def fetchExamples(id: String, num: Int = 10): Future[Seq[PositiveExample]] = {
    val es = Synthesis.testValues(num)
    for {
      resp ← evalPipeline(req("eval", EvalRequest(Some(id), None, es)))
    } yield (es, resp.outputs.get).zipped.map((i, o) ⇒ PositiveExample(i, parseLong(o.drop(2))))
  }

  val problemPipeline = network ~> unmarshal[Seq[Problem]]
  def newProblem(): Future[Seq[Problem]] =
    problemPipeline(Post(path("myproblems")))

  val guessPipeline = network ~> unmarshal[GuessResponse]
  def deliverGuess(guess: Guess): Future[GuessResponse] =
    guessPipeline(req("guess", guess))

  def parseLong(str: String): Long = new BigInteger(str, 16).longValue()

  def req[T: Marshaller](kind: String, t: T) =
    Post(path(kind), t)

  def path(kind: String) = s"/${kind}?auth=${key}vpsH1H"

  def shutdown() = system.shutdown()
}

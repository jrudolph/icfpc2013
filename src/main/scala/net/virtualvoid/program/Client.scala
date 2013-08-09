package net.virtualvoid.program

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.io.IO
import scala.concurrent.duration._
import spray.can.Http
import spray.can.Http.{ HostConnectorInfo, HostConnectorSetup }
import scala.concurrent.{ Future, Await }
import com.typesafe.config.ConfigFactory
import spray.client.pipelining._
import akka.util.Timeout

object Client {
  implicit val system = ActorSystem()
  import system.dispatcher
  val key = ConfigFactory.load("auth").getString("app.key")
  implicit val timeout: Timeout = 5.seconds

  val hostConnectorInfo = Await.result((IO(Http) ? HostConnectorSetup("icfpc2013.cloudapp.net")).mapTo[HostConnectorInfo], 1.second)

  import JsonProtocol._
  import spray.httpx.SprayJsonSupport._
  val trainPipeline = sendReceive(hostConnectorInfo.hostConnector) ~> unmarshal[TrainingProblem]

  def train(request: TrainRequest): Future[TrainingProblem] = trainPipeline(Post(s"/train?auth=${key}vpsH1H", request))

  def shutdown() = system.shutdown()
}

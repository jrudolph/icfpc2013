resolvers += "spray nightlies" at "http://nightlies.spray.io/"

libraryDependencies ++= {
  val akkaV = "2.2.0"
  val sprayV = "1.2-20130801"
  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaV,
    "io.spray" % "spray-client" % sprayV,
    "io.spray" % "spray-can" % sprayV,
    "io.spray" %% "spray-json" % "1.2.5",
    "org.parboiled" %% "parboiled-scala" % "1.1.5",
    // RUNTIME
    "com.typesafe.akka" %% "akka-slf4j" % akkaV % "runtime",
    "ch.qos.logback" % "logback-classic" % "1.0.0" % "runtime",
    // TEST
    "org.specs2" %% "specs2" % "1.13" % "test"
  )
}

scalaVersion := "2.10.2"

ScalariformSupport.formatSettings

Revolver.settings

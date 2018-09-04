name := "futures101"

version := "0.1"

scalaVersion := "2.12.6"

organization := "com.showtix"

libraryDependencies ++= {
  val akkaVersion = "2.5.12"
  val akkaHttp = "10.1.1"
  Seq(
    "com.typesafe.akka"       %% "akka-actor"                      % akkaVersion,
    "com.typesafe.akka"       %%  "akka-slf4j"                     % akkaVersion,
    "com.typesafe.akka"       %%  "akka-testkit"                   % akkaVersion   % "test",
    "org.scalatest"           %% "scalatest"                       % "3.0.5"       % "test",
    "joda-time"                % "joda-time"                       % "2.10"

  )
}
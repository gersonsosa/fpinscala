name := "fpinscala"

ThisBuild / scalaVersion := "3.6.3"

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(
    name = Some("Build project"),
    commands = List("test:compile")
  )
)

ThisBuild / scalacOptions ++= List(
  "-feature",
  "-deprecation",
  "-Ykind-projector:underscores",
  "-source:future"
)

ThisBuild / libraryDependencies += "org.scalameta" %% "munit" % "1.0.1" % Test

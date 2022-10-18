lazy val root = project
  .in(file(""))
  .settings(
    name         := "functional-data-modeling",
    version      := "0.1.0",
    scalacOptions ++= Seq(
      "-language:postfixOps"
    ),
    scalaVersion := "3.1.3"
  )

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")

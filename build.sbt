val dottyVersion = "0.21.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,
    scalacOptions in compile ++= Seq(
      "-Yindent-colons"
    ),
    libraryDependencies ++= Seq(
      ("org.typelevel" %% "cats-core" % "2.0.0").withDottyCompat(scalaVersion.value)
    )
  )

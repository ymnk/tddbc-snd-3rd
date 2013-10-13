import sbt._
import Keys._

object BuildSettings {
  val buildName = "tddbc-snd-3rd"
  val buildOrganization = "ymnk"
  val buildVersion = "0.0.1"
  val buildScalaVersion = "2.10.1"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion,
   scalacOptions := Seq("-deprecation",
                        "-encoding",
                        "utf8",
                        "-feature",
                        "-language:reflectiveCalls",
                        "-language:implicitConversions"),
    crossPaths   := false,
    javaOptions ++= Seq (
      "-target", "1.5"
    )
  )
}

object MyBuild extends Build {

  import BuildSettings._

  val scalatest = "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

  private val dependencies = Seq (
    scalatest,
    "org.scala-lang" % "scala-compiler" % buildScalaVersion
  )

  private val test_arguments = 
    Seq(Tests.Argument( TestFrameworks.ScalaTest, "-oD", "-oS"))

  lazy val root =
    Project( buildName, file("."), settings = buildSettings )
      .settings( libraryDependencies ++= dependencies )
      .settings( testOptions in Test ++= test_arguments )
      .settings( parallelExecution in Test := false )
}

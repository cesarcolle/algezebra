import pl.project13.scala.sbt.JmhPlugin


name := "algebra"

version := "0.1"

scalaVersion := "2.12.6"

val algebraVersion = "0.7.0"
val bijectionVersion = "0.9.4"
val javaEwahVersion = "1.1.4"
val paradiseVersion = "2.1.0"
val quasiquotesVersion = "2.1.0"
val scalaTestVersion = "3.0.1"
val scalacheckVersion = "1.13.4"
val utilVersion = "6.20.0"
val utilVersion212 = "6.39.0"


val sharedSettings = Seq(
  organization := "com.github.cesarcolle",
  scalaVersion := "2.11.11"
)

parallelExecution in Test := true

lazy val algezebraCore: Project = project
  .in(file("algezebra-core"))
  .settings(sharedSettings)
  .settings(
    name := "noether-core",
    moduleName := "noether-core",
    description := "Machine Learning Aggregators",
    libraryDependencies ++= Seq(
      "com.twitter" %% "algebird-core" % "0.13.4",
      "org.scalatest" %% "scalatest" % scalaTestVersion,
"com.googlecode.javaewah" % "JavaEWAH" % javaEwahVersion,
"org.scalacheck" %% "scalacheck" % scalacheckVersion
    ),
    fork in Test := true
  )

lazy val algezebraBenchmark = project
  .in(file("algezebra-benchmark"))
  .settings(JmhPlugin.projectSettings: _*)
  .settings(sharedSettings)
  .settings(coverageExcludedPackages := "com\\.github\\.cesarcolle\\.algezebra-benchmark.*")
  .dependsOn(algezebraCore)
  .enablePlugins(JmhPlugin)

val algebra = Project(
  id = "algebra",
  base = file("."))
    .settings(sharedSettings)
  .settings(coverageExcludedPackages := "<empty>;.*\\.benchmark\\..*")
  .aggregate(algezebraCore, algezebraBenchmark)

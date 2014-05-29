name := "selfassembly"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.11.1",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
  "junit" % "junit-dep" % "4.10" % "test",
  "com.novocode" % "junit-interface" % "0.10" % "test"
)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s")

parallelExecution in Global := false

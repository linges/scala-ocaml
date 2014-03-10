name := "scala-ocaml"

version := "0.1"

scalaVersion := "2.10.0"

parallelExecution in Test := false

testOptions in Test += Tests.Argument("-oD")

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "1.9.1",
	"com.googlecode.kiama" %% "kiama" % "1.4.0"
)

name  :="AS Tiles puzzle solver"

organization := "nl.amsscala"

version := "0.0"

scalaVersion :="2.10.4"

// EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource
// EclipseKeys.withSource := true

libraryDependencies ++= {
  	Seq(
  	    "org.scalatest" % "scalatest_2.10" % "2.1.3" % "test",
		"org.scala-lang" % "scala-swing" % "2.10+"
  	)
}
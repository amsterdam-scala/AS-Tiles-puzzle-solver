name := "AS Tiles puzzle solver"

organization := "nl.amsscala"

version := "0.0"

scalaVersion := "2.11.2"

// EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource
// EclipseKeys.withSource := true

// add scala-xml dependency when needed (for Scala 2.11 and newer) in a robust way
// this mechanism supports cross-version publishing
// taken from: http://github.com/scala/scala-module-dependency-sample
libraryDependencies := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    // if scala 2.11+ is used, add dependency on scala-xml module
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      libraryDependencies.value ++ Seq(
        // "org.scala-lang.modules" %% "scala-xml" % "1.0.1",
        // "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
        "org.scala-lang.modules" %% "scala-swing" % "1.0.1")
    case _ =>
      // or just libraryDependencies.value if you don't depend on scala-swing
      libraryDependencies.value :+ "org.scala-lang" % "scala-swing" % scalaVersion.value
  }
}

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

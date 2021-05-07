name := "exadel-scala-course-2021"

version := "0.1"

scalaVersion := "2.13.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.7" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-15" % "3.2.5.0" % "test"

libraryDependencies +=
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.2"

//idePackagePrefix := Some("scala2021.skarasik")

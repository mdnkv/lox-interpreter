ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.1"

lazy val root = (project in file("."))
  .settings(
    name := "lox-interpreter",
    idePackagePrefix := Some("dev.mednikov.loxscala")
  )

organization := "com.inkenkun.x1"
name := "scala-for-ml-exercise"
version := "0.0.1"

scalaVersion := "2.11.12"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",   // source files are in UTF-8
  "-deprecation",         // warn about use of deprecated APIs
  "-unchecked",           // warn about unchecked type parameters
  "-feature",             // warn about misused language features
  "-language:higherKinds",// allow higher kinded types without `import scala.language.higherKinds`
  "-Xlint",               // enable handy linter warnings
  //  "-Xfatal-warnings",     // turn compiler warnings into errors
  "-Ypartial-unification" // allow the compiler to unify type constructors of different arities
)

libraryDependencies ++= Seq(
  "co.theasi"            %% "plotly"        % "0.2.0",
  "com.github.tototoshi" %% "scala-csv"     % "1.3.5",
  "log4j"                 % "log4j"         % "1.2.17",
  "org.apache.commons"    % "commons-math3" % "3.6.1",
  "org.typelevel"        %% "cats-core"     % "0.9.0",
  "org.vegas-viz"        %% "vegas"         % "0.3.11",
  "org.scalatest"        %% "scalatest"     % "3.0.1" % "test"
)

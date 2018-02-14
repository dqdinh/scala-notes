import scalariform.formatter.preferences._
import com.typesafe.sbt.SbtScalariform.ScalariformKeys

// Filter out compiler flags to make the repl experience functional...
lazy val badConsoleFlags = Seq("-Xfatal-warnings", "-Ywarn-unused:imports")

lazy val scala212          = "2.12.4"
lazy val catsVersion       = "1.0.1"
lazy val scalaCheckVersion = "1.13.5"
lazy val specs2Version     = "3.9.5"
lazy val simulacrumVersion = "0.11.0"

lazy val commonSettings = Seq(
  organization := "io.github.dqdinh",
  name := "scala-notes",
  description := "notes about scala",
  version := s"1.0",
  scalaVersion := scala212,
  libraryDependencies ++= Seq(
    "com.github.mpilquist"  %% "simulacrum"           % simulacrumVersion,
    "org.typelevel"         %% "cats-core"            % catsVersion,
    "org.scalacheck"        %% "scalacheck"           % scalaCheckVersion % Test,
    "org.specs2"            %% "specs2-scalacheck"    % specs2Version % Test
  ),
  scalacOptions := Seq(
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-explaintypes",                     // Explain type errors in more detail.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
    "-language:higherKinds",             // Allow higher-kinded types
    "-language:implicitConversions",     // Allow definition of implicit functions called views
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfuture",                          // Turn on future language features.
    "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
    "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
    "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
    "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
    "-Xlint:option-implicit",            // Option.apply used implicit view.
    "-Xlint:package-object-classes",     // Class or object defined in package object.
    "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match",              // Pattern match may not be typesafe.
    "-Yno-adapted-args",                 // Do not adapt an argument to match the receiver.
    "-Ypartial-unification",             // Enable partial unification in type constructor inference
    "-Ywarn-dead-code",                  // Warn when dead code is identified.
    "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
    "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen",              // Warn when numerics are widened.
    "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals",              // Warn if a local definition is unused.
    "-Ywarn-unused:params",              // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates",            // Warn if a private member is unused.
    "-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
  ),
  scalacOptions in (Compile, console) ~= (_.filterNot(badConsoleFlags.contains(_))),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6"),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full),
  ScalariformKeys.preferences := ScalariformKeys.preferences.value
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(AlignParameters, true)
    .setPreference(DanglingCloseParenthesis, Preserve),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  testOptions in Test += Tests.Argument(
    TestFrameworks.ScalaCheck,
    "-minSuccessful", "100",
    "-maxDiscarded", "500",
    "-minSize", "0",
    "-maxSize", "100",
    "-workers", "1",
    "-verbosity", "0"
  ),
  buildInfoOptions += BuildInfoOption.ToMap
)

lazy val clippyBuildSettings = Seq(
  com.softwaremill.clippy.ClippySbtPlugin.clippyColorsEnabled := true
)

lazy val scalanotes = (project in file("."))
  .enablePlugins(BuildInfoPlugin, ScalafmtPlugin)
  .settings(commonSettings)
  .settings(clippyBuildSettings)

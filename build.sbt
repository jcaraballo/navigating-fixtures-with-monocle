import sbt._

name := "navigating-fixtures-with-monocle"

version := "0.1"

scalaVersion := "2.12.1"

resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")


val monocleVersion = "1.4.0-M1"  // or "1.4.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
//  "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion,
//  "com.github.julien-truffaut"  %%  "monocle-state"   % monocleVersion,
//  "com.github.julien-truffaut"  %%  "monocle-refined" % monocleVersion,
//  "com.github.julien-truffaut"  %%  "monocle-unsafe"  % monocleVersion
  "com.github.julien-truffaut"  %%  "monocle-law"     % monocleVersion % "test"
)

// for @Lenses macro support
//addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)


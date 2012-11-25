resolvers ++= Seq(
  "Typesafe Repository"               at "http://repo.typesafe.com/typesafe/releases/",
  "Sonatype Releases"                 at "http://oss.sonatype.org/content/repositories/releases",
  "Sonatype Snapshots"                at "http://oss.sonatype.org/content/repositories/snapshots"
)

crossScalaVersions := Seq("2.9.2")

libraryDependencies ++= Seq(
  "org.scalaz"         %% "scalaz-core"  % "7.0-SNAPSHOT" changing(),
  "org.specs2"         %% "specs2"       % "1.12.2"         % "test" changing(),
  "org.scalacheck"     %% "scalacheck"   % "1.10.0"         % "test"
)

version := "0.1"

organization := "com.github"

scalacOptions ++= Seq("-deprecation", "-unchecked")
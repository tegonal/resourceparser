name := "resourceparser"

organization := "com.tegonal"

version := "0.1.0-SNAPSHOT"

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.3.7" % "test"
)

initialCommands := "import com.tegonal.resourceparser._"

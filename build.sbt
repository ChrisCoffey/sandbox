libraryDependencies ++= Seq(
  "org.scalatest"                % "scalatest_2.11"              % "2.2.1" % "test",
"joda-time" % "joda-time" % "2.6",
"com.propensive" %% "rapture-core" % "1.1.0",
  "org.parboiled" %% "parboiled" % "2.1.0"
)

lazy val root = (project.in(file(".")).aggregate(moscalic))

lazy val moscalic = (project.in(file("./moscalic")))


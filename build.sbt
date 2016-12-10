name := "scalatest"

version := "1.0"
lazy val root = (project in file(".")).
	settings(
		name := "my-project",
		version := "1.0",
		scalaVersion := "2.11.4",
		mainClass in Compile := Some("main.Main")
	)
libraryDependencies  += "org.scala-lang" % "scala-reflect" % "2.11.7"
libraryDependencies ++= Seq(
	"junit" % "junit" % "4.8.1" % "test"
)
//unmanagedBase <<= baseDirectory { base => base / "lib" }
//unmanagedJars in Compile += file("gluegen-rt.jar")
libraryDependencies += "org.jogamp.jogl" % "jogl-all-main" % "2.3.2"
//libraryDependencies += "org.jogamp.joal" % "joal-main" % "2.3.2"
libraryDependencies += "org.jogamp.gluegen" % "gluegen-rt-main" % "2.3.2"
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.1" % "test"

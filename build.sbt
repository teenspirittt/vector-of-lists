ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "vector of lists",
    libraryDependencies += "org.openjfx" % "javafx-controls" % "17.0.1",
    libraryDependencies += "org.openjfx" % "javafx-fxml" % "17.0.1",
    unmanagedResourceDirectories in Compile += baseDirectory.value / "src" / "main" / "scala" / "resources"
  )

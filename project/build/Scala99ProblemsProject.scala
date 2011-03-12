import sbt._

class Scala99Problems(info: ProjectInfo) extends DefaultWebProject(info) with AutoCompilerPlugins with IdeaProject {

  val scalaVer = buildScalaVersion

  val sourceArtifact = Artifact.sources(artifactID)

  override def managedStyle = ManagedStyle.Maven

  // Scala Libraries
  val scalaCompiler = "org.scala-lang" % "scala-compiler" % scalaVer % "test"
  val specs = "org.scala-tools.testing" %% "specs" % "1.6.5"  % "test" withSources()

}

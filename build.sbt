lazy val common = Seq(
    organization := "in.thedatateam",
    version := "0.0.14-SNAPSHOT",
    crossScalaVersions := Seq("2.10.4"),
    scalacOptions ++= Seq("-target:jvm-1.7", "-feature", "-deprecation", "-language:_"),
    resolvers += Resolver.sonatypeRepo("releases"),
    licenses += ("APSL-2.0", url("https://www.apache.org/licenses/LICENSE-2.0"))
  )

lazy val lib = project
  .in(file("lib"))
  .settings(common)
  .settings(
    name := """sbt-swagger-codegen-lib""",
    libraryDependencies ++= Seq(
      "com.eed3si9n" %% "treehugger" % "0.4.1",
      "io.swagger" % "swagger-parser" % "1.0.27"
    )
  )

lazy val plugin = project
  .in(file("plugin"))
  .settings(common)
  .settings(
    name := """sbt-swagger-codegen""",
    sbtPlugin := true
  )
  .dependsOn(lib)

lazy val root = project
  .in(file("."))
  .settings(common)
  .settings(
    publish := {}
  )
  .aggregate(lib, plugin)





addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "0.2.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.3")

addSbtPlugin("com.geirsson" % "sbt-scalafmt" % "0.6.3")

addSbtPlugin("me.lessis" % "bintray-sbt" % "0.3.0")

 resolvers += Resolver.bintrayRepo("th3iedkid","maven")

resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"

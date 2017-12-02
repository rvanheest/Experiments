enablePlugins(ScalaJSPlugin)

name := "Scala.js experiments"
scalaVersion := "2.12.4"

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

libraryDependencies += "org.scala-js" %% "scalajs-library" % "0.6.21"
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.3"
libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.9.2"
libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.7"
libraryDependencies += "com.lihaoyi" %%% "scalarx" % "0.3.2"
libraryDependencies += "io.monix" %%% "monix-reactive" % "3.0.0-8084549"

skip in packageJSDependencies := false
jsDependencies += "org.webjars" % "jquery" % "2.1.4" / "2.1.4/jquery.js"

jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()

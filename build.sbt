name := "orr-reg"

version := "0.1.0"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "com.hp.hpl.jena"       % "jena"                      % "2.6.3",
  "com.github.scopt"     %% "scopt"                     % "3.5.0",
  "org.scalaj"           %% "scalaj-http"               % "2.3.0",
  "org.json4s"           %% "json4s-native"             % "3.3.0",
  "org.json4s"           %% "json4s-ext"                % "3.3.0",
  "joda-time"             % "joda-time"                 % "2.9.7"
)

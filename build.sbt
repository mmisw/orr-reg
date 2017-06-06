name := "orr-reg"

version := "0.1.0"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:_")

libraryDependencies ++= Seq(
  "org.apache.jena"         %   "jena"                      % "3.2.0",
  "org.apache.jena"         %   "jena-tdb"                  % "3.2.0", //(*)
  "net.sourceforge.owlapi"  %   "owlapi-distribution"       % "5.0.5",
  "com.github.scopt"        %%  "scopt"                     % "3.5.0",
  "org.scalaj"              %%  "scalaj-http"               % "2.3.0",
  "org.json4s"              %%  "json4s-native"             % "3.3.0",
  "org.json4s"              %%  "json4s-ext"                % "3.3.0",
  "joda-time"               %   "joda-time"                 % "2.9.7"
)

//(*) https://jena.apache.org/download/maven.html:
//  "...use of <type>pom</type> ... does not work in all tools.
//  An alternative is to depend on jena-tdb, which will pull in the other artifacts."
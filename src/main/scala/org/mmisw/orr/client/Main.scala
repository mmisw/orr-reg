package org.mmisw.orr.client

import java.io.File

import org.joda.time.DateTime
import org.json4s._
import org.json4s.ext.JodaTimeSerializers
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.writePretty

import scala.collection.mutable.{Set ⇒ MutableSet}
import scalaj.http.{Http, HttpOptions, HttpResponse, MultiPart}

case class Opts(
                 url: Option[String] = None,
                 maxDepth: Int = 0,
                 ontLib: String = "jena",
                 newVersion: Boolean = false,
                 action: Option[String] = None,
                 orr: Orr = Orr()
               )

case class Orr(
                endpoint:   Option[String] = None,
                username:   Option[String] = None,
                password:   Option[String] = None,
                orgName:    Option[String] = None,
                visibility: Option[String] = None,
                status:     Option[String] = None
              )


class Main(opts: Opts) {
  private val userName: String = opts.orr.username.get
  private val password: String = opts.orr.password.get

  // ad hoc convenience
  private val excludeUris = List(
    "http://sweet.jpl.nasa.gov/2.3/sweetAll.owl",
    "http://sweet.jpl.nasa.gov/2.3/humanAgriculture.owl"
  )

  private val processedUris: MutableSet[String] = MutableSet()

  private implicit val jsonFormats = DefaultFormats ++ JodaTimeSerializers.all
  private implicit val serFormats = Serialization.formats(NoTypeHints)

  private val action: OntologyAction =
    opts.action match {
      case Some("register")   ⇒ new RegisterAction
      case Some("unregister") ⇒ new UnregisterAction
      case Some("load")       ⇒ LoadOntModelAction
      case Some(a) ⇒ throw new Exception(s"unrecognized action: $a")
      case None    ⇒ throw new Exception(s"unspecified action")
    }

  def run(): Unit = {
    processedUris.clear()
    processOntology(opts.url.get)
    println(s"\nprocessed ontologies: ${processedUris.size}")
    println(s"\nexcluded: $excludeUris\n")
  }

  case class ActionInfo(uri: String,
                        bytes: Array[Byte],
                        file: File
                       )

  abstract class OntologyAction {
    def doIt(info: ActionInfo): Unit
  }

  class RegisterAction extends OntologyAction {
    def doIt(info: ActionInfo): Unit = {
      if (excludeUris.contains(info.uri)) {
        println("-> excluded from registration: " + info.uri)
        return
      }
      val uploadResult = uploadOntology(info)
      println(s"    uploadResult: filename=${uploadResult.filename} format=${uploadResult.format}")

      val registrationResult = register(info.uri, uploadResult)
      println(s"    registrationResult:\n    " +
        writePretty(registrationResult).replaceAll("\n", "\n    "))
    }

    private def uploadOntology(info: ActionInfo): UploadedFileInfo = {
      val route = opts.orr.endpoint.get + "/v0/ont/upload"
      println("    uploading")

      val response: HttpResponse[String] = Http(route)
        .timeout(connTimeoutMs = 5*1000, readTimeoutMs = 60*1000)
        .postMulti(MultiPart("file", "filename", "text/plain", info.bytes))
        .auth(userName, password)
        .asString

      if (response.code != 200) {
        throw new Exception(s"error uploading uri=${info.uri}: response=" + response)
      }

      val json = parse(response.body)
      json.extract[UploadedFileInfo]
    }

    /**
      * @return Some(result) is registration completed.
      *         None if ontology already registered and newVersion option was not indicated.
      */
    private def register(initialUri: String, ufi: UploadedFileInfo): Option[OntologyRegistrationResult] = {
      val (actualUri, ontInfoOpt) = getActualUri(initialUri, ufi)

      val rdfsLabel = "http://www.w3.org/2000/01/rdf-schema#label"
      val name: String = ontInfoOpt match {
        case Some(oi) ⇒ oi.metadata.get(rdfsLabel) match {
          case Some(list) ⇒ list.head
          case None ⇒ "no rdfs:label"
        }
        case None ⇒ "TODO name"
      }

      val params = collection.mutable.MutableList[(String, String)](
        "uri" → actualUri
        , "name" → name
        , "userName" → userName
        , "uploadedFilename" → ufi.filename
        , "uploadedFormat" → ufi.format
      )

      opts.orr.orgName    foreach { s ⇒ params += ("orgName" → s) }
      opts.orr.visibility foreach { s ⇒ params += ("visibility" → s) }
      opts.orr.status     foreach { s ⇒ params += ("status" → s) }

      val route = opts.orr.endpoint.get + "/v0/ont"
      println(s"    registering uri=$actualUri")

      val responseOpt: Option[HttpResponse[String]] = {
        val postResponse: HttpResponse[String] = Http(route)
          .timeout(connTimeoutMs = 5*1000, readTimeoutMs = 60*1000)
          .postForm(params)
          .auth(userName, password)
          .asString

        if (postResponse.code == 201) {
          Some(postResponse)
        }
        else if (postResponse.code == 409) {
          if (opts.newVersion) {
            println(s"    -> Already registered. Registering new version")
            val putResponse: HttpResponse[String] = Http(route)
              .timeout(connTimeoutMs = 5*1000, readTimeoutMs = 60*1000)
              .postForm(params)
              .method("PUT")
              .auth(userName, password)
              .asString

            if (putResponse.code == 200) {
              Some(putResponse)
            }
            else {
              throw new Exception(s"error registering new version of uri=$actualUri: postResponse=" + putResponse)
            }
          }
          else {
            println(s"    -> Already registered.")
            None
          }
        }
        else {
          throw new Exception(s"error registering uri=$actualUri: postResponse=" + postResponse)
        }
      }

      responseOpt map {response ⇒
        val json = parse(response.body)
        json.extract[OntologyRegistrationResult]
      }
    }
  }

  class UnregisterAction extends OntologyAction {
    def doIt(info: ActionInfo): Unit = {
      if (excludeUris.contains(info.uri)) {
        println("-> excluded from unregistration: " + info.uri)
        return
      }
      val response: HttpResponse[String] = {
        val response1: HttpResponse[String] = unregister(info.uri)
        if (response1.code == 404) {
          println("    -> " + response1.statusLine)
          replaceHttpScheme(info.uri) match {
            case Some(uri2) ⇒
              unregister(uri2)

            case None ⇒ response1
          }
        }
        else response1
      }

      if (response.code != 200) {
        println("    -> " + response.statusLine)
      }
    }

    def unregister(uri: String): HttpResponse[String] = {
      val route = opts.orr.endpoint.get + "/v0/ont"
      println(s"    unregistering uri=$uri")
      Http(route)
        .timeout(connTimeoutMs = 5*1000, readTimeoutMs = 60*1000)
        .params(Seq(
          "uri" → uri,
          "userName" → userName
        ))
        .auth(userName, password)
        .method("DELETE")
        .asString
    }
  }

  object LoadOntModelAction extends OntologyAction {
    def doIt(info: ActionInfo): Unit = loadOntModel(info)
  }

  private def loadOntModel(info: ActionInfo): Unit = {
    opts.ontLib match {
      case "jena"   ⇒ ontHelper.JenaOntology(info.uri)
      case "owlapi" ⇒ ontHelper.OwlApiOntology(info.uri)
      case s        ⇒ throw new Exception("unrecognized library: " + s)
    }
  }

  private def processOntology(uri: String, depth: Int = 0): Unit = {
    if (!processedUris.contains(uri)) {
      processedUris.add(uri)
      println("\n[%2d] %s" format (depth, uri))
      val (file, bytes) = download(uri)
      println("     -> " + file)

      action.doIt(ActionInfo(uri, bytes, file))

      if (depth < opts.maxDepth) {
        val model = ontHelper.JenaOntology(uri, Some(file))
        model.importedUris foreach { processOntology(_, depth + 1) }
      }
    }
  }

  private def download(url: String): (File, Array[Byte]) = {
    val response: HttpResponse[String] = Http(url)
      .option(HttpOptions.followRedirects(true))
      .asString
    val file = new File("downloaded", url.replace('/', '|'))
    import java.nio.charset.StandardCharsets
    import java.nio.file.Files
    val bytes = response.body.getBytes(StandardCharsets.UTF_8)
    Files.write(file.toPath, bytes)
    (file, bytes)
  }

  private def getActualUri(initialUri: String, ufi: UploadedFileInfo): (String, Option[PossibleOntologyInfo]) = {
    ufi.possibleOntologyUris.get(initialUri) match {
      case x@Some(_) ⇒ (initialUri, x)
      case None ⇒
        replaceHttpScheme(initialUri) match {
          case Some(uri2) ⇒
            ufi.possibleOntologyUris.get(uri2) match {
              case x@Some(_) ⇒ (uri2, x)
              case None ⇒ (initialUri, None)
            }
          case None ⇒ (initialUri, None)
        }
    }
  }

  // copied from orr-ont's OntService
  /**
    * If uri starts with "http:" or "https:", returns a Some
    * with the same uri but with the scheme replaced for the other.
    * Otherwise, None.
    */
  private def replaceHttpScheme(uri: String): Option[String] = {
    if      (uri.startsWith("http:"))  Some("https:" + uri.substring("http:".length))
    else if (uri.startsWith("https:")) Some("http:" +  uri.substring("https:".length))
    else None
  }

}

case class UploadedFileInfo(userName: String,
                            filename: String,
                            format: String
                            , possibleOntologyUris: Map[String, PossibleOntologyInfo]
                           )

case class PossibleOntologyInfo(explanations: List[String],
                                metadata: Map[String,List[String]])

case class OntologyRegistrationResult(
                                       uri:         String,
                                       version:     Option[String] = None,
                                       visibility:  Option[String] = None,
                                       status:      Option[String] = None,
                                       registered:  Option[DateTime] = None,
                                       updated:     Option[DateTime] = None,
                                       removed:     Option[DateTime] = None)


object Main {
  def main(args: Array[String]): Unit = {

    val parser = new scopt.OptionParser[Opts]("orr-reg") {
      head("orr-reg", "0.0.1")

      opt[String]('o', "ont").required().
        action((x, c) => c.copy(url = Some(x))).
        text(s"Ontology URL")

      opt[Int]('d', "depth").action((x, c) =>
        c.copy(maxDepth = x)).
        text("Max depth processing imported ontologies (0)")

      opt[String]("orr").optional().
        action((x, c) => c.copy(orr = c.orr.copy(endpoint = Some(x)))).
        text("ORR REST endpoint URL")

      opt[String]('u', "username").optional().
        action((x, c) => c.copy(orr = c.orr.copy(username = Some(x)))).
        text("Username")

      opt[String]('p', "password").optional().
        action((x, c) => c.copy(orr = c.orr.copy(password = Some(x)))).
        text("Password")

      opt[String]("action").optional().
        action((x, c) => c.copy(action = Some(x))).
        text("One of register, unregister, load.")

      opt[String]("ont-lib").optional().
        action((x, c) => c.copy(ontLib = x)).
        text("One of jena, owlapi (default: jena")

      opt[String]("org").optional().
        action((x, c) => c.copy(orr = c.orr.copy(orgName = Some(x)))).
        text("Owning organization for registration (default: submitting user)")

      opt[String]('v', "visibility").optional().
        action((x, c) => c.copy(orr = c.orr.copy(visibility = Some(x)))).
        text("public or owner")

      opt[String]('s', "status").optional().
        action((x, c) => c.copy(orr = c.orr.copy(status = Some(x)))).
        text("Status")

      opt[Unit]("new-version").optional().
        action((_, c) => c.copy(newVersion = true)).
        text("Register new version if ontology already exists")

      help("help").text("Print this usage text")
    }

    parser.parse(args, Opts()) foreach { new Main(_).run() }
  }
}

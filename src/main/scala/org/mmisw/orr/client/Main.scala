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
                 ontIri:          Option[String] = None,
                 maxDepth:        Int = 0,
                 irisFilename:    Option[String] = None,
                 locResFrom:      Option[String] = None,
                 locResTo:        Option[String] = None,
                 ontLib:          String = "jena",
                 newVersion:      Boolean = false,
                 action:          Option[String] = None,
                 orr:             Orr = Orr()
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
//    "http://sweet.jpl.nasa.gov/2.3/sweetAll.owl",
//    "http://sweet.jpl.nasa.gov/2.3/humanAgriculture.owl"
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

    (opts.ontIri, opts.irisFilename) match {
      case (Some(ontIri), None)   ⇒ processOntologyIri(ontIri)
      case (None, Some(filename)) ⇒ processOntologyIris(filename)

      case _ ⇒
        throw new Exception(s"one of --ontIri and --ontIris must be provided")
    }

    println(s"\nprocessed ontologies: ${processedUris.size}")
    println(s"\nexcluded: $excludeUris\n")
  }

  case class ActionInfo(uri:   String,
                        bytes: Option[Array[Byte]] = None,
                        file:  Option[File] = None
                       )

  abstract class OntologyAction {
    def requiresDownload: Boolean
    def doIt(info: ActionInfo): Unit
  }

  class RegisterAction extends OntologyAction {
    def requiresDownload: Boolean = true

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
        .postMulti(MultiPart("file", "filename", "text/plain", info.bytes.get))
        .auth(userName, password)
        .asString

      if (response.code != 200) {
        throw new Exception(s"error uploading iri=${info.uri}: response=" + response)
      }

      val json = parse(response.body)
      json.extract[UploadedFileInfo]
    }

    /**
      * @return Some(result) if registration completed.
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
        "iri" → actualUri
        , "name" → name
        , "userName" → userName
        , "uploadedFilename" → ufi.filename
        , "uploadedFormat" → ufi.format
      )

      opts.orr.orgName    foreach { s ⇒ params += ("orgName" → s) }
      opts.orr.visibility foreach { s ⇒ params += ("visibility" → s) }
      opts.orr.status     foreach { s ⇒ params += ("status" → s) }

      val route = opts.orr.endpoint.get + "/v0/ont"
      println(s"    registering iri=$actualUri")

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
              throw new Exception(s"error registering new version of iri=$actualUri: postResponse=" + putResponse)
            }
          }
          else {
            println(s"    -> Already registered.")
            None
          }
        }
        else {
          throw new Exception(s"error registering iri=$actualUri: postResponse=" + postResponse)
        }
      }

      responseOpt map {response ⇒
        val json = parse(response.body)
        json.extract[OntologyRegistrationResult]
      }
    }
  }

  class UnregisterAction extends OntologyAction {
    def requiresDownload: Boolean = false

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
      println(s"    unregistering iri=$uri")
      Http(route)
        .timeout(connTimeoutMs = 5*1000, readTimeoutMs = 60*1000)
        .params(Seq(
          "iri" → uri,
          "userName" → userName
        ))
        .auth(userName, password)
        .method("DELETE")
        .asString
    }
  }

  object LoadOntModelAction extends OntologyAction {
    def requiresDownload: Boolean = true
    def doIt(info: ActionInfo): Unit = loadOntModel(info)
  }

  private def loadOntModel(info: ActionInfo): Unit = {
    opts.ontLib match {
      case "jena"   ⇒ ontHelper.JenaOntology(info.uri)
      case "owlapi" ⇒ ontHelper.OwlApiOntology(info.uri)
      case s        ⇒ throw new Exception("unrecognized library: " + s)
    }
  }

  private def createActionInfo(iri: String, action: OntologyAction): ActionInfo = {
    if (action.requiresDownload) {
      val (file, bytes) = download(iri)
      println("     -> " + file)
      ActionInfo(iri, Some(bytes), Some(file))
    }
    else ActionInfo(iri)
  }

  private def processOntologyIri(iri: String, depth: Int = 0): Unit = {
    if (!processedUris.contains(iri)) {
      processedUris.add(iri)
      println("\n[%2d] %s" format (depth, iri))

      val actionInfo = createActionInfo(iri, action)
      action.doIt(actionInfo)

      if (depth < opts.maxDepth) {
        val model = ontHelper.JenaOntology(iri, Some(actionInfo.file.get))
        model.importedUris foreach { processOntologyIri(_, depth + 1) }
      }
    }
  }

  private def processOntologyIris(irisFilename: String): Unit = {
    println(s"processing IRIs from $irisFilename:")
    io.Source.fromFile(irisFilename).getLines()
      .map(_.trim).filterNot(_.isEmpty).filterNot(_.startsWith("#")).map(_.replaceAll("""^"|"$""", "")
    ) foreach { iri ⇒
      if (!processedUris.contains(iri)) {
        processedUris.add(iri)
        println(s"   iri: $iri")

        val actionInfo = createActionInfo(iri, action)
        action.doIt(actionInfo)
      }
    }
  }

  private def download(iri: String): (File, Array[Byte]) = {
    val url = opts.locResFrom match {
      case None ⇒ iri
      case Some(from) ⇒
        val re = from.r
        val to = opts.locResTo.get
        iri match {
          case re(name) ⇒
            to.replaceAllLiterally("$1", name)
          case _ ⇒ iri
        }
    }

    val response: HttpResponse[String] = Http(url)
      .option(HttpOptions.followRedirects(true))
      .asString

    if (response.code != 200)
      throw new Exception(s"failed to retrieve url=$url ⇒ ${response.code}: ${response.body}")

    val dir = new File("downloaded")
    dir.mkdir()
    val file = new File(dir, url.replace('/', '|'))
    import java.nio.charset.StandardCharsets
    import java.nio.file.Files
    val bytes = response.body.getBytes(StandardCharsets.UTF_8)
    Files.write(file.toPath, bytes)
    //println("download:  wrote " + bytes.length + " to " + file)
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
    * If iri starts with "http:" or "https:", returns a Some
    * with the same iri but with the scheme replaced for the other.
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

      opt[String]('o', "ontIri").optional().
        action((x, c) ⇒ c.copy(ontIri = Some(x))).
        text(s"Ontology IRI")

      opt[Int]('d', "depth").action((x, c) ⇒
        c.copy(maxDepth = x)).
        text("In combination with --ontIri, max depth for processing imported ontologies (0)")

      opt[String]('f', "ontIris").optional().
        action((x, c) ⇒ c.copy(irisFilename = Some(x))).
        text(s"File with explicit list of ontology IRIs")

      opt[String]("locResFrom").optional().
        action((x, c) ⇒ c.copy(locResFrom = Some(x))).
        text("'From' pattern for location resolution. Eg: http://sweetontology.net/(.*)")

      opt[String]("locResTo").optional().
        action((x, c) ⇒ c.copy(locResTo = Some(x))).
        text("'To' replacement template for location resolution. Eg: https://raw.githubusercontent.com/ESIPFed/sweet/master/$1.ttl")

      opt[String]("orr").optional().
        action((x, c) ⇒ c.copy(orr = c.orr.copy(endpoint = Some(x)))).
        text("ORR REST endpoint URL")

      opt[String]('u', "username").optional().
        action((x, c) ⇒ c.copy(orr = c.orr.copy(username = Some(x)))).
        text("Username")

      opt[String]('p', "password").optional().
        action((x, c) ⇒ c.copy(orr = c.orr.copy(password = Some(x)))).
        text("Password")

      opt[String]("action").optional().
        action((x, c) ⇒ c.copy(action = Some(x))).
        text("One of register, unregister, load.")

      opt[String]("ont-lib").optional().
        action((x, c) ⇒ c.copy(ontLib = x)).
        text("One of jena, owlapi (default: jena)")

      opt[String]("org").optional().
        action((x, c) ⇒ c.copy(orr = c.orr.copy(orgName = Some(x)))).
        text("Owning organization for registration (default: submitting user)")

      opt[String]('v', "visibility").optional().
        action((x, c) ⇒ c.copy(orr = c.orr.copy(visibility = Some(x)))).
        text("public or owner")

      opt[String]('s', "status").optional().
        action((x, c) ⇒ c.copy(orr = c.orr.copy(status = Some(x)))).
        text("Status")

      opt[Unit]("new-version").optional().
        action((_, c) ⇒ c.copy(newVersion = true)).
        text("Register new version if ontology already exists")

      help("help").text("Print this usage text")
    }

    parser.parse(args, Opts()) foreach { new Main(_).run() }
  }
}

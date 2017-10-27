/*
    This Scala script is to be run with [Ammonite](http://ammonite.io/#ScalaScripts).

    Adjust the initial section of this script below (TTL directory, and desired actions)
    and then, with Ammonite's `amm` executable installed on your system, simply run:

        $ amm conversion.sc
 */
import java.io.File
import java.nio.file.{Files, StandardCopyOption}

// indicate the directory containing the turtle files
val ttlDir = new File("/Users/carueda/github/ESIPFed/sweet")

if (true) {
  // Process all files in the given directory.

  ttlDir.list().filter(_.endsWith(".ttl")) foreach { filename ⇒
    val sweet = filename.substring(0, filename.length - ".ttl".length)
    val processor = new Processor(sweet)

    //processor.convertSweet(useJena = true, useOwlApi = false)

    // or:
    processor.replaceSweet(useJena = false)
  }
}
else {
  // actions on a specific SWEET.

  // create processor for the particular SWEET:
  val processor = new Processor("reprSciUnits")

  // to generate the outputs from the two libraries for a specific SWEET ontology:
  processor.convertSweet(useJena = true, useOwlApi = true)

  // to process and replace a SWEET ontology:
  //processor.replaceSweet("reprSciUnits", useJena = true)
}


///////////// supporting code follows /////////////////

import $ivy.`org.slf4j:slf4j-nop:1.7.25`

// Jena:
import $ivy.`org.apache.jena:jena:3.2.0`
import $ivy.`org.apache.jena:jena-tdb:3.2.0`

// OWL-API:
import $ivy.`net.sourceforge.owlapi:owlapi-distribution:5.1.2`


object helper {
  // just a common interface for the two libraries
  trait Helper {
    trait HOntology {
      val nsPrefixMapOpt: Option[Map[String,String]]
      val underlying: AnyRef
    }
    def loadOntModel(file: File, iri: String): HOntology
    def saveOntModel(base: String, hOnt: HOntology, toFile: File,
                     nsPrefixMapOpt: Option[Map[String,String]] = None): Unit
  }

  import collection.JavaConverters._

  object jena extends Helper {
    import org.apache.jena.ontology.OntModel
    import java.io.FileOutputStream
    import org.apache.jena.ontology.{OntDocumentManager, OntModel, OntModelSpec}
    import org.apache.jena.rdf.model.ModelFactory

    def loadOntModel(file: File, iri: String): HOntology = {
      val spec = new OntModelSpec(OntModelSpec.OWL_MEM)
      spec.setDocumentManager(new OntDocumentManager)
      val model: OntModel = ModelFactory.createOntologyModel(spec, null)
      model.setDynamicImports(false)
      model.getDocumentManager.setProcessImports(false)
      val lang = if (file.getName.endsWith(".ttl")) "TTL" else "RDF/XML"
      val is = new java.io.FileInputStream(file)
      try model.read(is, iri, lang)
      finally is.close()
      model.getNsPrefixMap.asScala.toMap
      val hOnt = new HOntology {
        val nsPrefixMapOpt = Some(model.getNsPrefixMap.asScala.toMap)
        val underlying = model
      }
      hOnt
    }

    def saveOntModel(base: String, hOnt: HOntology, toFile: File,
                     nsPrefixMapOpt: Option[Map[String,String]] = None): Unit = {
      val model = hOnt.underlying.asInstanceOf[OntModel]
      val writer = model.getWriter("TTL")
      nsPrefixMapOpt orElse hOnt.nsPrefixMapOpt foreach { nsPrefixMap ⇒
        nsPrefixMap foreach { case (prefixName, prefix) ⇒
          logln(s"Jena: setPrefix: $prefixName -> $prefix")
          model.setNsPrefix(prefixName, prefix)
        }
      }
      val os = new FileOutputStream(toFile)
      try writer.write(model, os, base)
      finally os.close()
    }

    def areIsomorphic(hOnt1: HOntology, hOnt2: HOntology): Boolean = {
      val jont1 = hOnt1.underlying.asInstanceOf[OntModel]
      val jont2 = hOnt2.underlying.asInstanceOf[OntModel]
      jont2.isIsomorphicWith(jont2)
    }
  }

  object owlApi extends Helper {
    import org.semanticweb.owlapi.formats.TurtleDocumentFormat
    import org.semanticweb.owlapi.model._
    import org.semanticweb.owlapi.apibinding.OWLManager
    import org.semanticweb.owlapi.io.FileDocumentSource

    private val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager

    def loadOntModel(file: File, iri: String): HOntology = {
      manager.getIRIMappers.clear() // https://sourceforge.net/p/owlapi/mailman/message/23787202/
      val configurator = new  OntologyConfigurator()
        .withUseNamespaceEntities(true)
        .setLoadAnnotationAxioms(false)
        .setStrict(false)
        .setMissingImportHandlingStrategy(MissingImportHandlingStrategy.SILENT)
        .setRetriesToAttempt(0)

      val conf = configurator.buildLoaderConfiguration()

      val docSource = new FileDocumentSource(file)
      val ont: OWLOntology = manager.loadOntologyFromOntologyDocument(docSource, conf)
      val hOnt = new HOntology {
        val nsPrefixMapOpt = None
        val underlying = ont
      }
      hOnt
    }

    def saveOntModel(base: String, hOnt: HOntology, toFile: File,
                     nsPrefixMapOpt: Option[Map[String,String]] = None): Unit = {
      val model = hOnt.underlying.asInstanceOf[OWLOntology]
      import java.nio.file.Files

      val turtleFormat = new TurtleDocumentFormat()
      turtleFormat.setDefaultPrefix(base + "/")

      nsPrefixMapOpt orElse hOnt.nsPrefixMapOpt foreach { nsPrefixMap ⇒
        nsPrefixMap foreach { case (prefixName, prefix) ⇒
          logln(s"OWLAPI: setPrefix: $prefixName -> $prefix")
          turtleFormat.setPrefix(prefixName, prefix)
        }
      }

      val out = Files.newOutputStream(toFile.toPath)
      try manager.saveOntology(model, turtleFormat, out)
      finally out.close()
    }
  }
}

class Processor(sweet: String) {
  val iri = s"http://sweetontology.net/$sweet"
  val filename = s"$sweet.ttl"

  val inFile = new File(ttlDir, filename)

  import helper._

  // we get the prefixes using Jena in all cases:
  val nsPrefixMapOpt = jena.loadOntModel(inFile, iri).nsPrefixMapOpt

  // converts with output in new file(s)
  def convertSweet(useJena: Boolean, useOwlApi: Boolean): Unit = {
    println(s"\n$sweet")
    if (useJena) {
      println("    using Jena:")
      processFile(jena, "jena")
    }
    if (useOwlApi) {
      println("    using OWL-API:")
      processFile(owlApi, "owlapi")
    }
  }

  // converts and replaces input file
  def replaceSweet(useJena: Boolean): Unit = {
    println(s"\n$sweet:")
    if (useJena) {
      println("    using Jena:")
      processFile(jena, "jena", replace = true)
    }
    else {
      println("    using OWL-API:")
      processFile(owlApi, "owlapi", replace = true)
    }
  }


  def processFile(helper: Helper, suffix: String, replace: Boolean = false): Unit = {
    val outFile = if (replace) {
      val f = File.createTempFile(filename, "__tmp.ttl")
      f.deleteOnExit()
      f
    }
    else new File(s"$filename.$suffix.ttl")

    // prettify is simply load and save the model
    def prettify(): Unit = {
      logln(s"- loading $inFile")
      val ont = helper.loadOntModel(inFile, iri)
      logln(s"- saving $outFile")
      helper.saveOntModel(iri, ont, outFile, ont.nsPrefixMapOpt orElse nsPrefixMapOpt)
    }

    def verifyIsomorphism(): Boolean = {
      log(s"- verifying isomorphism ")
      val ont1 = jena.loadOntModel(inFile, iri)
      val ont2 = jena.loadOntModel(outFile, iri)
      val isomorphic = jena.areIsomorphic(ont1, ont2)
      println(" " + (if (isomorphic) "√" else "x"))
      isomorphic
    }

    prettify()
    val isomorphic = verifyIsomorphism()
    if (replace && isomorphic) {
      Files.copy(outFile.toPath, inFile.toPath, StandardCopyOption.REPLACE_EXISTING)
      logln(s"- $inFile overwritten")
    }
  }
}

def logln(s: String): Unit = println(s"\t$s")

def log(s: String): Unit = {
  print(s"\t$s")
  System.out.flush()
}

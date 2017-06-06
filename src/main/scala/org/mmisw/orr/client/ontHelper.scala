package org.mmisw.orr.client

import java.io.File

import org.apache.jena.ontology.OntModel
import org.mmisw.orr.client.ont.{jenaHelper, owlApiHelper}
import org.semanticweb.owlapi.model.OWLOntology

object ontHelper {

  trait Ontology {
    def importedUris: Set[String]
  }

  case class JenaOntology(uri: String, fileOpt: Option[File] = None) extends Ontology {
    import scala.collection.JavaConversions._

    private val ont: OntModel = jenaHelper.loadOntModel(uri, fileOpt)

    def importedUris: Set[String] = {
      val uris = ont.listImportedOntologyURIs(false)
      uris.toList.toSet
    }
  }

  case class OwlApiOntology(uri: String, fileOpt: Option[File] = None) extends Ontology {
    private val ont: OWLOntology = owlApiHelper.loadOntModel(fileOpt.get)

    def importedUris: Set[String] = {
      import scala.collection.JavaConverters._
      val d = ont.importsDeclarations()
      (d.iterator().asScala map (_.getIRI.getIRIString)).toSet
    }
  }
}

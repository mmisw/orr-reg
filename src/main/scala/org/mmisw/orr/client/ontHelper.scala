package org.mmisw.orr.client

import java.io.File

import org.apache.jena.ontology.OntModel
import org.mmisw.orr.client.ont.{jenaHelper, owlApiHelper}
import org.semanticweb.owlapi.model.OWLOntology

object ontHelper {

  trait Ontology {
    def importedIris: Set[String]
  }

  case class JenaOntology(iri: String, fileOpt: Option[File] = None) extends Ontology {
    import scala.collection.JavaConversions._

    private val ont: OntModel = jenaHelper.loadOntModel(iri, fileOpt)

    def importedIris: Set[String] = {
      val iris = ont.listImportedOntologyURIs(false)
      iris.toList.toSet
    }
  }

  case class OwlApiOntology(iri: String, fileOpt: Option[File] = None) extends Ontology {
    private val ont: OWLOntology = owlApiHelper.loadOntModel(fileOpt.get)

    def importedIris: Set[String] = {
      import scala.collection.JavaConverters._
      val d = ont.importsDeclarations()
      (d.iterator().asScala map (_.getIRI.getIRIString)).toSet
    }
  }
}

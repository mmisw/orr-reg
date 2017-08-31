package org.mmisw.orr.client.ont

import java.io.{File, FileInputStream}

import org.apache.jena.ontology.{OntDocumentManager, OntModel, OntModelSpec}
import org.apache.jena.rdf.model.ModelFactory

object jenaHelper {

  def loadOntModel(iri: String, fileOpt: Option[File] = None): OntModel = {
    //println(s"loadOntModel: iri=$iri fileOpt=$fileOpt")
    val spec = new OntModelSpec(OntModelSpec.OWL_MEM)
    spec.setDocumentManager(new OntDocumentManager)
    val model = ModelFactory.createOntologyModel(spec, null)
    model.setDynamicImports(false)
    model.getDocumentManager.setProcessImports(false)
    fileOpt match {
      case None       ⇒ model.read(iri);

      case Some(file) ⇒
      val lang = if (file.getName.endsWith(".ttl")) "TTL" else "RDF/XML"
        val is = new FileInputStream(file)
        try model.read(is, iri, lang)
        finally is.close()
    }
    model
  }
}

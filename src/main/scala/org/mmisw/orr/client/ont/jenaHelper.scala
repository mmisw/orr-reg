package org.mmisw.orr.client.ont

import java.io.{File, FileInputStream}

import org.apache.jena.ontology.{OntDocumentManager, OntModel, OntModelSpec}
import org.apache.jena.rdf.model.ModelFactory

object jenaHelper {

  def loadOntModel(uri: String, fileOpt: Option[File] = None): OntModel = {
    val spec = new OntModelSpec(OntModelSpec.OWL_MEM)
    spec.setDocumentManager(new OntDocumentManager)
    val model = ModelFactory.createOntologyModel(spec, null)
    model.setDynamicImports(false)
    model.getDocumentManager.setProcessImports(false)
    fileOpt match {
      case None       ⇒ model.read(uri);
      case Some(file) ⇒
        val is = new FileInputStream(file)
        try model.read(is, uri)
        finally is.close()
    }
    model
  }
}

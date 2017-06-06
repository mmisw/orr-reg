package org.mmisw.orr.client.ont

import java.io.File

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io.FileDocumentSource
import org.semanticweb.owlapi.model._


object owlApiHelper {
  def loadOntModel(file: File): OWLOntology = {
    println("owlApiHelper.loadOntModel: loading file=" + file)

    val m: OWLOntologyManager = OWLManager.createOWLOntologyManager
    m.getIRIMappers.clear() // https://sourceforge.net/p/owlapi/mailman/message/23787202/
    val conf = new OWLOntologyLoaderConfiguration
    conf.setMissingImportHandlingStrategy(MissingImportHandlingStrategy.SILENT)
    val docSource = new FileDocumentSource(file)
    m.loadOntologyFromOntologyDocument(docSource, conf)
  }
}

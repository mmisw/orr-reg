package org.mmisw.orr.client

import java.io.File

import scala.util.control.NonFatal


object validate {

  def main(args: Array[String]): Unit = {
    val file = new File(args(0))
    println("\n\n----USING Jena:")
    try {
      val ont = ontHelper.JenaOntology("", Some(file))
      println(s"ont.importedIris: ${ont.importedIris.size}")
    }
    catch {
      case NonFatal(e) ⇒ e.printStackTrace()
    }

    println("\n\n----USING OwlApi:")
    try {
      val ont = ontHelper.OwlApiOntology("", Some(file))
      println(s"ont.importedIris: ${ont.importedIris.size}")
    }
    catch {
      case NonFatal(e) ⇒ e.printStackTrace()
    }
  }
}

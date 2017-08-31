2017-08-30 0.2.0

- registration of the new SWEET ontologies (v3):

        > runMain org.mmisw.orr.client.Main --orr http://cor.esipfed.org/ont/api -u carueda -p ???? --ontIri http://sweetontology.net/sweetAll --locResFrom http://sweetontology.net/(.*) --locResTo https://raw.githubusercontent.com/ESIPFed/sweet/master/$1.ttl --action register --depth 100 --org esip --visibility public
    
- new options `--locResFrom from --locResTo to` for location resolution
- new option `--ontIris filename` that allows to explicit list the IRIs to be processed.
  This option just used for the unregistration of all the SWEET v3 ontologies at the COR:
  
        > runMain org.mmisw.orr.client.Main --orr http://cor.esipfed.org/ont/api -u carueda -p ???? --ontIris sweets.txt --action unregister
  
  
2017-08-27

- initial commit 
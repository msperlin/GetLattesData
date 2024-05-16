get_acad_back <- function(my_xml) {

  grad <- fetch_df(my_xml, ".//FORMACAO-ACADEMICA-TITULACAO//GRADUACAO")
  mestrado <-  fetch_df(my_xml, ".//FORMACAO-ACADEMICA-TITULACAO//MESTRADO")
  doutorado <-  fetch_df(my_xml, ".//FORMACAO-ACADEMICA-TITULACAO//DOUTORADO")
  pos_doc <- fetch_df(my_xml, ".//FORMACAO-ACADEMICA-TITULACAO//POS-DOUTORADO")

  l_out <- list(
    grad = grad,
    mestrado = mestrado,
    doutorado = doutorado,
    pos_doc = pos_doc
  )

  return(l_out)
}

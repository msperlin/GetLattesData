get_accepted_papers <- function(my_xml) {

  accepted_papers <- parse_papers(my_xml, parent_tag = "ARTIGO-ACEITO-PARA-PUBLICACAO")

  return(accepted_papers)
}

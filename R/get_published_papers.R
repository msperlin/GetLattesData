get_published_papers <- function(my_xml) {

  papers <- parse_papers(my_xml, parent_tag = "ARTIGO-PUBLICADO")

  return(papers)
}
